(in-package :hob)

(defun as-var (word)
  (let ((name (h-word-name word)))
    ;; Obscure SBCL extension
    (when (equal name "/") (setf name "xxx/"))
    (if (equal name "_")
        (gensym)
        (intern name :hob.gen))))

(defun binding-var (word)
  (if (equal (h-word-name word) "_") (gensym) (bind-word word :value :value (as-var word))))

(defun hcompile (expr)
  (match expr
    (:lit (h-lit-val expr))
    (:word
     (let ((insts (expr-ann expr :cls-instances))
           (val (or (lookup-word expr :value :value)
                    (error "undefined variable ~a at compile time" expr))))
       (if insts
           `(funcall ,val ,@(loop :for inst :in insts :collect (cls-inst-fields inst)))
           val)))
    ((:seq exprs) (and exprs (hcompile-seq exprs)))
    (("#fn" req-args opt-args rest-arg body)
     (let (args rest-var)
       (doseq (req req-args)
         (push (binding-var req) args))
       (when (h-seq-vals opt-args) (push '&optional args))
       (doseq (("=" opt def) opt-args)
         (push (list (binding-var opt) (hcompile def)) args))
       (unless (h-seq-p rest-arg)
         (push '&rest args)
         (push (setf rest-var (binding-var rest-arg)) args))
       `(lambda ,(nreverse args)
          ,@(when rest-var `((let ((a (nreverse ,rest-var))) (setf ,rest-var (vector 2)) (dolist (a a) (setf ,rest-var (vector 1 a ,rest-var))))))
          ,(hcompile body))))
    (("#match" val cases)
     `(cond ,@(lmapseq (("->" lit body) cases)
                (if (and (h-word-p lit) (equal (h-word-name lit) "_"))
                    `(t ,(hcompile body))
                    `((equal ,(hcompile lit) ,(hcompile val)) ,(hcompile body))))))
    (("#fld" val i) `(svref ,(hcompile val) ,(h-lit-val i)))
    (("#len" val) `(length ,(hcompile val)))
    (("#assign" place val) `(setf ,(hcompile place) ,(hcompile val)))
    (("#assert" a b) `(unless (equal ,(hcompile a) ,(hcompile b)) (error "matching failed")))
    (("#assert" :nil) '(error "matching failed"))
    (((:or "#data" "#def" "#var" "#class" "#instance") . :_) (hcompile-seq (list expr)))
    ((fn . args) `(funcall ,(hcompile fn)
                           ,@(loop :for arg :in args :collect (hcompile arg))))))

(defun hcompile-seq (exprs)
  (let (vars top body)
    (dolist (expr exprs)
      (match expr
        (((:or "#def" "#var") name :_)
         (push (bind-word name :value :value (as-var name)) vars))
        (("#data" :_ variants)
         (loop :for variant :in (seq-list variants) :for i :from 1 :do
            (multiple-value-bind (name ctor)
                (match variant
                  (:word (values variant `(vector ,i)))
                  ((name . args)
                   (let ((syms (loop :repeat (length args) :collect (gensym))))
                     (values name `(lambda ,syms (vector ,i ,@syms))))))
              (push `(setf ,(as-var name) ,ctor) top)
              (push (bind-word name :value :value (as-var name)) vars))))
        (("#class" :_ :_ body)
         (let ((i 0))
           (doseq (("type" name :_) body)
             (let ((sym (as-var name)))
               (push (bind-word name :value :value sym) vars)
               (push `(setf ,sym (lambda (dict) (lambda (&rest args) (apply (elt dict ,i) args)))) top))
             (incf i))))
        (t)))
    (dolist (expr exprs)
      (match expr
        (((:or "#def" "#var") name val)
         (let ((set `(setf ,(as-var name) ,(hcompile val))))
           (if (match val (("#fn" . :_) t) (t nil)) (push set top) (push set body))))
        (((:or "#data" "#class") . :_))
        (("#instance" :_ name :_ body)
         (let* ((inst (expr-ann name :cls-instance))
                (cls (cls-inst-cls inst))
                (vec (cls-inst-fields inst)))
           (push `(progn ,@(lmapseq (("def" name value) body)
                             (let ((pos (position (h-word-name name) (cls-fields cls) :test #'string= :key #'car)))
                               `(setf (elt ,vec ,pos) ,(hcompile value))))) top)))
        (t (push (hcompile expr) body))))
    `(let (,@vars)
       ,@(nreverse top)
       ,@(nreverse body))))
