(in-package :hob)

(defun as-var (word)
  (let ((name (h-word-name word)))
    ;; Obscure SBCL extension
    (when (equal name "/") (setf name "xxx/"))
    (if (equal name "_")
        (gensym)
        (intern name :hob.gen))))

(defun hcompile (expr)
  (match expr
    (:lit (h-lit-val expr))
    (:word (lookup-word expr :value :value))
    ((:seq exprs) (hcompile-seq exprs))
    (("#fn" args body)
     (let ((arg-syms (lmapseq (arg args)
                       (if (equal (h-word-name arg) "_")
                           (gensym)
                           (bind-word arg :value :value (as-var arg))))))
       `(lambda ,arg-syms ,(hcompile body))))
    (("#match" val cases)
     (let ((saw-t nil))
       `(cond ,@(lmapseq (("->" lit body) cases)
                   (if (and (h-word-p lit) (equal (h-word-name lit) "_"))
                       (progn (setf saw-t t)
                              `(t ,(hcompile body)))
                       `((equal ,(hcompile lit) ,(hcompile val)) ,(hcompile body))))
              ,@(unless saw-t `((t (error "fell through")))))))
    (("#fld" val i) `(svref ,(hcompile val) ,(h-lit-val i)))
    (("#assert" a b) `(unless (equal ,(hcompile a) ,(hcompile b)) (error "matching failed")))
    ((fn . args) `(funcall ,(hcompile fn)
                           ,@(loop :for arg :in args :collect (hcompile arg))))))

(defun hcompile-seq (exprs)
  (let (vars)
    (dolist (expr exprs)
      (match expr
        (("#def" pat val) ;; FIXME assumption that pat is a word
         (push (bind-word pat :value :value (as-var pat)) vars))
        (("#data" :_ variants)
         (doseq (variant variants)
           (let ((name (match variant (:word variant) ((name . :_) name))))
             (push (bind-word name :value :value (as-var name)) vars))))
        (t)))
    `(let (,@vars)
       ,@(loop :for expr :in exprs :collect
           (match expr
             (("#def" pat val) `(setf ,(as-var pat) ,(hcompile val)))
             (("#data" :_ variants)
              `(progn
                ,@(loop :for variant :in (seq-list variants) :for i :from 1 :collect
                     (match variant
                       (:word `(setf ,(as-var variant) (vector ,i)))
                       ((name . args) (let ((syms (loop :repeat (length args) :collect (gensym))))
                                        `(setf ,(as-var name) (lambda ,syms (vector ,i ,@syms)))))))))
             (t (hcompile expr)))))))

(defun compile-s-expr (s-expr)
  (handler-bind ((warning #'muffle-warning))
    (cl:compile nil `(lambda ()
                       #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                       ,s-expr))))
