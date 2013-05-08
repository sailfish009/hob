(in-package :hob)

(define-condition hob-type-error (hob-program-error) ())

(define-raise-function hob-type-error)

(variant type*
  (prim name sym)
  (data name args forms)
  (tparam val))

(variant type-expr
  (fun args result)
  (tvar cx instances ref)
  (inst type args)
  (tclose cx type))

(defstruct (tform (:constructor tform (type args templ))) type args templ)

(defun parse-type (expr)
  (match expr
    (:word
     (let ((found (or (lookup-word expr :type :type)
                      (hob-type-error expr "undefined type `~a`" expr))))
       (evcase found
         (prim (inst found ()))
         ((data args)
          (when args (hob-type-error expr "type `~a` takes type parameters" expr))
          (inst found ()))
         ((tparam val) val))))
    (("->" args ret)
     (fun (mapseq (a args) (parse-type a)) (parse-type ret)))
    (((:as :word head) . targs)
     (let ((found (or (lookup-word head :type :type)
                      (hob-type-error head "undefined type `~a`" expr))))
       (evcase found
         ((data args)
          (unless (= (length targs) (length args))
            (hob-type-error expr "incorrect number of parameters for type `~a`" head))
          (inst found (loop :for targ :in targs :for arg :in args :collect
                         (unify expr (parse-type arg) (parse-type targ)))))
         (t (hob-type-error expr "type `~a` does not take type parameters" head)))))))

(defvar *type-cx* nil)

(defun mkvar () (tvar *type-cx* nil nil))

(defstruct (instance (:constructor instance (cx))) cx subst)

(defun instantiate (type inst)
  (labels ((ins (tp)
             (setf tp (resolve tp))
             (vcase tp
               ((tvar cx)
                (if (member (instance-cx inst) cx)
                    (let ((found (assoc tp (instance-subst inst))))
                      (if found
                          (cdr found)
                          (let ((v (mkvar)))
                            (push (cons inst v) (tvar-instances tp))
                            (push (cons tp v) (instance-subst inst))
                            v)))
                    tp))
               ((inst type args) (inst type (mapcar #'ins args)))
               ((fun args result) (fun (mapcar #'ins args) (ins result))))))
    (ins type)))

(defun typecheck-seq (exprs)
  (let* ((cx (gensym))
         (*type-cx* (cons cx *type-cx*)))
    (dolist (expr exprs)
      (match expr
        (("#data" name variants)
         (multiple-value-bind (name args) (match name (:word name) ((name . args) (values name args)))
           (bind-word name :type :type (data (h-word-name name) args nil))))
        (("#def" name :_)
         (bind-word name :value :type (tclose cx (mkvar))))
        (t)))
    (dolist (expr exprs)
      (match expr
        (("#data" name variants)
         (let ((name (match name (:word name) ((name . :_) name))))
           (let* ((tp (lookup-word name :type :type))
                  (argvars (loop :for arg :in (slot-value tp 'args) :collect
                              (let ((v (mkvar))) (bind-word arg :type :type (tparam v)) v)))
                  (tpinst (inst tp argvars)))
             (doseq (variant variants)
               (multiple-value-bind (name fields)
                   (match variant
                     (:word variant)
                     ((name . fields) (values name (mapcar #'parse-type fields))))
                 (let ((vtype (if fields (fun fields tpinst) tpinst)))
                   (when argvars (setf vtype (tclose cx vtype)))
                   (bind-word name :value :type vtype)
                   (bind-word name :pattern :form (tform tp fields vtype))))))))
        (t)))
    (dolist (expr exprs)
      (match expr
        (("#def" pat val)
         (evcase (lookup-word pat :value :type)
           ((tclose type) (unify pat type (typecheck val)))))
        (t))))
  (let (last)
    (dolist (expr exprs)
      (match expr
        (("#def" . :_))
        (("#data" . :_))
        (t (setf last (typecheck expr)))))
    last))

(defun typecheck (expr)
  (match expr
    ((:seq forms) (typecheck-seq forms))
    (("#fn" args body)
     (let ((args (lmapseq (arg args)
                   (bind-word arg :value :type (mkvar)))))
       (fun args (typecheck body))))
    (("#match" inputs cases)
     (let ((input-types (lmapseq (input inputs) (typecheck input)))
           (out (mkvar)))
       (doseq (("->" pats body) cases)
         (loop :for tp :in input-types :for pat :in (seq-list pats) :do
            (unify pat tp (typecheck-pat pat)))
         (unify expr out (typecheck body)))
       out))
    ((head . args)
     (let ((arg-types (mapcar #'typecheck args))
           (ret-ty (mkvar)))
       (unify head (fun arg-types ret-ty) (typecheck head))
       ret-ty))
    (:word
     (let ((found (lookup-word expr :value :type)))
       ;; FIXME different error type
       (unless found (hob-type-error expr "undefined variable `~a`" expr))
       (vcase found
         ((tclose cx type) (instantiate type (instance cx)))
         (t found))))
    (:lit (type-of-lit expr))))

(defun typecheck-pat (pat)
  (match pat
    (:lit (type-of-lit pat))
    (:word
     (if (is-variable pat)
         (if (string= (h-word-name pat) "_")
             (mkvar)
             (bind-word pat :value :type (mkvar)))
         (typecheck pat)))
    (((:as :word head) . args)
     (let ((found (or (lookup-word head :pattern :form)
                      (hob-type-error head "undefined pattern `~a`" head)))
           (result (mkvar)))
       (unless (= (length args) (length (tform-args found)))
         (hob-type-error "wrong number of arguments for `~a` (expected ~a)" head (length (tform-args found))))
       (unify pat (fun (loop :for arg :in args :collect (typecheck-pat arg)) result)
              (vcase (tform-templ found)
                ((tclose cx type) (instantiate type (instance cx)))
                (t (tform-templ found))))
       result))))

(defun type-of-lit (expr)
  (etypecase (h-lit-val expr)
    (integer (inst (prim "Int" 'integer) ()))
    (number (inst (prim "Float" 'double-float) ()))
    (character (inst (prim "Char" 'character) ()))
    (string (inst (prim "String" 'string) ()))))

(defun resolve (ty)
  (vcase ty
    ((tvar ref) (if ref (setf (tvar-ref ty) (resolve ref)) ty))
    (t ty)))

(defun unify (expr t1 t2)
  (setf t1 (resolve t1)
        t2 (resolve t2))
  (when (eq t1 t2) (return-from unify t1))
  (flet ((fail () (hob-type-error expr "failed to unify types ~a and ~a"
                                  (print-type t1) (print-type t2))))
    (when (and (typep t2 'tvar)
               (or (not (typep t1 'tvar)) (< (length (tvar-cx t1)) (length (tvar-cx t2)))))
      (rotatef t1 t2))
    (evcase t1
      ((tvar)
       (when (occurs t1 t2)
         (hob-type-error expr "can not construct infinite type ~a ~a" t1 t2))
       (setf (tvar-ref t1) t2)
       (when (tvar-instances t1)
         (if (typep t2 'tvar)
             (setf (tvar-instances t2) (nconc (tvar-instances t1) (tvar-instances t2))
                   (tvar-instances t1) nil)
             (loop :for (inst . v) :in (tvar-instances t1) :do
                ;; FIXME this expr will be utterly useless for understanding errors
                (unify expr v (instantiate t2 inst)))))
       t2)
      ((inst type args)
       (unless (and (typep t2 'inst)
                    (vcase type
                      (prim (equalp type (inst-type t2)))
                      (t (eq type (inst-type t2))))
                    (= (length args) (length (inst-args t2))))
         (fail))
       (inst type (loop :for a1 :in args :for a2 :in (inst-args t2) :collect
                     (unify expr a1 a2))))
      ((fun args result)
       (unless (and (eq (type-of t2) 'fun) (= (length args) (length (fun-args t2))))
         (fail))
       (fun (loop :for a1 :in args :for a2 :in (fun-args t2) :collect (unify expr a1 a2))
            (unify expr result (fun-result t2)))))))

(defun print-type (type)
  (let ((seen ()) (next (char-code #\a)))
    (labels ((prnt (type)
               (setf type (resolve type))
               (evcase type
                 ((fun args result)
                  (format nil "(~{~a ~}-> ~a)" (mapcar #'prnt args) (prnt result)))
                 ((inst type args)
                  (let ((name (vcase type
                                ((prim name) name)
                                ((data name) name))))
                    (if args (format nil "(~a~{ ~a~})" name (mapcar #'prnt args)) name)))
                 ((tvar)
                  (or (cdr (assoc type seen))
                      (let* ((n (code-char next))
                             (s (string n)))
                        (incf next)
                        (push (cons type s) seen)
                        s))))))
      (prnt type))))

(defun occurs (var tp)
  (let ((tp (resolve tp)))
    (evcase tp
      ((tvar) (eq var tp))
      ((inst args) (loop :for a :in args :when (occurs var a) :return t))
      ((fun args result) (or (loop :for a :in args :when (occurs var a) :return t)
                             (occurs var result))))))
