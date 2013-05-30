(in-package :hob)

(define-condition hob-type-error (hob-program-error) ())

(define-raise-function hob-type-error)

(variant type*
  (prim name sym)
  (data name n-args forms)
  (array*)
  (tparam val))

(defgeneric type-n-args (tp)
  (:method ((tp data)) (data-n-args tp))
  (:method ((tp array*)) 1)
  (:method (tp) 0))

(defgeneric type-name (tp)
  (:method ((tp data)) (data-name tp))
  (:method ((tp array*)) "[]")
  (:method ((tp prim)) (prim-name tp)))

(variant type-expr
  (fun req-args opt-args rest-arg result cx count)
  (tvar cx instances ref)
  (inst type args)
  (tclose cx type))

;; Builtin types

(defparameter *int* (prim "Int" 'integer))
(bind! *top* :type "Int" :type *int*)
(defparameter *float* (prim "Float" 'double-float))
(bind! *top* :type "Float" :type *float*)
(defparameter *char* (prim "Char" 'character))
(bind! *top* :type "Char" :type *char*)
(defparameter *string* (prim "String" 'string))
(bind! *top* :type "String" :type *string*)

;; Basic arithmetic

(let* ((int (inst *int* ()))
       (binary (mkfun (list int int) int)))
  (bind! *top* :value "+" :type binary)
  (bind! *top* :value "-" :type binary)
  (bind! *top* :value "*" :type binary)
  (bind! *top* :value "/" :type binary))

(bind! *top* :value "+" :value (lambda (a b) (+ a b)))
(bind! *top* :value "-" :value (lambda (a b) (- a b)))
(bind! *top* :value "*" :value (lambda (a b) (* a b)))
(bind! *top* :value "/" :value (lambda (a b) (/ a b)))

;; Unit type

(defparameter *unit*
  (let* ((tp (data "_" 0 ())))
    (push tp (data-forms tp))
    (bind! *top* :type "_" :type tp)
    (bind! *top* :value "$_" :type (inst tp ()))
    (bind! *top* :value "$_" :value (vector 1))
    (bind! *top* :pattern "$_" :type (cons tp 1))
    tp))

;; Booleans

(defparameter *bool*
  (let ((tp (data "Bool" 0 ())))
    (setf (data-forms tp) (list tp tp))
    (bind! *top* :type "Bool" :type tp)
    (bind! *top* :value "$true" :type (inst tp ()))
    (bind! *top* :value "$false" :type (inst tp ()))
    (bind! *top* :value "$true" :value (vector 1))
    (bind! *top* :value "$false" :value (vector 2))
    (bind! *top* :pattern "$true" :type (cons tp 1))
    (bind! *top* :pattern "$false" :type (cons tp 2))
    tp))

;; Tuples

(defun declare-tuple (arity)
  (let* ((cx (gensym))
         (*type-cx* (cons cx *type-cx*))
         (name (with-output-to-string (out) (dotimes (i (1- arity)) (write-char #\, out))))
         (vars (loop :repeat arity :collect (mkvar)))
         (type (data name arity ()))
         (ctor (tclose cx (mkfun vars (inst type vars)))))
    (push ctor (data-forms type))
    (bind! *top* :type name :type type)
    (bind! *top* :value name :type ctor)
    (bind! *top* :pattern name :type (cons type 1))
    (bind! *top* :value name :value
           (let ((syms (loop :repeat arity :collect (gensym))))
             (funcall (compile-s-expr `(lambda ,syms (vector 1 ,@syms))))))))

(loop :for a :from 2 :below 10 :do (declare-tuple a))

;; Option type

(defparameter *option*
  (let* ((cx (gensym))
         (*type-cx* (cons cx *type-cx*))
         (tp (data "Option" 1 ()))
         (var (list (mkvar)))
         (ctor (tclose cx (mkfun var (inst tp var)))))
    (setf (data-forms tp) (list ctor (tclose cx tp)))
    (bind! *top* :type "Option" :type tp)
    (bind! *top* :value "some" :type ctor)
    (bind! *top* :value "some" :value (lambda (a) (vector 1 a)))
    (bind! *top* :pattern "some" :type (cons tp 1))
    (bind! *top* :value "$none" :type (tclose cx (inst tp (list var))))
    (bind! *top* :value "$none" :value (vector 2))
    (bind! *top* :pattern "$none" :type (cons tp 2))
    tp))

;; List type

(defparameter *list*
  (let* ((cx (gensym))
         (*type-cx* (cons cx *type-cx*))
         (tp (data "[]" 1 ()))
         (v (mkvar))
         (inst (inst tp (list v)))
         (ctor (tclose cx (mkfun (list v inst) inst))))
    (setf (data-forms tp) (list ctor (tclose cx tp)))
    (bind! *top* :type "l[]" :type tp)
    (bind! *top* :value "$nil" :type (tclose cx (inst tp (list v))))
    (bind! *top* :value "$nil" :value (vector 2))
    (bind! *top* :pattern "$nil" :type (cons tp 2))
    (bind! *top* :value "cons" :type ctor)
    (bind! *top* :value "cons" :value (lambda (a b) (vector 1 a b)))
    (bind! *top* :pattern "cons" :type (cons tp 1))
    tp))

;; Array type

(defparameter *array* (array*))
(defparameter *array-ctor-type*
  (let* ((cx (gensym))
         (*type-cx* (cons cx *type-cx*))
         (v (mkvar))
         (inst (inst *array* (list v)))
         (ctor (tclose cx (mkfun (list v inst) inst nil v))))
    (bind! *top* :type "[]" :type *array*)
    (bind! *top* :value "[]" :type ctor)
    (bind! *top* :value "[]" :value (lambda (&rest elts) (concatenate 'vector elts)))
    (bind! *top* :pattern "[]" :type (cons *array* nil))
    ctor))

(defvar *type-cx* nil)

(defun mkvar () (tvar *type-cx* nil nil))

(defun mkfun (req-args result &optional opt-args rest-arg count)
  (fun req-args opt-args rest-arg result *type-cx*
       (or count (and (or opt-args rest-arg) (cons nil nil)))))

(defun parse-type (expr)
  (match expr
    (:word
     (let ((found (or (lookup-word expr :type :type)
                      (hob-type-error expr "undefined type `~a`" expr))))
       (cond ((typep found 'tparam) (tparam-val found))
             ((> (type-n-args found) 0) (hob-type-error expr "type `~a` takes type parameters" expr))
             (t (inst found ())))))
    (("#fn" req-args opt-args rest-arg ret)
     (mkfun (mapcar #'parse-type (h-seq-vals req-args))
            (parse-type ret)
            (mapcar #'parse-type (h-seq-vals opt-args))
            (unless (h-seq-p rest-arg) (parse-type rest-arg))))
    (((:as :word head) . targs)
     (let ((found (or (lookup-word head :type :type)
                      (hob-type-error head "undefined type `~a`" expr))))
       (unless (= (length targs) (type-n-args found))
         (hob-type-error expr "incorrect number of parameters for type `~a`" head))
       (inst found (loop :for targ :in targs :collect (parse-type targ)))))))

(defstruct (instance (:constructor instance (cx))) cx subst (instantiating 0))

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
               ((fun req-args opt-args rest-arg result cx count)
                (fun (mapcar #'ins req-args) (mapcar #'ins opt-args)
                     (and rest-arg (ins rest-arg)) (ins result) *type-cx*
                     (if (member (instance-cx inst) cx) (copy-list count) count))))))
    (ins type)))

(defun typecheck-seq (exprs)
  (let* ((cx (gensym))
         (*type-cx* (cons cx *type-cx*))
         (def-types ()))
    (dolist (expr exprs)
      (match expr
        (("#data" name variants)
         (multiple-value-bind (name args) (match name (:word name) ((name . args) (values name args)))
           (dolist (arg args)
             (bind-word arg :type :type (tparam (mkvar))))
           (bind-word name :type :type (data (h-word-name name) (length args) nil))))
        (("#def" pat :_) (push (typecheck-pat pat t) def-types))
        (("#var" pat :_) (push (typecheck-pat pat t t) def-types))
        (t)))
    (dolist (expr exprs)
      (match expr
        (("#data" name variants)
         (let* ((name (match name (:word name) ((name . :_) name)))
                (tp (lookup-word name :type :type))
                (vars (loop :repeat (data-n-args tp) :collect (mkvar)))
                (tpinst (inst tp vars))
                (i 0))
           (doseq (variant variants)
             (multiple-value-bind (name fields)
                 (match variant
                   (:word variant)
                   ((name . fields) (values name (mapcar #'parse-type fields))))
               (let ((vtype (if fields (mkfun fields tpinst) tpinst)))
                 (push vtype (data-forms tp))
                 (when (> 0 (data-n-args tp)) (setf vtype (tclose cx vtype)))
                 (bind-word name :value :type vtype)
                 (bind-word name :pattern :type (cons tp (incf i))))))
           (setf (data-forms tp) (nreverse (data-forms tp)))))
        (t)))
    (setf def-types (nreverse def-types))
    (dolist (expr exprs)
      (match expr
        (((:or "#def" "#var") :_ val)
         (unify val (pop def-types) (typecheck val)))
        (t))))
  (let (last)
    (dolist (expr exprs)
      (match expr
        (((:or "#def" "#var" "#data") . :_))
        (t (setf last (typecheck expr)))))
    last))

(defun typecheck (expr)
  (match expr
    ((:seq forms) (if forms (typecheck-seq forms) (inst *unit* ())))
    (("#fn" req-args opt-args rest-arg body)
     (let ((req-args (lmapseq (req req-args)
                       (bind-word req :value :type (mkvar))))
           (opt-args (lmapseq (("=" name def) opt-args)
                       (bind-word name :value :type (typecheck def))))
           (rest-arg (when (h-word-p rest-arg)
                       (let ((v (mkvar)))
                         (bind-word rest-arg :value :type (inst *list* (list v)))
                         v))))
       (mkfun req-args (typecheck body) opt-args rest-arg)))
    (("#match" inputs cases)
     (let ((input-types (lmapseq (input inputs) (typecheck input)))
           (out (mkvar)))
       (doseq (("->" pats body) cases)
         (loop :for tp :in input-types :for pat :in (seq-list pats) :do
            (unify pat tp (typecheck-pat pat)))
         (unify expr out (typecheck body)))
       out))
    (("#assign" place val)
     (unless (lookup-word place :value :mut)
       (hob-type-error place "binding `~a` is not mutable" place))
     (unify expr (typecheck place) (typecheck val)))
    ((head . args)
     (let ((arg-types (mapcar #'typecheck args))
           (ret-ty (mkvar)))
       (unify head (mkfun arg-types ret-ty) (typecheck head))
       ret-ty))
    (:word
     (let ((found (lookup-word expr :value :type)))
       ;; FIXME different error type
       (unless found (hob-type-error expr "undefined variable `~a`" expr))
       (vcase found
         ((tclose cx type) (instantiate type (instance cx)))
         (t found))))
    (:lit (type-of-lit expr))))

(defun typecheck-pat (pat &optional close mut)
  (let ((type (typecheck-pat* pat close mut)))
    (when *context* (setf (gethash pat (context-pat-types *context*)) type))
    type))

(defun typecheck-pat* (pat close mut)
  (match pat
    (:lit (type-of-lit pat))
    (("#guard" test pat)
     (prog1 (typecheck-pat pat close mut)
       (unify test (typecheck test) (inst *bool* ()))))
    (:word
     (if (is-variable pat)
         (if (string= (h-word-name pat) "_")
             (mkvar)
             (let ((v (mkvar)))
               (when mut (bind-word pat :value :mut t))
               (bind-word pat :value :type (if close (tclose (car *type-cx*) v) v))
               v))
         (typecheck pat)))
    (((:as :word head) . args)
     (let* ((found (or (lookup-word head :pattern :type)
                       (hob-type-error head "undefined pattern `~a`" head)))
            (result (mkvar))
            (templ (type-form-templ (car found) (cdr found))))
       (unify pat (mkfun (loop :for arg :in args :collect (typecheck-pat arg close mut)) result)
              (vcase templ ((tclose cx type) (instantiate type (instance cx))) (t templ)))
       result))))

(defgeneric type-form-templ (tp ctor-id)
  (:method ((tp data) ctor-id)
    (nth (1- ctor-id) (data-forms tp)))
  (:method ((tp array*) ctor-id)
    (declare (ignore ctor-id))
    *array-ctor-type*))

(defun type-of-lit (expr)
  (etypecase (h-lit-val expr)
    (integer (inst *int* ()))
    (number (inst *float* ()))
    (character (inst *char* ()))
    (string (inst *string* ()))))

(defun resolve (ty)
  (vcase ty
    ((tvar ref) (if ref (setf (tvar-ref ty) (resolve ref)) ty))
    ((fun req-args opt-args rest-arg count)
     (loop :while (cdr count) :do (setf count (setf (fun-count ty) (cdr count))))
     (when (car count)
       (setf (fun-count ty) nil
             (fun-opt-args ty) nil
             (fun-rest-arg ty) nil
             (fun-req-args ty) (loop :repeat (car count) :collect (or (pop req-args) (pop opt-args) rest-arg))))
     ty)
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
                (when (> (instance-instantiating inst) 25)
                  (hob-type-error expr "creating infinite type"))
                (incf (instance-instantiating inst))
                (unwind-protect
                     ;; FIXME this expr will be utterly useless for understanding errors
                     (unify expr v (instantiate t2 inst))
                  (decf (instance-instantiating inst))))))
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
      ((fun (req1 req-args) (opt1 opt-args) (rest1 rest-arg) (res1 result) (c1 count))
       (vcase t2
         ((fun (req2 req-args) (opt2 opt-args) (rest2 rest-arg) (res2 result) (c2 count))
          (unify expr res1 res2)
          (when (and c1 c2)
            ;; Two specific types must match exactly
            (unless (and (= (length req1) (length req2)) (= (length opt1) (length opt2))
                         (eq (not res1) (not res2))) (fail))
            (loop :for r1 :in req1 :for r2 :in req2 :do (unify expr r1 r2))
            (loop :for o1 :in opt1 :for o2 :in opt2 :do (unify expr o1 o2))
            (when res1 (unify expr res1 res2))
            (setf (cdr c1) c2)
            (return-from unify (if (< (length (fun-cx t1)) (length (fun-cx t2))) t1 t2)))
          (when c2
            (rotatef t1 t2)
            (rotatef c1 c2)
            (rotatef req1 req2)
            (rotatef opt1 opt2)
            (rotatef rest1 rest2)
            (rotatef res1 res2))
          ;; t1 is specific, must be simplified
          (when c1
            (when (or (and (not rest1) (> (length req2) (+ (length req1) (length opt1))))
                      (< (length req2) (length req1))) (fail))
            (loop :for arg :in req2 :do
               (unify expr arg (or (pop req2) (pop opt1) rest1)))
            (setf (car c1) (length req2))
            (return-from unify t1))
          ;; Neither is specific, simple unification
          (unless (= (length req1) (length req2)) (fail))
          (loop :for a1 :in req1 :for a2 :in req2 :do (unify expr a1 a2))
          t1)
         (t (fail)))))))

(defun print-type (type)
  (let ((seen ()) (next (char-code #\a)))
    (labels ((prnt (type)
               (setf type (resolve type))
               (evcase type
                 ((fun req-args opt-args rest-arg result)
                  (format nil "(~{~a ~}~:[~;~:* &opt~{ ~a~}~]~:[~;~:*&rest ~a~] -> ~a)"
                          (mapcar #'prnt req-args) (mapcar #'prnt opt-args)
                          (and rest-arg (prnt rest-arg)) (prnt result)))
                 ((inst type args)
                  (let ((name (type-name type)))
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
      ((fun req-args opt-args rest-arg result)
       (or (loop :for a :in req-args :when (occurs var a) :return t)
           (loop :for a :in opt-args :when (occurs var a) :return t)
           (and rest-arg (occurs var rest-arg))
           (occurs var result))))))
