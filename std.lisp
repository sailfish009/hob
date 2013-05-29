(in-package :hob)

(defvar *top* (scope nil))

(defmacro define-macro (ns name &body clauses)
  (let ((a (gensym)))
    `(bind! *top* ,ns ,name :macro
            (lambda (,a)
              (let ((*expanding* ,a))
                (match* (h-app-args ,a) ,@clauses
                        (t 
                         (locally
                             #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                             (syntax-error (h-app-head ,a) "Bad arguments to ~a" ,name)))))))))

(defmacro define-seq-macro (name (arg) &body body)
  `(bind! *top* :value ,name :seq-macro (lambda (,arg) ,@body)))

;; Macros

(define-macro :value "def"
  ((pat value) (h-app "#def" pat value)))

(define-macro :value "var"
  ((pat value) (h-app "#var" pat value)))

(define-macro :value "if"
  ((test then else) (h-app "#match" test
                           (h-seq (list (h-app "->" (h-word "$true") then)
                                        (h-app "->" (h-word "$false") else)))))
  ((test then) (h-app "if" test then (h-word "()" (expr-end-pos then))))
  ((test then "else" else) (h-app "if" test then else)))

(defun split-arglist (args)
  (let (req opt rest in-opt in-rest)
    (dolist (arg args)
      (match arg
        ("&rest"
         (when in-rest (syntax-error arg "multiple &rest directives"))
         (setf in-rest t in-opt nil))
        ("&opt"
         (when in-opt (syntax-error arg "multiple &opt directives"))
         (when in-rest (syntax-error arg "&opt after &rest directive"))
         (setf in-opt t))
        (t
         (cond (in-rest
                (when rest (syntax-error arg "multiple arguments after &rest"))
                (setf rest arg))
               (in-opt (push arg opt))
               (t (push arg req))))))
    (when (and in-rest (not rest)) (syntax-error args "missing argument name after &rest"))
    (values (nreverse req) (nreverse opt) rest)))

(defun parse-arglist (args)
  (let* ((list (seq-list args))
         (guard (and (cddr list)
                     (let ((last-two (nthcdr (- (length list) 2) list)))
                       (and (h-word-p (car last-two)) (equal (h-word-name (car last-two)) "&if")
                            (second last-two))))))
    (when guard (setf list (subseq list 0 (- (length list) 2))))
    (multiple-value-bind (req opt rest) (split-arglist list)
      (loop :for expr :in opt :do
         (match expr
           (("=" :_ :_))
           (t (syntax-error expr "optional argument without default value"))))
      (values req opt rest guard))))

(define-seq-macro "->" (cases)
  (block nil
    (let (fn n-req n-opt has-rest parsed)
      (doseq (((:as "->" arr) pats body) cases)
        (unless fn (setf fn (h-word "#fn" (h-word-pos arr))))
        (multiple-value-bind (req opt rest guard) (parse-arglist pats)
          (cond ((not n-req)
                 (setf n-req (length req) n-opt (length opt) has-rest (and rest t)))
                (t (when (or opt (> n-opt 0))
                     (syntax-error pats "can't have optional arguments with multiple matched forms"))
                   (unless (= (length req) n-req) (syntax-error pats "mismatched number of required arguments"))
                   (unless (eq (and rest t) has-rest) (syntax-error pats "mismatched presence of &rest argument"))))
          (push (list req opt rest guard body) parsed)))
      ;; Simple call, no need for pattern matching
      (unless (cdr parsed)
        (destructuring-bind (req opt rest guard body) (car parsed)
          (when (and (not guard)
                     (every #'is-variable req)
                     (dolist (optvar opt t)
                       (unless (is-variable (car (h-app-args optvar))) (return nil)))
                     (or (not rest) (is-variable rest)))
            (return (h-app fn (h-seq req) (h-seq opt) (or rest (h-nil)) body)))))
      (setf parsed (nreverse parsed))
      (let ((req-syms (loop :for i :below n-req :collect (h-word (format nil "#arg~a" i))))
            (opt-syms (loop :for i :below n-opt :collect (h-word (format nil "#arg~a" (+ n-req i)))))
            (rest-sym (and has-rest (h-word "#rest"))))
        (h-app fn (h-seq req-syms)
               (h-seq (loop :for sym :in opt-syms :for form :in (second (car parsed)) :collect
                         (h-app "=" sym (second (h-app-args form)))))
               (or rest-sym (h-nil))
               (h-app "#match" (h-seq (append req-syms opt-syms (and rest-sym (list rest-sym))))
                      (h-seq (loop :for (req opt rest guard body) :in parsed :collect
                                (h-app "->" (h-seq (append req (mapcar (lambda (x) (car (h-app-args x))) opt)
                                                           (and rest (list rest)) (and guard (list (h-word "&if") guard))))
                                       body)))))))))
  
(define-macro :value "match"
  ((args cases) (h-app "#match" args cases)))

(define-macro :value "let"
  ((bindings body)
   (let (pats values)
     (doseq (b bindings)
       (match b
         (("=" pat val) (push pat pats) (push val values))
         (t (syntax-error b "malformed let binding"))))
     (h-app "#match" (h-seq (nreverse values))
            (h-app "->" (h-seq (nreverse pats)) body)))))

(define-macro :value "data"
  ((name variants) (h-app "#data" name variants)))

(define-macro :value "<-"
  ((place val) (h-app "#assign" place val)))

(define-macro :type "->"
  ((args result)
   (multiple-value-bind (req opt rest) (split-arglist (seq-list args))
     (h-app "#fn" (h-seq req) (h-seq opt) (or rest (h-nil)) result))))

;; Types

(defparameter *int* (prim "Int" 'integer))
(defparameter *float* (prim "Float" 'double-float))
(defparameter *char* (prim "Char" 'character))
(defparameter *string* (prim "String" 'string))
(bind! *top* :type "Int" :type *int*)
(bind! *top* :type "Float" :type *float*)
(bind! *top* :type "Char" :type *char*)
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

(defparameter *unit* (data "_" () (list "$_")))
(bind! *top* :type "_" :type *unit*)
(bind! *top* :value "$_" :type (inst *unit* ()))
(bind! *top* :value "$_" :value (vector 1))

;; Booleans

(defparameter *bool* (data "Bool" () (list "$true" "$false")))
(bind! *top* :type "Bool" :type *bool*)
(bind! *top* :value "$true" :type (inst *bool* ()))
(bind! *top* :value "$false" :type (inst *bool* ()))
(bind! *top* :value "$true" :value (vector 1))
(bind! *top* :value "$false" :value (vector 2))

;; Tuples

(defun declare-tuple (arity)
  (let* ((cx (gensym))
         (*type-cx* (cons cx *type-cx*))
         (name (with-output-to-string (out) (dotimes (i (1- arity)) (write-char #\, out))))
         (vars (loop :repeat arity :collect (mkvar)))
         (type (data name vars (list name)))
         (ctor (tclose cx (mkfun vars (inst type vars)))))
    (bind! *top* :type name :type type)
    (bind! *top* :value name :type ctor)
    (bind! *top* :pattern name :form (tform type vars ctor))
    (bind! *top* :value name :value
           (let ((syms (loop :repeat arity :collect (gensym))))
             (funcall (compile-s-expr `(lambda ,syms (vector 1 ,@syms))))))))

(loop :for a :from 2 :below 10 :do (declare-tuple a))

;; Option type

(defparameter *option*
  (let* ((cx (gensym))
         (*type-cx* (cons cx *type-cx*))
         (var (list (mkvar)))
         (type (data "Option" var (list "Some" "None")))
         (ctor (tclose cx (mkfun var (inst type var)))))
    (bind! *top* :type "Option" :type type)
    (bind! *top* :value "some" :type ctor)
    (bind! *top* :value "some" :value (lambda (a) (vector 1 a)))
    (bind! *top* :pattern "some" :form (tform type var ctor))
    (bind! *top* :value "$none" :type (tclose cx (inst type (list var))))
    (bind! *top* :value "$none" :value (vector 2))
    type))

;; List type

(defparameter *list*
  (let* ((cx (gensym))
         (*type-cx* (cons cx *type-cx*))
         (v (mkvar))
         (type (data "[]" (list v) (list "cons" "$nil")))
         (inst (inst type (list v)))
         (ctor (tclose cx (mkfun (list v inst) inst))))
    (bind! *top* :type "[]" :type type)
    (bind! *top* :value "$nil" :type (tclose cx (inst type (list v))))
    (bind! *top* :value "$nil" :value (vector 2))
    (bind! *top* :value "cons" :type ctor)
    (bind! *top* :value "cons" :value (lambda (a b) (vector 1 a b)))
    (bind! *top* :pattern "cons" :form (tform type (list v inst) ctor))
    type))
