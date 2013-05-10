(in-package :hob)

(defvar *top* (scope nil))

(defmacro define-macro (ns name &body clauses)
  (let ((a (gensym)))
    `(bind! *top* ,ns ,name :macro
            (lambda (,a)
              (let ((*expanding* ,a))
                (match* (h-app-args ,a) ,@clauses
                        (t (syntax-error (h-app-head ,a) "Bad arguments to ~a" ,name))))))))

(defmacro define-seq-macro (name (arg) &body body)
  `(bind! *top* :value ,name :seq-macro (lambda (,arg) ,@body)))

;; Macros

(define-macro :value "def"
  ((pat value) (h-app "#def" pat value)))

(define-macro :value "if"
  ((test then else) (h-app "#match" test
                           (h-seq (list (h-app "->" (h-word "$true") then)
                                        (h-app "->" (h-word "$false") else)))))
  ((test then) (h-app "if" test then (h-word "()" (expr-end-pos then))))
  ((test then "else" else) (h-app "if" test then else)))

(define-seq-macro "->" (cases)
  (block nil
    (let (arr-pos)
      (match cases
        (((:as "->" arr) args body)
         (setf arr-pos (expr-start-pos arr))
         (unless (doseq (arg args) (unless (is-variable arg) (return t)))
           (return (h-app (h-word "#fn" arr-pos) args body))))
        ((:seq exprs) (setf arr-pos (expr-start-pos (h-app-head (car exprs))))))
      (let* ((n-pats (test-cases cases))
             (syms (loop :repeat n-pats :for i :from 0 :collect (format nil "#arg~a" i))))
        (h-app (h-word "#fn" arr-pos) (h-seq (mapcar #'h-word syms))
               (h-app (h-word "#match" arr-pos) (h-seq (mapcar #'h-word syms)) cases))))))
  
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

;; Types

(defparameter *int* (prim "Int" 'integer))
(bind! *top* :type "Int" :type *int*)
(bind! *top* :type "Float" :type (prim "Float" 'double-float))
(bind! *top* :type "Char" :type (prim "Char" 'character))
(bind! *top* :type "String" :type (prim "String" 'string))

;; Basic arithmetic

(let* ((int (inst *int* ()))
       (binary (fun (list int int) int)))
  (bind! *top* :value "+" :type binary)
  (bind! *top* :value "-" :type binary)
  (bind! *top* :value "*" :type binary)
  (bind! *top* :value "/" :type binary))

(bind! *top* :value "+" :value (lambda (a b) (+ a b)))
(bind! *top* :value "-" :value (lambda (a b) (- a b)))
(bind! *top* :value "*" :value (lambda (a b) (* a b)))
(bind! *top* :value "/" :value (lambda (a b) (/ a b)))

;; Unit type

(defparameter *unit* (data "()" () (list "()")))
(bind! *top* :type "()" :type *unit*)
(bind! *top* :value "()" :type (inst *unit* ()))
(bind! *top* :value "()" :value (vector 1))

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
         (ctor (tclose cx (fun vars (inst type vars)))))
    (bind! *top* :type name :type type)
    (bind! *top* :value name :type ctor)
    (bind! *top* :pattern name :form (tform type vars ctor))
    (bind! *top* :value name :value
           (let ((syms (loop :repeat arity :collect (gensym))))
             (funcall (compile-s-expr `(lambda ,syms (vector 1 ,@syms))))))))

(loop :for a :from 2 :below 10 :do (declare-tuple a))
