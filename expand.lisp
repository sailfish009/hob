(in-package :hob)

(defparameter *macros* (list (list :type) (list :value) (list :pattern)))
(defparameter *special-forms* (copy-tree *macros*))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-define-macro (type seqp name body)
    (let ((base (assoc type (if (char= (schar name 0) #\#) *special-forms* *macros*))))
      (loop :for cons :on (cdr base) :do
         (when (equal (car cons) name) (return (setf (cdr cons) (cons seqp body))))
         :finally (push (cons name (cons seqp body)) (cdr base))))))

(defmacro define-macro (type name &body clauses)
  (let ((a (gensym))
        (e (if (consp name) (prog1 (second name) (setf name (car name))) (gensym))))
    `(do-define-macro ,type nil ,name
                      (lambda (,e ,a)
                        (declare (ignorable ,e))
                        (match* ,a ,@clauses
                           (t (syntax-error ,a "Bad arguments to ~a" ,name)))))))

(defmacro define-seq-macro (type name (&rest args) &body body)
  (let (env arg)
    (ecase (length args)
      (1 (setf env (gensym) arg (car args)))
      (2 (setf env (car args) arg (second args))))
    `(do-define-macro ,type t ,name
                      (lambda (,env ,arg)
                        (declare (ignorable ,env))
                        ,@body))))

(defun extend (env type list)
  (cons (cons type list) env))

(defun lookup (type word env)
  (loop :for (tp . bindings) :in env :do
     (when (equal tp type)
       (loop :for (name . val) :in bindings :do
          (when (equal name word) (return-from lookup val))))))

(defun find-special-form (type expr)
  (and (h-word-p expr)
       (cddr (assoc (h-word-name expr)
                    (cdr (assoc type *special-forms*)) :test #'string=))))

(defun test-cases (cases)
  (let (n-args)
    (loop :for case :in (seq-list cases) :do
       (match case
         (("->" args :_)
          (if n-args
              (unless (= (seq-len args) n-args)
                (syntax-error args "inconsistent argument count"))
              (setf n-args (seq-len args))))
         (t (syntax-error case "expected arrow application"))))
    n-args))

(define-macro :value ("#def" env)
  ((pat value) (h-app "#def" pat (expand-value value env))))

(define-macro :value ("#match" env)
  ((values cases)
   (let ((n-cases (test-cases cases))
         (values (mapseq (value values) (expand-value value env))))
     (unless (= n-cases (seq-len values))
       (syntax-error values "amount of input values doesn't match amount of patterns"))
     (let ((cases (mapseq (cs cases)
                    (match cs
                      (("->" pats body)
                       (multiple-value-bind (pats bound) (expand-patterns pats env)
                         (h-app "->" pats (expand-value body (extend env :value (loop :for b :in bound :collect (cons b nil)))))))))))
       (h-app "#match" values cases)))))

(define-macro :value ("#fn" env)
  ((args body)
   (let ((shadow ()))
     (loop :for arg :in (seq-list args) :do
        (unless (is-variable arg)
          (syntax-error arg "invalid binding in argument list"))
        (push (cons (h-word-name arg) nil) shadow))
     (h-app "#fn" args (expand-value body (extend env :value shadow))))))

;; Value expansion

(defun expand-value (expr env)
  (setf expr (expand-value-outer expr env))
  (match expr
    ((head . rest)
     (let ((spec-form (find-special-form :value head)))
       (if spec-form
           (funcall spec-form env rest)
           (h-app* (expand-value head env) (loop :for arg :in rest :collect (expand-value arg env))))))
    ((:seq elts)
     (let ((can-expand (match (car elts)
                         (((:word name) . :_)
                          (let ((macro (lookup :value name env)))
                            (and (car macro) (cdr macro))))
                         (:_ nil))))
       (if can-expand
           (expand-value (funcall can-expand env expr) env)
           (expand-value-seq elts env))))
    (:_ expr)))

(defun expand-value-outer (expr env)
  (loop :do
     (match expr
       (((:word name) . rest)
        (let ((macro (lookup :value name env)))
          (if macro
              (setf expr (funcall (cdr macro) env (if (car macro) expr rest)))
              (return expr))))
       (t (return expr)))))

(defun expand-value-seq (exprs env)
  (let ((old-env env))
    (flet ((shadw (type name)
             (let ((list (loop :for part :in env :do
                            (cond ((eq part (car old-env))
                                   (let ((new (list type))) (push new env) (return new)))
                                  ((eq (car part) type) (return part))))))
               (push (cons name nil) (cdr list)))))
      (setf exprs (loop :for expr :in exprs :collect
                     (let ((expr (expand-value-outer expr env)))
                       (match expr
                         (("#type" (:word name) :_ (:seq forms))
                          (shadw :type name)
                          (loop :for form :in forms :do
                             (match form
                               (("#variant" (:word name) . :_)
                                (shadw :value name)
                                (shadw :pattern name)))))
                         (("#def" pat :_) (loop :for var :in (pattern-vars pat) :do (shadw :value var)))
                         (:_))
                       expr)))
      (h-seq (loop :for expr :in exprs :collect (expand-value expr env))))))

;; Pattern expansion

(defvar *bound*)

(defun reg-binding (expr &optional (check t))
  (let ((name (h-word-name expr)))
    (unless (string= name "_")
      (when (and check (member name *bound* :test #'string=))
        (syntax-error expr "binding ~a multiple times" name))
      (push name *bound*))))

(defun expand-pattern* (expr env)
  (match expr
    (:word (when (is-variable expr) (reg-binding expr)) expr)
    (:lit expr)
    ;; FIXME pattern macros
    ((head . args) (h-app* head (loop :for arg :in args :collect (expand-pattern* arg env))))
    (:_ (syntax-error expr "invalid pattern"))))

(defun expand-pattern (expr env)
  (let ((*bound* ()))
    (values (expand-pattern* expr env) *bound*)))

(defun expand-patterns (expr env)
  (let ((*bound* ()))
    (values (mapseq (pat expr) (expand-pattern* pat env)) *bound*)))

(defun pattern-vars (pat)
  (let ((*bound* ()))
    (labels ((iter (pat)
               (match pat
                 (:word (when (is-variable pat) (reg-binding pat nil)))
                 ((:_ . args) (dolist (arg args) (iter arg)))
                 (:_))))
      (iter pat)
      *bound*)))

;; Tester

(defun test-expand (str &optional name)
  (expand-value (parse str name) *macros*))

;; Macros

(define-macro :value ("def" env)
  ((pat value) (h-app "#def" (expand-pattern pat env) value)))

(define-macro :value "if"
  ((test then else) (h-app "#match" test
                           (h-seq (list (h-app "->" (h-word "$true") then)
                                        (h-app "->" (h-word "$false") else)))))
  ((test then) (h-app "if" test then (h-word "()" (expr-end-pos then))))
  ((test then "else" else) (h-app "if" test then else)))

(define-seq-macro :value "->" (cases)
  (block nil
    (match cases
      (("->" args body)
       (unless (loop :for arg :in (seq-list args) :do
                  (unless (is-variable arg) (return t)))
         (return (h-app "#fn" args body))))
      (t))
    (let* ((n-pats (test-cases cases))
           (syms (loop :repeat n-pats :for i :from 0 :collect (format nil "#arg~a" i))))
      (h-app "#fn" (h-seq (mapcar #'h-word syms))
             (h-app "#match" (h-seq (mapcar #'h-word syms)) cases)))))
  
(define-macro :value "match"
  ((args cases) (h-app "#match" args cases)))

(define-macro :value "let"
  ((bindings body)
   (let (pats values)
     (loop :for b :in (seq-list bindings) :do
        (match b
          (("=" pat val) (push pat pats) (push val values))
          (t (syntax-error b "malformed let binding"))))
     (h-app "#match" (h-seq (nreverse values))
            (h-app "->" (h-seq (nreverse pats)) body)))))
