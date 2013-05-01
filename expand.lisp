(in-package :hob)

(defparameter *toplevel* (scope nil))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-define-val-macro (seqp name body)
    (let* ((kind (if seqp :seq-macro (if (char= (schar name 0) #\#) :special :macro)))
           (base (vlookup* *toplevel* name kind)))
      (if base
          (setf (binding-val base) body)
          (vbind *toplevel* name kind body))))
  (defun do-define-type-macro (name body)
    (let* ((kind (if (char= (schar name 0) #\#) :special :macro))
           (base (tlookup* *toplevel* name kind)))
      (if base
          (setf (binding-val base) body)
          (tbind *toplevel* name kind body))))
  (defun do-define-pat-macro (name body)
    (let* ((kind (if (char= (schar name 0) #\#) :special :macro))
           (base (plookup* *toplevel* name kind)))
      (if base
          (setf (binding-val base) body)
          (pbind *toplevel* name kind body)))))

(defmacro define-macro (type name &body clauses)
  (let ((a (gensym))
        (e (if (consp name) (prog1 (second name) (setf name (car name))) (gensym))))
    `(,(ecase type (:value 'do-define-val-macro) (:type 'do-define-type-macro) (:pat 'do-define-pat-macro))
       ,@(when (eq type :value) '(nil))
       ,name
       (lambda (,e ,a)
         (declare (ignorable ,e))
         (let ((*expanding* (h-app-head ,a)))
           (match* (h-app-args ,a) ,@clauses
                   (t (syntax-error (h-app-args ,a) "Bad arguments to ~a" ,name))))))))

(defmacro define-seq-macro (name (&rest args) &body body)
  (let (env arg)
    (ecase (length args)
      (1 (setf env (gensym) arg (car args)))
      (2 (setf env (car args) arg (second args))))
    `(do-define-val-macro t ,name
                          (lambda (,env ,arg)
                            (declare (ignorable ,env))
                            ,@body))))

(defun test-cases (cases)
  (let (n-args)
    (doseq (case cases)
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
                    (let ((inner (scope env)))
                      (match cs
                        (((:as "->" h) pats body)
                         (let ((pats (expand-patterns pats inner)))
                           (h-app h pats (expand-value body inner)))))))))
       (h-app "#match" values cases)))))

(define-macro :value ("#fn" env)
  ((args body)
   (let ((inner (scope env)))
     (doseq (arg args)
       (unless (is-variable arg)
         (syntax-error arg "invalid binding in argument list"))
       (vbind inner (h-word-name arg) :value nil))
     (h-app "#fn" args (expand-value body inner)))))

;; Value expansion

(defun expand-value (expr env)
  (expand-value-inner (expand-value-outer expr env) env))

(defun expand-value-inner (expr env)
  (match expr
    ((head . rest)
     (let ((spec-form (and (h-word-p head) (vlookup* env (h-word-name head) :special))))
       (if spec-form
           (funcall (binding-val spec-form) env expr)
           (h-app* (expand-value head env) (loop :for arg :in rest :collect (expand-value arg env))))))
    ((:seq elts)
     (let ((can-expand (match (car elts)
                         (((:word name) . :_)
                          (let ((macro (vlookup* env name :seq-macro)))
                            (and macro (binding-val macro))))
                         (:_ nil))))
       (if can-expand
           (expand-value (funcall can-expand env expr) env)
           (expand-value-seq elts env))))
    (:_ expr)))

(defun expand-value-outer (expr env)
  (loop :do
     (match expr
       (((:word name) . :_)
        (let ((macro (vlookup* env name :macro)))
          (if macro
              (setf expr (funcall (binding-val macro) env expr))
              (return expr))))
       (t (return expr)))))

(defun expand-value-seq (exprs env)
  (let ((env (scope env)))
    (setf exprs (loop :for expr :in exprs :collect
                   (let ((expr (expand-value-outer expr env)))
                     (match expr
                       (("#type" (:word name) :_ (:seq forms))
                        (tbind env name :type nil)
                        (loop :for form :in forms :do
                           (match form
                             (("#variant" (:word name) . :_)
                              (vbind env name :value nil)
                              ;; FIXME shadow pattern
                              ))))
                       ;; FIXME relying on def->#def to expand patterns and thus shadow vars
                       (:_))
                     expr)))
    (h-seq (loop :for expr :in exprs :collect (expand-value-inner expr env)))))

;; Pattern expansion

(defun reg-binding (expr env &optional (check t))
  (let ((name (h-word-name expr)))
    (unless (string= name "_")
      (when (and check (vlookup* env name :value))
        (syntax-error expr "binding ~a multiple times" name))
      (vbind env name :value nil))))

(defun expand-pattern (expr env)
  (match expr
    (:word (when (is-variable expr) (reg-binding expr env)) expr)
    (:lit expr)
    ;; FIXME pattern macros
    ((head . args) (h-app* head (loop :for arg :in args :collect (expand-pattern arg env))))
    (:_ (syntax-error expr "invalid pattern"))))

(defun expand-patterns (expr env)
   (mapseq (pat expr) (expand-pattern pat env)))

(defun pattern-vars (pat)
  (let ((*bound* ()))
    (labels ((iter (pat)
               (match pat
                 (:word (when (is-variable pat) (reg-binding pat nil)))
                 ((:_ . args) (dolist (arg args) (iter arg)))
                 (:_))))
      (if (h-seq-p pat)
          (doseq (p pat) (iter p))
          (iter pat))
      *bound*)))

;; Tester

(defun test-expand (str &optional name)
  (expand-value (parse str name) *toplevel*))

;; Macros

(define-macro :value ("def" env)
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
