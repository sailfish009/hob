(in-package :hob)

(defparameter *toplevel* (scope nil))
(defparameter *special-forms* ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-define-macro (seqp ns name body)
    (let* ((kind (if seqp :seq-macro :macro))
           (base (get-binding *toplevel* ns name))
           (cons (assoc kind (binding-fields base))))
      (if cons
          (setf (cdr cons) body)
          (push (cons kind body) (binding-fields base)))))
  (defun do-define-special-form (name outer inner)
    (let ((known (assoc name *special-forms* :test #'string=)))
      (if known
          (setf (cdr known) (cons outer inner))
          (push (cons name (cons outer inner)) *special-forms*)))))

(defmacro define-macro (ns name &body clauses)
  (let ((a (gensym)))
    `(do-define-macro nil ,ns ,name
       (lambda (,a)
         (let ((*expanding* ,a))
           (match* (h-app-args ,a) ,@clauses
                   (t (syntax-error (h-app-head ,a) "Bad arguments to ~a" ,name))))))))

(defmacro define-seq-macro (name (arg) &body body)
  `(do-define-macro t :value ,name (lambda (,arg) ,@body)))

(defmacro define-special-form (name (&optional env form) &body clauses)
  (let ((env (or env (gensym)))
        (form (or form (gensym))))
    `(do-define-special-form ,name
       (lambda (,env ,form)
         (declare (ignorable ,env))
         (let ((*expanding* ,form))
           (match* (h-app-args ,form)
             ,@(loop :for (pat . body) :in clauses
                  :when (member :outer body) :collect
                  (cons pat (nthcdr (1+ (position :outer body)) body)))
             (t ,form))))
       (lambda (,env ,form)
         (declare (ignorable ,env))
         (let ((*expanding* ,form))
           (match* (h-app-args ,form)
             ,@(loop :for (pat . body) :in clauses :collect
                  (cons pat (loop :for elt :in body

                               :when (eq elt :outer) :do (return b)
                               :collect elt :into b :finally (return b))))
             (t (syntax-error (h-app-head ,form) "Bad arguments to ~a" ,name))))))))
          

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

(define-special-form "#def" (env)
  ((pat value)
   (h-app "#def" pat (expand-value value env))
   :outer
   (h-app "#def" (expand-pattern pat env) value)))

(define-special-form "#match" (env)
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

(define-special-form "#fn" (env)
  ((args body)
   (let ((inner (scope env)))
     (doseq (arg args)
       (unless (is-variable arg)
         (syntax-error arg "invalid binding in argument list"))
       (get-binding inner :value (h-word-name arg)))
     (h-app "#fn" args (expand-value body inner)))))

(defun as-binding (word env ns)
  (get-binding env ns (h-word-name word))
  (assert (not (h-word-env word)))
  (setf (h-word-env word) env)
  word)

(define-special-form "#type" (env form)
  (((:as :word name) (:as :seq args) (as :seq forms))
   (let ((inner (scope env)))
     (doseq ((:as :word arg) args)
       (as-binding arg inner :type))
     (h-app "#type" name args (mapseq (form forms)
                                (match form
                                  ((name . fields)
                                   (h-app* name (loop :for field :in fields :collect
                                                   (expand-type field inner))))))))
   :outer
   (as-binding name env :type)
   (doseq (((:as :word vname) . fields) forms)
     (as-binding vname env :value))
   form))

;; Value expansion

(defun expand-value (expr env)
  (expand-value-inner (expand-value-outer expr env) env))

(defun expand-value-inner (expr env)
  (match expr
    ((head . rest)
     (let ((special (and (h-word-p head) (cdr (assoc (h-word-name head) *special-forms* :test #'string=)))))
       (if special
           (funcall (car special) env expr)
           (h-app* (expand-value head env) (loop :for arg :in rest :collect (expand-value arg env))))))
    ((:seq elts)
     (let ((can-expand (match (car elts)
                         (((:word name) . :_) (lookup env :value name :seq-macro))
                         (:_ nil))))
       (if can-expand
           (expand-value (funcall can-expand expr) env)
           (expand-value-seq elts env))))
    (:word (unless (h-word-env expr) (setf (h-word-env expr) env)) expr)
    (:_ expr)))

(defun expand-value-outer (expr env)
  (loop :do
     (match expr
       (((:word name) . :_)
        (let ((special (cdr (assoc name *special-forms* :test #'string=))))
          (if special
              (return (funcall (cdr special) env expr))
              (let ((macro (lookup env :value name :macro)))
                (if macro
                    (setf expr (funcall macro expr))
                    (return expr))))))
       (t (return expr)))))

(defun expand-value-seq (exprs env)
  (let ((env (scope env)))
    (setf exprs (loop :for expr :in exprs :collect (expand-value-outer expr env)))
    (h-seq (loop :for expr :in exprs :collect (expand-value-inner expr env)))))

;; Pattern expansion

(defun reg-binding (expr env &optional (check t))
  (let ((name (h-word-name expr)))
    (unless (string= name "_")
      (when (and check (find-binding env :value name))
        (syntax-error expr "binding ~a multiple times" name))
      (as-binding expr env :value)))
  expr)

(defun expand-pattern (expr env)
  (match expr
    (:word
     (if (is-variable expr)
         (reg-binding expr env)
         (expand-value expr env)))
    (:lit expr)
    ;; FIXME pattern macros
    ((head . args) (h-app* head (loop :for arg :in args :collect (expand-pattern arg env))))
    (:_ (syntax-error expr "invalid pattern"))))

(defun expand-patterns (expr env)
   (mapseq (pat expr) (expand-pattern pat env)))

;; Type expansion

(defun expand-type (expr env)
  (match expr
    (:word
     (unless (h-word-env expr) (setf (h-word-env expr) env))
     expr)
    ((head . args)
     (h-app* (expand-type head env)
             (loop :for arg :in args :collect (expand-type arg env))))
    (:lit (syntax-error expr "found literal in type position"))
    (:seq (syntax-error expr "found sequence in type position"))))

;; Tester

(defun test-expand (str &optional name)
  (expand-value (parse str name) (scope *toplevel*)))

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
