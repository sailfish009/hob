(in-package :hob)

;; FIXME disallow #let/#date style bindings outside of blocks

(defparameter *special-forms* ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun do-define-special-form (name outer inner)
    (let ((known (assoc name *special-forms* :test #'string=)))
      (if known
          (setf (cdr known) (cons outer inner))
          (push (cons name (cons outer inner)) *special-forms*)))))

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
         (let ((len (match args
                      ((:seq vals) (loop :for i :from 0 :for val :in vals :do
                                      (match val ("&if" (return i)) (t))
                                      :finally (return i)))
                      (t 1))))
           (if n-args
               (unless (= len n-args)
                 (syntax-error args "inconsistent argument count"))
               (setf n-args len))))
        (t (syntax-error case "expected arrow application"))))
    n-args))

(define-special-form "#def" (env)
  ((pat value)
   (h-app "#def" pat (expand-value value env))
   :outer
   (h-app "#def" (expand-pattern pat env env) value)))

(define-special-form "#var" (env)
  ((pat value)
   (h-app "#var" pat (expand-value value env))
   :outer
   (h-app "#var" (expand-pattern pat env env) value)))

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
                         (let ((pats (expand-patterns pats env inner)))
                           (h-app h pats (expand-value body inner)))))))))
       (h-app "#match" values cases)))))

(define-special-form "#fn" (env)
  ((args body)
   (let ((inner (scope env)))
     (doseq (arg args)
       (unless (is-variable arg)
         (syntax-error arg "invalid binding in argument list"))
       (as-binding arg inner :value))
     (h-app "#fn" args (expand-value body inner)))))

(defun as-binding (word env ns &optional is-const)
  (get-binding env ns (h-word-name word))
  (setf (h-word-env word) env)
  (when (is-meta word) (syntax-error word "binding a meta word"))
  (if is-const
      (when (is-variable word) (syntax-error word "using variable name for constant"))
      (unless (is-variable word) (syntax-error word "using constant name for variable")))
  word)

(define-special-form "#data" (env form)
  ((name variants)
   (match name
     (:word)
     (((:word _) . params)
      (setf env (scope env))
      (dolist (param params)
        (unless (h-word-p param)
          (syntax-error param "type parameters must be words"))
        (as-binding param env :type)))
     (t (syntax-error name "invalid name form in data declaration")))
   (h-app "#data" name
          (mapseq (variant variants)
            (match variant
              (:word variant)
              (((:as :word name) . args)
               (h-app* name (loop :for arg :in args :collect
                               (expand-type arg env)))))))
   :outer
   (match name
     (:word (as-binding name env :type))
     (((:as :word name) . _) (as-binding name env :type))
     (t))
   (doseq (variant variants)
     (multiple-value-bind (name args)
         (match variant ((name . args) (values name args)) (t variant))
       (unless (h-word-p name)
         (syntax-error name "variant names must be words"))
       (as-binding name env :value (not args))
       (as-binding name env :pattern (not args))))
   form))

;; Value expansion

(defun expand-value (expr env)
  (expand-value-inner (expand-value-outer expr env) env))

(defun expand-value-inner (expr env)
  (match expr
    ((head . rest)
     (let ((special (and (h-word-p head) (cdr (assoc (h-word-name head) *special-forms* :test #'string=)))))
       (if special
           (funcall (cdr special) env expr)
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
              (return (funcall (car special) env expr))
              (let ((macro (or (lookup env :value name :macro)
                               (lookup env :value name :seq-macro))))
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

(defun expand-pattern (expr outer-env inner-env)
  (match expr
    (:word
     (if (is-variable expr)
         (reg-binding expr inner-env)
         (expand-value expr outer-env)))
    (:lit expr)
    ;; FIXME pattern macros
    (((:as :word head) . args)
     (setf (h-word-env head) outer-env)
     (h-app* head (loop :for arg :in args :collect (expand-pattern arg outer-env inner-env))))
    (:_ (syntax-error expr "invalid pattern"))))

(defun expand-patterns (expr outer-env inner-env)
  (match expr
    ((:seq pats)
     (let (test epats)
       (loop :for cur :on pats :do
          (match (car cur)
            ("&if"
             (unless (and (= (length cur) 2) (not (eq cur pats)))
               (syntax-error (car cur) "unsyntactic &if in pattern"))
             (setf test (expand-value (second cur) inner-env))
             (return))
            (pat (push (expand-pattern pat outer-env inner-env) epats))))
       (setf epats (h-seq (nreverse epats)))
       (if test (h-app "#guard" test epats) epats)))
    (:_ (expand-pattern expr outer-env inner-env))))

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
  (expand-value (parse str name) (scope *top*)))
