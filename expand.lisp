(in-package :hob)

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

(defun maybe-add-env (word env)
  (if (h-word-env word)
      (copy-h-word word)
      (ann-word word :env env)))

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

(defun as-binding (word env ns &optional is-const)
  (get-binding env ns (h-word-name word))
  (when (is-meta word) (syntax-error word "binding a meta word"))
  (if is-const
      (when (is-variable word) (syntax-error word "using variable name for constant"))
      (unless (is-variable word) (syntax-error word "using constant name for variable")))
  (ann-word word :env env))

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

(define-special-form "#assign" (env)
  (((:as :word place) val)
   (h-app "#assign" (expand-value place env) (expand-value val env))))

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
  ((req-args opt-args rest-arg body)
   (let ((inner (scope env)))
     (h-app
      "#fn"
      (mapseq (req req-args)
        (unless (is-variable req)
          (syntax-error req "invalid binding in argument list"))
        (as-binding req inner :value))
      (mapseq (opt opt-args)
        (match opt
          (("=" (:as :word v) def)
           (h-app "=" (as-binding v inner :value) (expand-value def env)))
          (t (syntax-error opt "&opt arguments must be `(= word default)` forms"))))
      (cond ((is-variable rest-arg) (as-binding rest-arg inner :value))
            ((and (h-seq-p rest-arg) (not (h-seq-vals rest-arg))) (copy-h-seq rest-arg))
            (t (syntax-error rest-arg "&rest arguments must be either a word or []")))
      (expand-value body inner)))))

(define-special-form "#data" (env form)
  ((name variants)
   form
   :outer
   (let ((inner env))
     (h-app
      "#data"
      (match name
        (:word (as-binding name env :type))
        (((:as :word name) . args)
         (setf inner (scope env))
         (h-app* (as-binding name env :type) (loop :for arg :in args :collect (as-binding arg inner :type))))
        (t (syntax-error name "invalid name form in data declaration")))
      (mapseq (variant variants)
        (match variant
          (:word (as-binding (as-binding variant env :value t) env :pattern t))
          (((:as :word name) . args)
           (h-app* (as-binding (as-binding name env :value) env :pattern)
                   (loop :for arg :in args :collect (expand-type arg inner))))
          (t (syntax-error variant "invalid variant form"))))))))

(define-special-form "#class" (env form)
  ((name vars body)
   form
   :outer
   (let ((inner (scope env)))
     (h-app
      "#class"
      (as-binding name env :class)
      (mapseq (v vars) (as-binding v inner :type))
      (mapseq (form body)
        (match form
          (("type" (:as :word name) type)
           (h-app (h-app-head form) (as-binding name env :value)
                  (expand-type type inner)))
          (t (syntax-error form "only `type` forms may appear in a class body"))))))))

(define-special-form "#instance" (env form)
  ((bounds (:as :word class) types body)
   (h-app "#instance"
          (mapseq (bound bounds) (expand-bound bound env))
          (maybe-add-env class env)
          (mapseq (type types) (expand-type type env))
          (mapseq (form body)
            (match form
              (("def" (:as :word name) val) (h-app "def" (copy-h-word name) (expand-value val env)))
              (t (syntax-error form "only `def` forms may appear in an instance")))))))

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
     (unless elts (return-from expand-value-inner expr))
     (let ((can-expand (match (car elts)
                         (((:word name) . :_) (lookup env :value name :seq-macro))
                         (:_ nil))))
       (if can-expand
           (expand-value (funcall can-expand expr) env)
           (expand-value-seq elts env))))
    (:word (maybe-add-env expr env))
    (:lit (copy-h-lit expr))))

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
    (if (string= name "_")
        (copy-h-word expr)
        (progn
          (when (and check (find-binding env :value name))
            (syntax-error expr "binding ~a multiple times" name))
          (as-binding expr env :value)))))

(defun expand-pattern (expr outer-env inner-env)
  (match expr
    (:word
     (if (is-variable expr)
         (reg-binding expr inner-env)
         (expand-value expr outer-env)))
    (:lit (copy-h-lit expr))
    ;; FIXME pattern macros
    (((:as :word head) . args)
     (h-app* (ann-word head :env outer-env) (loop :for arg :in args :collect (expand-pattern arg outer-env inner-env))))
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
       (setf epats (if (cdr epats) (h-seq (nreverse epats)) (car epats)))
       (if test (h-app "#guard" test epats) epats)))
    (:_ (expand-pattern expr outer-env inner-env))))

;; Type expansion

(defun expand-bound (expr env)
  (match expr
    (((:as :word class) . args)
     (h-app* class (loop :for arg :in args :collect (expand-type arg env))))
    (t (syntax-error expr "invalid class name"))))

(defun expand-type (expr env)
  (labels ((expand (expr)
             (match expr
               (:word (maybe-add-env expr env))
               (:lit (syntax-error expr "found literal in type position"))
               (:seq (syntax-error expr "found sequence in type position"))
               (("#fn" req opt rest ret)
                (h-app (h-app-head expr)
                       (mapseq (arg req) (expand arg))
                       (mapseq (arg opt) (expand arg))
                       (match rest (:nil rest) (t (expand rest)))
                       (expand ret)))
               (((:as :word head) . args)
                (let ((mac (lookup env :type (h-word-name head) :macro)))
                  (if mac
                      (expand (funcall mac expr))
                      (h-app* (expand head) (mapcar #'expand args))))))))
    (expand expr)))

;; Tester

(defun test-expand (str &optional name)
  (expand-value (parse str name) (scope *top*)))
