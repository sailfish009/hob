(in-package :hob)

;; Transform

(defun transform-list (exprs f)
  (let ((changed :no))
    (dolist (expr exprs)
      (multiple-value-bind (applied splice) (funcall f expr)
        (when (and (eq changed :no) (not (eq applied expr)))
          (setf changed nil)
          (loop :for copy :in exprs :do
             (when (eq copy expr) (return))
             (push copy changed)))
        (unless (eq changed :no)
          (if splice
              (dolist (form applied) (push form changed))
              (push applied changed)))))
    (if (eq changed :no)
        exprs
        (nreverse changed))))

(defun transform-expr (expr f)
  (match expr
    ((:seq vals) (let ((trans (transform-list vals f)))
                   (if (eq trans vals) expr (h-seq trans))))
    ((head . args)
     (let ((trans-head (funcall f head))
           (trans-args (transform-list args f)))
       (if (and (eq trans-head head) (eq trans-args args))
           expr
           (h-app* trans-head trans-args))))
    (t expr)))

;; Walk

(defmacro walk (expr (var) &body cases)
  `(walk* ,expr (lambda (,var) (match ,var ,@cases (t)))))

(defun walk* (expr f)
  (unless (eq (funcall f expr) :handled)
    (match expr
      ((:seq forms) (dolist (form forms) (walk* form f)))
      ((head . args) (walk* head f) (dolist (arg args) (walk* arg f)))
      (t))))
