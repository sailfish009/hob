(in-package :hob)

;; Update

(defun update-expr (expr f)
  (match expr
    ((:seq vals) (update-list vals f t))
    ((head . args)
     (let ((new-head (funcall f head)))
       (unless (eq head new-head) (setf (h-app-head expr) new-head)))
     (update-list args f))
    (t))
  expr)

(defun update-list (exprs f &optional allow-splice)
  (loop :for cons :on exprs :do
     (multiple-value-bind (new-expr splice) (funcall f (car cons))
       (unless (eq new-expr (car cons))
         (if (and splice allow-splice)
             (let ((list (h-seq-vals new-expr)))
               (setf (car cons) (car list)
                     (cdr cons) (append (cdr list) (cdr cons))
                     cons (nthcdr (1- (length list)) (cdr cons))))
             (setf (car cons) new-expr))))))

;; Walk

(defmacro walk (expr (var) &body cases)
  `(walk* ,expr (lambda (,var) (match ,var ,@cases (t :unhandled)))))

(defun walk* (expr f)
  (when (eq (funcall f expr) :unhandled)
    (match expr
      ((:seq forms) (dolist (form forms) (walk* form f)))
      ((head . args) (walk* head f) (dolist (arg args) (walk* arg f)))
      (t))))

(defun walk-pat-vars (pat f)
  (match pat
    (:word (when (is-variable pat) (funcall f pat)))
    ((:_ . args) (dolist (arg args) (walk-pat-vars arg f)))
    (:_)))

(defmacro do-pat-vars ((v pat) &body body)
  `(walk-pat-vars ,pat (lambda (,v) ,@body)))
