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
  `(walk* ,expr (lambda (,var) (match ,var ,@cases (t)))))

(defun walk* (expr f)
  (unless (eq (funcall f expr) :handled)
    (match expr
      ((:seq forms) (dolist (form forms) (walk* form f)))
      ((head . args) (walk* head f) (dolist (arg args) (walk* arg f)))
      (t))))
