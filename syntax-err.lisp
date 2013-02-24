(in-package :hob)

(defparameter *current-filename* nil)
(defparameter *current-file* nil)

(defun find-pos (str pos)
  (loop :with off := 0 :for l :from 1 :do
     (let ((next (position #\newline str :start off)))
       (when (or (not next) (> next pos))
         (return (values l (- pos off))))
       (setf off (1+ next)))))

(define-condition hob-program-error (simple-error)
  ((line :initarg :line :reader hob-error-line)
   (col :initarg :col :reader hob-error-col)
   (file :initarg :file :reader hob-error-file)))

(defmethod print-object ((err hob-program-error) stream)
  (call-next-method)
  (unless (zerop (hob-error-line err))
    (let ((f (hob-error-file err)))
      (format stream " (~@[~a ~]line ~a, char ~a)" (and f (file-namestring f))
              (hob-error-line err) (hob-error-col err)))))

(define-condition hob-syntax-error (hob-program-error) ())

(define-condition hob-parse-error (hob-syntax-error) ())

(defun hob-parse-error (line col control &rest args)
  (error 'hob-parse-error :format-control control :format-arguments args
         :line line :col col :file *current-filename*))
(defun hob-stream-error (in pos control &rest args)
  (multiple-value-bind (line col) (find-pos (tstream-file in) pos)
    (apply #'hob-parse-error line col control args)))
(defun hob-token-error (in control &rest args)
  (apply #'hob-parse-error (tstream-tok-start-line in) (tstream-tok-start-col in)
         control args))