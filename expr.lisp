(in-package :hob)

(defstruct h-expr start end)

(defstruct (h-lit (:include h-expr) (:constructor h-lit (val))) val)

(defstruct (h-seq (:include h-expr) (:constructor mk-h-seq (vals))) vals)
(defun h-seq (vals)
  (dolist (val vals) (assert (h-expr-p val)))
  (if (or (not vals) (cdr vals))
      (mk-h-seq vals)
      (car vals)))
(defun h-nil () (mk-h-seq ()))

(defstruct (h-app (:include h-expr) (:constructor mk-h-app (head args))) head args)
(defun h-app* (head args)
  (if (stringp head)
      (setf head (h-word head))
      (assert (h-expr-p head)))
  (dolist (arg args) (assert (h-expr-p arg)))
  (mk-h-app head args))
(defun h-app (head &rest args)
  (h-app* head args))

(defstruct (h-word (:include h-expr) (:constructor mk-h-word (name))) name)
(defun h-word (name)
  (assert (stringp name))
  (mk-h-word name))

(defmethod print-object ((e h-lit) out)
  (let ((v (h-lit-val e)))
    (etypecase v
      (string (write-escaped-string v out))
      (cons (write-string (car v) out) (write-escaped-string (cdr v) out))
      (character (write-char #\\ out) (write-escaped-char v out))
      (integer (write v :stream out))
      (real (format out "~,,,,,,'eE" v)))))

(defun write-escaped-string (str out)
  (write-char #\" out)
  (loop :for ch :across str :do
     (write-escaped-char ch out))
  (write-char #\" out))

(defun write-escaped-char (ch out)
  (let ((escd (case ch
                (#\newline #\n) (#\return #\r) (#\tab #\t)
                (#\backspace #\b) (#\null #\0) (#\\ #\\) (t nil))))
    (if escd
        (progn (write-char #\\ out) (write-char escd out))
        (let ((code (char-code ch)))
          (cond ((> code #xffff) (format out "\\U~8,'0x" code))
                ((> code #xff) (format out "\\u~4,'0x" code))
                ((> code 127) (format out "\\x~2,'0x" code))
                (t (write-char ch out)))))))

(defmethod print-object ((e h-seq) *standard-output*)
  (if (h-seq-vals e)
      (pprint-logical-block (nil nil :prefix "[" :suffix "]")
        (let ((first t))
          (dolist (elt (h-seq-vals e))
            (if first
                (setf first nil)
                (progn (write-char #\Space) (pprint-newline :linear)))
            (write elt))))
      (write-string "()")))

(defmethod print-object ((e h-app) *standard-output*)
  (pprint-logical-block (nil nil :prefix "(" :suffix ")")
    (write (h-app-head e))
    (dolist (elt (h-app-args e))
      (write-char #\Space)
      (pprint-newline :fill)
      (write elt))))

(defmethod print-object ((e h-word) out)
  (let ((nm (h-word-name e)))
    (if (and (not (every #'is-word-char nm))
             (not (equal nm "::")))
        (progn
          (write-char #\\ out)
          (write-escaped-string nm out))
        (write-string nm out))))

(defun span (expr start end)
  (setf (h-expr-start expr) start
        (h-expr-end expr) end))
