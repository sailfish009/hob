(in-package :hob)

(defstruct pos file start-line start-col end-line end-col)

(defun pos-less (a b)
  (let ((linea (pos-start-line a)) (lineb (pos-start-line b)))
    (or (< linea lineb) (and (= linea lineb) (< (pos-start-col a) (pos-start-col b))))))

(defun span-pos (start end)
  (make-pos :file (pos-file start)
            :start-line (pos-start-line start)
            :start-col (pos-start-col start)
            :end-line (pos-end-line end)
            :end-col (pos-end-col end)))

(defstruct h-expr pos) ;; FIXME save proper position info

(defstruct (h-lit (:include h-expr) (:constructor h-lit (val &optional pos))) val)

(defstruct (h-seq (:include h-expr) (:constructor mk-h-seq (vals &optional pos))) vals)
(defun h-seq (vals)
  (assert vals)
  (dolist (val vals) (assert (h-expr-p val)))
  (if (or (not vals) (cdr vals))
      (mk-h-seq vals)
      (car vals)))

(defvar *expanding* nil)

(defstruct (h-app (:include h-expr) (:constructor mk-h-app (head args))) head args)
(defun h-app* (head args)
  (if (stringp head)
      (setf head (h-word head (and *expanding* (expr-start-pos *expanding*))))
      (assert (h-expr-p head)))
  (dolist (arg args) (assert (h-expr-p arg)))
  (mk-h-app head args))
(defun h-app (head &rest args)
  (h-app* head args))

(defstruct (h-word (:include h-expr) (:constructor mk-h-word (name pos env))) name env)
(defun h-word (name &optional pos env)
  (assert (stringp name))
  (mk-h-word name pos env))

(defun expr-start-pos (expr)
  (or (h-expr-pos expr)
      (typecase expr
        (h-seq (let ((val0 (car (h-seq-vals expr))))
                 (and val0 (expr-start-pos val0))))
        (h-app (let ((head-pos (expr-pos (h-app-head expr)))
                     (arg0-pos (and (h-app-args expr) (expr-start-pos (car (h-app-args expr))))))
                 (if (and arg0-pos (or (not head-pos) (pos-less arg0-pos head-pos)))
                     arg0-pos
                     head-pos))))))

(defun expr-end-pos (expr)
  (or (h-expr-pos expr)
      (typecase expr
        (h-seq (let ((valn (car (last (h-seq-vals expr)))))
                 (and valn (expr-end-pos valn))))
        (h-app (if (h-app-args expr)
                   (expr-end-pos (car (last (h-app-args expr))))
                   (expr-end-pos (h-app-head expr)))))))

(defun expr-pos (expr)
  (let ((start (expr-start-pos expr))
        (end (expr-end-pos expr)))
    (cond ((or (not end) (eq start end)) start)
          ((not start) end)
          (t (span-pos start end)))))

(defun seq-len (s)
  (if (h-seq-p s) (length (h-seq-vals s)) 1))
(defun seq-list (s)
  (if (h-seq-p s) (h-seq-vals s) (list s)))
(defmacro mapseq ((var seq) &body body)
  (let ((b (gensym)) (s (gensym)))
    `(flet ((,b (,var) ,@body))
       (let ((,s ,seq))
         (if (h-seq-p ,s)
             (h-seq (loop :for ,var :in (h-seq-vals ,s) :collect (,b ,var)))
             (,b ,s))))))
(defmacro lmapseq ((pat seq) &body body)
  (let ((b (gensym)) (s (gensym)) (a (gensym)))
    `(flet ((,b (,a) (match ,a (,pat ,@body))))
       (let ((,s ,seq))
         (if (h-seq-p ,s)
             (loop :for ,a :in (h-seq-vals ,s) :collect (,b ,a))
             (list (,b ,s)))))))
(defmacro doseq ((pat seq) &body body)
  (let ((b (gensym)) (s (gensym)) (v (gensym)))
    `(block nil
       (flet ((,b (,v) (match ,v (,pat ,@body))))
         (let ((,s ,seq))
           (if (h-seq-p ,s)
               (loop :for ,v :in (h-seq-vals ,s) :do (,b ,v))
               (,b ,s)))))))

(defun is-const (w)
  (or (char= (schar w 0) #\$)
      (equal w "()"))) ;; FIXME other non-alphabetics?
(defun is-variable (e)
  (and (h-word-p e) (not (is-const (h-word-name e)))))

(defmethod print-object ((e h-lit) out)
  (let ((v (h-lit-val e)))
    (etypecase v
      (string (write-escaped-string v out))
      (cons (write-string (car v) out) (write-escaped-string (cdr v) out))
      (character (write-char #\\ out) (write-escaped-char v out nil))
      (integer (write v :stream out))
      (real (format out "~,,,,,,'eE" v)))))

(defun write-escaped-string (str out)
  (write-char #\" out)
  (loop :for ch :across str :do
     (write-escaped-char ch out #\"))
  (write-char #\" out))

(defun write-escaped-char (ch out quote)
  (let ((escd (case ch
                (#\newline #\n) (#\return #\r) (#\tab #\t)
                (#\backspace #\b) (#\null #\0) (#\\ #\\) (t nil))))
    (when (eq ch quote) (setf escd ch))
    (if escd
        (progn (write-char #\\ out) (write-char escd out))
        (let ((code (char-code ch)))
          (cond ((> code #xffff) (format out "\\U~8,'0x" code))
                ((> code #xff) (format out "\\u~4,'0x" code))
                ((> code 127) (format out "\\x~2,'0x" code))
                (t (write-char ch out)))))))

(defmethod print-object ((e h-seq) *standard-output*)
  (pprint-logical-block (nil nil :prefix "[" :suffix "]")
    (let ((first t))
      (dolist (elt (h-seq-vals e))
        (if first
            (setf first nil)
            (progn (write-char #\Space) (pprint-newline :linear)))
        (write elt)))))

(defmethod print-object ((e h-app) *standard-output*)
  (pprint-logical-block (nil nil :prefix "(" :suffix ")")
    (write (h-app-head e))
    (dolist (elt (h-app-args e))
      (write-char #\Space)
      (pprint-newline :fill)
      (write elt))))

(defmethod print-object ((e h-word) out)
  (let ((nm (h-word-name e)))
    (if (or (= (length nm) 0)
            (and (not (every #'is-word-char nm))
                 (not (member nm '("::" "()") :test #'string=))))
        (progn
          (write-char #\` out)
          (loop :for ch :across nm :do
             (write-escaped-char ch out #\`))
          (write-char #\` out))
        (write-string nm out))))

(defun dissect-improper-list (lst)
  (let (main end)
    (loop :while lst :do
       (unless (consp lst) (return (setf end lst)))
       (push (car lst) main)
       (setf lst (cdr lst)))
    (values (nreverse main) end)))

(defvar *pat-bound*)

(defun compile-match-pat (pat in)
  (cond ((or (eq pat :_) (eq pat t)) t)
        ((eq pat :word) `(h-word-p ,in))
        ((eq pat :lit) `(h-lit-p ,in))
        ((eq pat :seq) `(h-seq-p ,in))
        ((stringp pat) `(and (h-word-p ,in) (equal (h-word-name ,in) ,pat)))
        ((symbolp pat)
         (push pat *pat-bound*)
         `(prog1 t (setf ,pat ,in)))
        ((consp pat)
         (case (car pat)
           (:seq (push (second pat) *pat-bound*)
                 `(and (h-seq-p ,in)
                       (prog1 t (setf ,(second pat) (if (h-seq-p ,in) (h-seq-vals ,in) (list ,in))))))
           (:as (push (third pat) *pat-bound*)
                `(progn (setf ,(third pat) ,in)
                        ,(compile-match-pat (second pat) in)))
           (:word (push (second pat) *pat-bound*)
                  `(when (h-word-p ,in)
                     (setf ,(second pat) (h-word-name ,in))
                     t))
           (t (let ((sym (gensym)))
                `(and (h-app-p ,in)
                      (let ((,sym (h-app-head ,in)))
                        (declare (ignorable ,sym))
                        ,(compile-match-pat (car pat) sym))
                      (let ((,sym (h-app-args ,in)))
                        (declare (ignorable ,sym))
                        ,(compile-match-pats (cdr pat) sym)))))))
        (t (error "Unrecognized pattern: ~a" pat))))

(defun compile-match-pats (pats in)
  (when (eq pats t) (return-from compile-match-pats t))
  (multiple-value-bind (args rest) (dissect-improper-list pats)
    `(and (,(if rest '>= '=) (length ,in) ,(length args))
          ,@(loop :for i :from 0 :for arg :in args :for sym := (gensym) :collect
               `(let ((,sym (nth ,i ,in)))
                  (declare (ignorable ,sym))
                  ,(compile-match-pat arg sym)))
          ,@(when (and rest (not (eq rest :_)))
                  (push rest *pat-bound*)
                  `((prog1 t (setf ,rest (nthcdr ,(length args) ,in))))))))

(defmacro match (value &body clauses)
  (when (and (not (cdr clauses)) (symbolp (caar clauses)) (not (keywordp (caar clauses))))
    (return-from match `(let ((,(caar clauses) ,value)) ,@(cdar clauses))))
  (let ((v (gensym)))
    `(let ((,v ,value))
       (declare (ignorable ,v))
       (block match
         ,@(loop :for (pat . body) :in clauses :collect
                  (let* ((*pat-bound* ())
                         (test (compile-match-pat pat v)))
                    `(let ,*pat-bound*
                       (when ,test (return-from match (progn ,@body))))))
         (error "Non-exhaustive pattern")))))

(defmacro match* (values &body clauses)
  (let ((v (gensym)))
    `(let ((,v ,values))
       (declare (ignorable ,v))
       (block match*
         ,@(loop :for (pats . body) :in clauses :collect
              (let* ((*pat-bound* ())
                     (test (compile-match-pats pats v)))
                `(let ,*pat-bound*
                   (when ,test (return-from match* (progn ,@body))))))
         (error "Non-exhaustive pattern")))))
