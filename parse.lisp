(in-package :hob)

(defun parse (input &optional (filename "input"))
  (let* ((file (etypecase input
                 (string input)
                 (stream (let ((buf (make-array (file-length input) :element-type '(unsigned-byte 8))))
                           (read-sequence buf input)
                           (#+sbcl sb-ext:octets-to-string
                            #+allegro excl:octets-to-string
                            #-(or allegro sbcl)(error "Sorry, didn't port to your implementation's octets-to-string yet")
                            buf :external-format :utf8)))))
         (in (make-tstream :filename filename :file file))
         (*current-filename* filename)
         (*current-file* file))
    (next-token in)
    (when (> (margin in) 0)
      (hob-token-error in "code must start at column 0"))
    (prog1 (parse-block in) (expect in :eof))))

(defun margin (in) (tstream-tok-start-col in))
(defun indent (in) (tstream-indentation in))

(defun outdents (in margin)
  (and (tstream-tok-new-line in) (< (tstream-tok-start-col in) margin)))

(defun must-be-indented (in &optional atleast)
  (when (and (tstream-tok-new-line in) (< (tstream-tok-start-col in) (if atleast (indent in) (1+ (indent in)))))
    (hob-token-error in "token ~a is not indented enough" (token-id in))))
  
(defun ends (in margin)
  (or (tok= in :eof)
      (and (tok= in :punc) (or (find (token-value in) ")]};,")
                               (is-arrow (token-value in))))
      (outdents in margin)))

(defun expect (in type &optional val margin)
  (unless (eat in type val margin)
    (if (and margin (tok= in type val))
        (hob-token-error in "token ~a is not indented enough" (token-id in))
        (hob-token-error in "unexpected ~a, expected ~a" (token-id in) (token-id* type val)))))

(defun eat (in type &optional val margin)
  (and (tok= in type val)
       (not (and margin (outdents in margin)))
       (next-token in)))

(defun unexpected (in)
  (hob-token-error in "unexpected ~a" (token-id in)))

(defun parse-block (in)
  (let ((margin (margin in)) first)
    (setf first (parse-tup-expr in (1+ margin)))
    (if (eat in :punc #\; margin)
        (progn
          (must-be-indented in t)
          (prog1 (h-seq (cons first (loop :collect (parse-tup-expr in (1+ margin))
                                       :while (eat in :punc #\; margin))))
            (unless (ends in margin) (hob-token-error in "mixing semicolons and indentation in block"))))
        (let ((rest (loop :until (ends in margin) :collect (parse-tup-expr in (1+ margin)))))
          (if rest (h-seq (cons first rest)) first)))))


(defun parse-tup-expr (in margin)
  (let ((first (parse-op-expr in margin)))
    (if (eat in :punc #\, margin)
        (progn
          (must-be-indented in t)
          (let* ((rest (loop :collect (parse-op-expr in margin) :while (eat in :punc #\, margin)))
                 (name (format nil "~{~a~}" (loop :repeat (length rest) :collect #\,))))
            (h-app* (h-word name) (cons first rest))))
        first)))

(defun parse-op-expr (in margin)
  (let ((prefix (eat in :op)))
    (if prefix
        (parse-app-expr in (h-word prefix) margin t)
        (let ((head (parse-subscript-expr in)))
          (if (tok= in :op)
              (let* ((op (token-value in))
                     (rest (loop :while (eat in :op op margin)
                              :do (when (ends in margin) (hob-token-error in "unfinished operator application"))
                              :collect (parse-subscript-expr in))))
                (unless (ends in margin) (hob-token-error in "unexpected continuation after operator application"))
                (h-app* (h-word op) (cons head rest)))
              (parse-app-expr in head margin))))))

(defun parse-app-expr (in head margin &optional no-arrow)
  (let ((tail (loop :for i :from 0 :until (ends in margin) :collect
                 (if (eat in :punc #\:)
                     (progn (must-be-indented in) (parse-block in))
                     (parse-subscript-expr in))))
        (val (token-value in)))
    (cond ((and (not no-arrow)
                (tok= in :punc)
                (is-arrow val)
                (not (outdents in margin)))
           (next-token in)
           (must-be-indented in)
           (h-app (h-word val)
                  (if tail (h-seq (cons head tail)) head)
                  (parse-block in)))
          (tail (h-app* head tail))
          (t head))))

(defun parse-subscript-expr (in)
  (let ((expr (parse-base-expr in)))
    (loop
       (cond ((eat in :punc #\. (indent in))
              (must-be-indented in)
              (setf expr (h-app (h-word ".") expr (parse-base-expr in))))
             (t (return expr))))))

(defmacro in-brackets (in close &body body)
  (let ((i (gensym)))
    `(let ((,i ,in))
       (push (indent ,i) (tstream-indentation-stack ,i))
       (next-token ,i)
       (must-be-indented ,i)
       (prog1 (progn ,@body)
         (setf (tstream-indentation ,i) (pop (tstream-indentation-stack ,i)))
         (expect ,i :punc ,close (indent ,i))))))

(defun parse-base-expr (in)
  (case (token-type in)
    ((:num :string :char) (h-lit (next-token in)))
    (:word (h-word (next-token in)))
    (:punc (case (token-value in)
             (#\( (in-brackets in #\)
                    (cond ((tok= in :punc #\)) (h-seq nil))
                          ((tok= in :op)
                           (let ((op (h-word (next-token in))))
                             (if (tok= in :punc #\))
                                 op
                                 (parse-app-expr in op 0 t))))
                          (t (parse-block in)))))
             (#\{ (in-brackets in #\}
                    (if (tok= in :punc #\})
                        (h-app "{}")
                        (h-app "{}" (parse-block in)))))
             (#\[ (in-brackets in #\]
                    (if (tok= in :punc #\])
                        (h-app "[]")
                        (h-app "[]" (parse-block in)))))
             (t (unexpected in))))
    (t (unexpected in))))
