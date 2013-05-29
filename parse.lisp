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
         (in (make-tstream :filename filename :file file)))
    (next-token in)
    (when (> (margin in) 0)
      (hob-token-error in "code must start at column 0"))
    (prog1 (parse-block in) (expect in :eof))))

(defun margin (in) (tstream-tok-start-col in))
(defun indent (in) (tstream-indentation in))

(defun outdents (in margin)
  (and (tstream-tok-new-line in) (< (tstream-tok-start-col in) margin)))

(defun must-be-indented (in &optional atleast)
  (when (and (tstream-tok-new-line in)
             (< (tstream-tok-start-col in) (if atleast (indent in) (1+ (indent in)))))
    (hob-token-error in "token ~a is not indented enough" (token-id in))))
  
(defun ends (in margin &optional (count-arrow t))
  (or (tok= in :eof)
      (and (tok= in :punc) (or (search (token-value in) ")]};,")
                               (and count-arrow (is-arrow (token-value in)))))
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

(defun token-pos (in &optional sline scol eline ecol)
  (make-pos :file (tstream-filename in)
            :start-line (or sline (tstream-tok-start-line in))
            :start-col (or scol (tstream-tok-start-col in))
            :end-line (or eline (tstream-tok-end-line in))
            :end-col (or ecol (tstream-tok-end-col in))))

(defun token-word (in)
  (h-word (string (token-value in)) (token-pos in)))

(defun parse-block (in)
  (push (indent in) (tstream-indentation-stack in))
  (let* ((margin (margin in))
         (first (parse-tup-expr in (1+ margin))))
    (let ((rest (loop :until (ends in margin) :collect (parse-tup-expr in (1+ margin)))))
      (setf (tstream-indentation in) (pop (tstream-indentation-stack in)))
      (if rest (h-seq (cons first rest)) first))))

(defun parse-tup-expr (in margin)
  (let ((first (parse-op-expr in margin)))
    (if (and (tok= in :punc ",") (not (outdents in margin)))
        (let ((commapos (token-pos in)))
          (next-token in)
          (progn
            (must-be-indented in t)
            (let* ((rest (loop :collect (parse-op-expr in margin) :while (eat in :punc "," margin)))
                   (name (format nil "~{~a~}" (loop :repeat (length rest) :collect #\,))))
              (h-app* (h-word name commapos) (cons first rest)))))
        first)))

(defun parse-op-expr (in margin)
  (if (tok= in :op)
      (let ((op (token-word in)))
        (next-token in)
        (parse-app-expr in op margin t))
      (let ((head (parse-subscript-expr in)))
        (if (tok= in :op)
            (let* ((op (token-word in))
                   (rest (loop :while (eat in :op (h-word-name op) margin)
                            :do (when (ends in margin)
                                  (if exprs
                                      (hob-token-error in "unfinished operator application")
                                      (return)))
                            :collect (parse-subscript-expr in) :into exprs
                            :finally (return exprs))))
              (unless (ends in margin) (hob-token-error in "unexpected continuation after operator application"))
              (h-app* op (cons head rest)))
            (parse-app-expr in head margin)))))

(defun parse-app-expr (in head margin &optional no-arrow)
  (let* ((blocks nil)
         (tail (loop :for i :from 0 :until (ends in margin) :do 
                  (when (and (not blocks) (eat in :punc ":" margin)) (setf blocks t) (when (ends in margin nil) (return)))
                  :collect (if blocks
                               (progn (must-be-indented in) (parse-block in))
                               (parse-subscript-expr in))))
         (val (token-value in)))
    (cond ((and (not no-arrow)
                (not blocks)
                (tok= in :punc)
                (is-arrow val)
                (not (outdents in margin)))
           (let ((arrow (token-word in)))
             (next-token in)
             (must-be-indented in)
             (h-app arrow
                    (if tail (h-seq (cons head tail)) head)
                    (parse-block in))))
          ((or tail blocks) (h-app* head tail))
          (t head))))

(defun parse-subscript-expr (in)
  (let ((expr (parse-base-expr in)))
    (loop
       (cond ((and (tok= in :punc ".") (not (outdents in (indent in))))
              (let ((dot (token-word in)))
                (next-token in)
                (must-be-indented in)
                (setf expr (h-app dot expr (parse-base-expr in)))))
             (t (return expr))))))

(defun in-brackets* (in close body)
  (push (indent in) (tstream-indentation-stack in))
  (next-token in)
  (must-be-indented in)
  (prog1 (funcall body)
    (setf (tstream-indentation in) (pop (tstream-indentation-stack in)))
    (expect in :punc close (indent in))))

(defmacro in-brackets (in close &body body)
  `(in-brackets* ,in ,close (lambda () ,@body)))

(defun bracketed-block (in end)
  (let (is-seq elts)
    (loop
       (when (tok= in :punc end) (return))
       (push (parse-block in) elts)
       (unless (eat in :punc ";") (return))
       (setf is-seq t))
    (if (or (not elts) (cdr elts) is-seq) (h-seq (nreverse elts)) (car elts))))

(defun ends-with (str suffix)
  (let ((start (- (length str) (length suffix))))
    (and (>= start 0) (search suffix str :start2 start))))

(defun parse-base-expr (in)
  (case (token-type in)
    ((:num :string :char) (prog1 (h-lit (token-value in) (token-pos in)) (next-token in)))
    (:word (prog1 (token-word in) (next-token in)))
    (:punc 
     (let ((start-line (tstream-tok-start-line in))
           (start-col (tstream-tok-start-col in))
           (val (token-value in)))
       (cond ((string= val "(")
              (in-brackets in ")"
                (cond ((tok= in :op)
                       (let ((op (token-word in)))
                         (next-token in)
                         (if (tok= in :punc ")")
                             op
                             (parse-app-expr in op 0 t))))
                      (t (bracketed-block in ")")))))
             ((ends-with val "{")
              (in-brackets in "}"
                (let ((name (h-word (concatenate 'string val "}") (token-pos in start-line start-col))))
                  (h-app name (bracketed-block in "}")))))
             ((ends-with val "[")
              (in-brackets in "]"
                (let ((name (h-word (concatenate 'string val "]") (token-pos in start-line start-col))))
                  (h-app name (bracketed-block in "]")))))
             ((is-arrow val)
              (h-app (h-word val (token-pos in start-line start-col))
                     (h-nil)
                     (progn (next-token in) (parse-block in))))
             (t (unexpected in)))))
     (t (unexpected in))))
