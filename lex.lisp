(in-package :hob)

(defstruct tstream
  filename
  file
  (pos 0)
  (line 1)
  (start-of-line 0)
  (indentation 0)
  (indentation-stack '(0))

  tok-start-col
  tok-start-line
  tok-end-col
  tok-end-line
  tok-type
  tok-value
  tok-new-line)

(defun token-value (in) (tstream-tok-value in))
(defun token-type (in) (tstream-tok-type in))

(defun next-token (in)
  (prog1 (token-value in) (read-token in)))

(defun token-id (in)
  (token-id* (token-type in) (token-value in)))

(defun token-id* (type val)
  (ecase type
    ((:op :punc) (string val))
    (:string (with-output-to-string (out) (write-escaped-string val out)))
    (:eof "EOF")
    (:num (format nil "~d" val))
    (:word val)))

(defun tok= (in type &optional val)
  (and (eq (token-type in) type)
       (or (not val) (equal (token-value in) val))))

(let ((decimal-number (cl-ppcre:create-scanner "^(-)?([\\d_]*)(?:\\.(\\d+))?(?:[eE](-?\\d+))?$"))
      (as-int (lambda (s) (if (length s) (parse-integer s) 0))))
  (defun parse-number (string)
    (unless (zerop (length string))
      (when (equal string "-") (return-from parse-number nil))
      (multiple-value-bind (match ms) (cl-ppcre:scan-to-strings decimal-number string)
        (when match
          (let ((neg (elt ms 0)) (base (elt ms 1)) (frac (elt ms 2)) (exp (elt ms 3)))
            (unless (or (and (not (equal base "")) (eql (char base 0) #\_))
                        (and exp (not base) (not frac)))
              (let ((num (funcall as-int (remove #\_ base))))
                (when frac
                  (incf num (/ (coerce (funcall as-int frac) 'double-float)
                               (expt 10 (length frac)))))
                (when exp
                  (setf num (* (coerce num 'double-float) (expt 10 (funcall as-int exp)))))
                (if neg (- num) num)))))))))

(defparameter *punctuation-chars* "{}()[];,.")
(defparameter *operator-chars* "~+-=/%$&|*^@<>!?")
(defun is-word-char (ch) (or (alphanumericp ch) (find ch "'_") (find ch *operator-chars*)))
(defun is-arrow (word) (find word '("->" "=>" "=") :test 'string=))

(defun cur-ch (in)
  (let ((file (tstream-file in))
        (pos (tstream-pos in)))
    (when (< pos (length file)) (schar file pos))))

(defun next (in)
  (let ((file (tstream-file in))
        (pos (tstream-pos in)))
    (when (< pos (length file))
      (when (eql (schar file pos) #\newline)
        (incf (tstream-line in))
        (setf (tstream-start-of-line in) (1+ pos)))
      (setf (tstream-pos in) (incf pos))
      (when (< pos (length file)) (schar file pos)))))

(defun skip-non-tokens (in)
  (loop :with new-line := nil :do
     (loop (case (cur-ch in)
             (#\newline (setf new-line t) (next in))
             (#\space (next in))
             (t (return))))
     (unless (eql (cur-ch in) #\#) (return new-line))
       (next in)
       (if (eql (cur-ch in) #\()
           (let ((last nil) (depth 1) (start (tstream-pos in)))
             (loop :for next := (cur-ch in) :do
                (unless next (hob-stream-error in start "Unterminated block comment"))
                (cond ((and (eql last #\#) (eql next #\()) (incf depth))
                      ((and (eql last #\)) (eql next #\#))
                       (when (zerop (decf depth)) (next in) (return))))
                (setf last next)
                (next in)))
           (loop :until (member (cur-ch in) '(nil #\newline)) :do (next in)))))

(defparameter *num-start* (cl-ppcre:create-scanner "^-?\\d*$"))

(defun is-operator (str)
  (every (lambda (ch) (find ch *operator-chars*)) str))

(defun read-token (in)
  (when (tstream-tok-new-line in)
    (setf (tstream-indentation in) (tstream-tok-start-col in)))
  (setf (tstream-tok-new-line in) (skip-non-tokens in)
        (tstream-tok-start-line in) (tstream-line in)
        (tstream-tok-start-col in) (- (tstream-pos in) (tstream-start-of-line in)))
  (let ((ch (cur-ch in))
        (start (tstream-pos in)))
    (next in)
    (multiple-value-bind (type val)
        (cond
          ((not ch) (values :eof t))
          ((eql ch #\") (values :string (read-string in #\")))
          ((eql ch #\`) (values :word (read-string in #\`)))
          ((eql ch #\:)
           (cond ((eql (cur-ch in) #\:) (next in) (values :op "::"))
                 (t (values :punc #\:))))
          ((eql ch #\.)
           (let* ((after (and (digit-char-p (cur-ch in)) (read-word in ch)))
                  (num (and after (parse-number after))))
             (if num
                 (values :num num)
                 (progn (setf (tstream-pos in) (1+ start)) (values :punc #\.)))))
          ((find ch *punctuation-chars*) (values :punc ch))
          ((is-word-char ch)
           (let ((word (read-word in ch)) num)
             (if (and (eql (cur-ch in) #\.) (cl-ppcre:scan *num-start* word))
                 (let ((end (tstream-pos in))
                       (whole (progn (next in) (concatenate 'string word "." (read-word in nil)))))
                   (setf num (parse-number whole))
                   (unless num (setf (tstream-pos in) end)))
                 (setf num (parse-number word)))
             (cond (num (values :num num))
                   ((is-arrow word) (values :punc word))
                   ((is-operator word) (values :op word))
                   (t (values :word word)))))
          (t (hob-stream-error in start "unexpected character: \\~a" ch)))
      (setf (tstream-tok-type in) type
            (tstream-tok-value in) val
            (tstream-tok-end-line in) (tstream-line in)
            (tstream-tok-end-col in) (- (tstream-pos in) (tstream-start-of-line in))))))

(defun read-string (in quote)
  (let ((start (tstream-pos in)))
    (with-output-to-string (out)
      (loop (let ((ch (cur-ch in)))
              (next in)
              (cond ((not ch) (hob-stream-error in start "Unterminated string constant"))
                    ((eql ch #\\) (write-char (read-escaped-char in) out))
                    ((eql ch quote) (return))
                    (t (write-char ch out))))))))

(defun read-hex-bytes (in n)
  (let ((num 0))
    (dotimes (i n)
      (setf num (ash num 4))
      (let ((digit (digit-char-p (or (next in) #\*) 16)))
        (if digit
            (incf num digit)
            (hob-stream-error in (tstream-pos in)
                              "Invalid hex-character pattern in string."))))
    num))

(defun read-escaped-char (in)
  (let ((ch (next in)))
    (case ch
      ((nil) (hob-stream-error in (tstream-pos in) "Invalid character escape"))
      (#\n #\newline) (#\r #\return) (#\t #\tab)
      (#\b #\backspace) (#\0 #\null)
      (#\x (code-char (read-hex-bytes in 2)))
      (#\u (code-char (read-hex-bytes in 4)))
      (#\U (code-char (read-hex-bytes in 8)))
      (t ch))))

(defun read-word (in start)
  (with-output-to-string (out)
    (when start (write-char start out))
    (loop :for cur := (cur-ch in)
          :while (and cur (is-word-char cur)) :do
       (write-char cur out)
       (next in))))
