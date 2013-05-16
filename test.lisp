(in-package :hob)

(defparameter *path* (asdf:component-pathname (asdf:find-component nil :hob)))

(defun return-spec (file)
  (let ((line (with-open-file (file file) (read-line file))))
    (and (> (length line) 5) (string= (subseq line 0 5) "# -> ")
         (if (eql (char line 5) #\!)
             (list :error (subseq line 7))
             (parse-integer (subseq line 5))))))

(define-condition test-failure (simple-error) ())

(defun print-if (name noisy val)
  (cond ((eq noisy t) (format t "~a ~a" name val))
        ((eq noisy name) (print val)))
  val)

(defun test-compile (file &optional name noisy)
  (with-context ()
    (let ((ast (print-if :expand noisy
                         (expand-value
                          (print-if :parse noisy (parse file name)) (scope *top*)))))
      (print-if :type noisy (typecheck ast))
      (setf ast (print-if :match noisy (expand-matches ast)))
      (verify-use-order ast)
      (hcompile ast))))

(defun run-test (file &optional noisy)
  (with-context ()
    (when (stringp file)
      (setf file (merge-pathnames (concatenate 'string "test/" file ".hob") *path*)))
    (with-open-file (in file :element-type '(unsigned-byte 8))
      (let* ((expr (print-if :compile noisy (test-compile in (pathname-name file) noisy))))
        (print-if :result noisy (funcall (compile-s-expr expr)))))))

(defun run-test* (file)
  (let ((expect (return-spec file))
        (val :no))
    (handler-case (setf val (run-test file))
      (error (e)
        (if (consp expect)
            (unless (search (second expect) (princ-to-string e))
              (error "expected error \"~a\", but got \"~a\"" (second expect) e))
            (error e))))
    (if (consp expect)
        (if (not (eq val :no))
            (error "expected error \"~a\" but returned ~a instead" (second expect) val)
            (second expect))
        (if (equal expect val)
            expect
            (error "returned ~a instead of expected ~a" val expect)))))

(defun run-testsuite ()
  (let ((ok t))
    (dolist (file (directory (merge-pathnames "test/*.hob" *path*)) ok)
      (let ((name (pathname-name file)))
        (handler-case (let ((res (run-test* file)))
                        (if res
                            (format t "   '~a' returned ~s as expected.~%" name res)
                            (format t "   '~a' ran successfully.~%" name)))
          (error (e) (format t "!! '~a' ~a~%" name e) (setf ok nil)))))))
