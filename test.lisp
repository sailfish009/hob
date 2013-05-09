(in-package :hob)

(defparameter *path* (asdf:component-pathname (asdf:find-component nil :hob)))

(defun return-spec (file)
  (let ((line (with-open-file (file file) (read-line file))))
    (and (> (length line) 5) (string= (subseq line 0 5) "# -> ")
         (parse-integer (subseq line 5)))))

(define-condition test-failure (simple-error) ())

(defun test-compile (file &optional name)
  (with-context ()
    (let ((ast (test-expand file name)))
      (typecheck ast)
      (expand-matches ast))))

(defun run-test (file)
  (with-context ()
    (when (stringp file)
      (setf file (merge-pathnames (concatenate 'string "test/" file ".hob") *path*)))
    (with-open-file (in file :element-type '(unsigned-byte 8))
      (test-compile in (pathname-name file)))
    (return-spec file)))

(defun run-testsuite ()
  (let ((ok t))
    (dolist (file (directory (merge-pathnames "test/*.hob" *path*)) ok)
      (let ((name (pathname-name file)))
        (handler-case (let ((res (run-test file)))
                        (if res
                            (format t "   '~a' returned ~a as expected.~%" name res)
                            (format t "   '~a' ran successfully.~%" name)))
          (error (e) (format t "!! '~a' ~a~%" name e) (setf ok nil)))))))
