(in-package :hob)

(defparameter *path* (asdf:component-pathname (asdf:find-component nil :hob)))

(defun return-spec (file)
  (let ((line (with-open-file (file file) (read-line file))))
    (and (> (length line) 5) (string= (subseq line 0 5) "# -> ")
         (parse-integer (subseq line 5)))))

(define-condition test-failure (simple-error) ())

(defun print-if (name noisy val)
  (cond ((eq noisy t) (format t "~a ~a" name val))
        ((eq noisy name) (print val)))
  val)

(defun test-compile (file &optional name noisy)
  (with-context ()
    (let ((ast (print-if :expand noisy (test-expand file name))))
      (print-if :type noisy (typecheck ast))
      (hcompile (print-if :match noisy (expand-matches ast))))))

(defun run-test (file &optional noisy)
  (with-context ()
    (when (stringp file)
      (setf file (merge-pathnames (concatenate 'string "test/" file ".hob") *path*)))
    (with-open-file (in file :element-type '(unsigned-byte 8))
      (let* ((expr (print-if :compile noisy (test-compile in (pathname-name file) noisy)))
             (prg (compile-s-expr expr))
             (val (print-if :result noisy (funcall prg)))
             (expect (return-spec file)))
        (unless (equal val expect)
          (error "returned ~a instead of expected ~a" val expect))
        expect))))

(defun run-testsuite ()
  (let ((ok t))
    (dolist (file (directory (merge-pathnames "test/*.hob" *path*)) ok)
      (let ((name (pathname-name file)))
        (handler-case (let ((res (run-test file)))
                        (if res
                            (format t "   '~a' returned ~a as expected.~%" name res)
                            (format t "   '~a' ran successfully.~%" name)))
          (error (e) (format t "!! '~a' ~a~%" name e) (setf ok nil)))))))
