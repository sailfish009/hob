(in-package :hob)

(defun compile-s-expr (s-expr)
  (handler-bind ((warning #'muffle-warning))
    (cl:compile nil `(lambda ()
                       #+sbcl(declare (sb-ext:muffle-conditions sb-ext:compiler-note))
                       ,s-expr))))
