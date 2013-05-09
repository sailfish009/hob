(in-package :hob)

(defstruct context
  (pat-types (make-hash-table :test 'eq)))

(defvar *context*)

(defmacro with-context (() &body body)
  `(let ((*context* (make-context))) ,@body))
