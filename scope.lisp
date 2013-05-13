(in-package :hob)

(defstruct (scope (:constructor scope (prev)))
  prev
  bindings)

(defstruct (binding (:constructor binding (name ns)))
  name
  ns
  fields)

(defun find-binding (scope ns name)
  (dolist (b (scope-bindings scope))
    (when (and (eq (binding-ns b) ns) (string= (binding-name b) name))
      (return b))))

(defun get-binding (scope ns name)
  (or (find-binding scope ns name)
      (let ((b (binding name ns)))
        (push b (scope-bindings scope))
        b)))

(defun lookup-binding (env ns name)
  (loop :for cur := env :then (scope-prev cur) :while cur :do
     (let ((found (find-binding cur ns name)))
       (when found (return found)))))

(defun binding-field (b kind)
  (assoc kind (binding-fields b)))

(defun lookup (env ns name kind)
  (let ((b (lookup-binding env ns name)))
    (and b (cdr (binding-field b kind)))))

(defun lookup-word (word ns kind)
  (lookup (h-word-env word) ns (h-word-name word) kind))

(defun bind (scope ns name kind value)
  (let ((known (get-binding scope ns name)))
    ;; FIXME reusing variables for pattern guard arguments doesn't work with this
;    (when (binding-field known kind)
;      (error "Rebinding ~a in binding for ~a (~a)" kind name ns))
    (push (cons kind value) (binding-fields known)))
  value)

(defun bind! (scope ns name kind value)
  (let* ((known (get-binding scope ns name))
         (field (binding-field known kind)))
    (if field
        (setf (cdr field) value)
        (push (cons kind value) (binding-fields known))))
  value)

(defun bind-word (word ns kind value)
  (bind (h-word-env word) ns (h-word-name word) kind value))
