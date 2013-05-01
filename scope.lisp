(in-package :hob)

(defstruct (scope (:constructor scope (prev)))
  prev
  pat
  value
  type)

(defstruct (binding (:constructor binding (name val &optional kind)))
  name
  kind
  val)

(defun vlookup* (env name kind)
  (loop :for cur := env :then (scope-prev cur) :while cur :do
     (loop :for b :in (scope-value cur) :do
        (when (equal (binding-name b) name)
          (return-from vlookup* (and (eq kind (binding-kind b)) b))))))

(defun vlookup (env name kind)
  (let ((found (vlookup* env name kind)))
    (if found
        (binding-val found)
        (error "Undefined variable ~a" name))))

(defun vbind (env name kind val)
  (let ((b (binding name val kind)))
    (push b (scope-value env))
    b))

(defun tlookup* (env name kind)
  (loop :for cur := env :then (scope-prev cur) :while cur :do
     (loop :for b :in (scope-type cur) :do
        (when (and (equal (binding-name b) name) (eq kind (binding-kind b)))
          (return-from tlookup* b)))))

(defun tlookup (env name kind)
  (let ((found (tlookup* env name kind)))
    (if found
        (binding-val found)
        (error "Undefined type ~a" name))))

(defun tbind (env name kind val)
  (let ((b (binding name val kind)))
    (push b (scope-type env))
    b))

(defun plookup* (env name kind)
  (loop :for cur := env :then (scope-prev cur) :while cur :do
     (loop :for b :in (scope-pat cur) :do
        (when (and (equal (binding-name b) name) (eq kind (binding-kind b)))
          (return-from plookup* b)))))

(defun plookup (env name kind)
  (let ((found (plookup* env name kind)))
    (if found
        (binding-val found)
        (error "Undefined pattern ~a" name))))

(defun pbind (env name kind val)
  (let ((b (binding name val kind)))
    (push b (scope-pat env))
    b))

