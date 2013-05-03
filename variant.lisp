(in-package :hob.variant)

;; Variant types

(defmacro variant (name &body constructors)
  (multiple-value-bind (name slots defined) (cond ((not (consp name)) (values name nil t))
                                                  ((eq (car name) :extend) (values (second name)))
                                                  (t (values (car name) (cdr name) t)))
    `(progn
       ,@(when defined `((defstruct (,name (:copier nil) (:constructor nil)) ,@slots)))
       ,@(loop :for c :in constructors
               :append (multiple-value-bind (name* fields)
                           (if (consp c) (values (car c) (cdr c)) (values c nil))
                         `((defstruct (,name* (:include ,name)
                                              (:copier nil) (:predicate nil)
                                              (:constructor ,name* ,fields))
                             ,@fields)
                           (defmethod print-object ((obj ,name*) stream)
                             (format stream "(~a~{ ~a~})" ',name*
                                     (list ,@(loop :for field :in fields
                                                   :collect (acc name* field 'obj))))))))
       ',name)))

(defun acc (struct slot place)
  `(,(intern (format nil "~a-~a" struct slot) (symbol-package struct)) ,place))

(defun case-body (sym cases)
  (loop :for (match . body) :in cases
        :if (not (consp match)) :collect `(,match ,@body)
        :else :collect
        `(,(car match)
           (let ,(loop :for slot :in (cdr match)
                       :collect (multiple-value-bind (var name)
                                    (if (consp slot) (values (car slot) (cadr slot)) (values slot slot))
                                  `(,var ,(acc (car match) name sym))))
             ,@body))))

(defun make-case (case value cases)
  (let ((v (gensym)))
    `(let ((,v ,value))
       (,case ,v ,@(case-body v cases)))))

(defmacro vcase (value &body cases)
  (make-case 'typecase value cases))
(defmacro evcase (value &body cases)
  (make-case 'etypecase value cases))
(defmacro vbind ((type &rest slots) value &body body)
  (make-case 'etypecase value (list (cons (cons type slots) body))))
(defmacro vref ((type slot) value)
  (make-case 'etypecase value (list (list (list type slot) slot))))
