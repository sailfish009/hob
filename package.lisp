(defpackage :hob.variant
  (:use :cl)
  (:export :variant :vcase :evcase :vbind :vref))

(defpackage :hob
  (:use :cl :hob.variant))

(defpackage :hob.gen (:use))
