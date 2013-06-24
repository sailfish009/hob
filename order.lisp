(in-package :hob)

(defun verify-use-order (expr)
  (match expr
    ((:seq exprs)
     (verify-use-order-in exprs)
     (dolist (expr exprs) (verify-use-order expr)))
    ((head . args)
     (verify-use-order head)
     (dolist (arg args) (verify-use-order arg)))
    (t)))

(defun word-binding (word)
  (lookup-binding (h-word-env word) :value (h-word-name word)))

(defun verify-use-order-in (forms)
  (let (all-defs defs)
    (loop :for form :in forms :do
       (match form
         (((:or "#def" "#var") name val)
          (let ((b (word-binding name)))
            (push (match val
                    (("#fn" . :_) (list b :function val :todo))
                    (t (list b :expr))) all-defs)))
         (t)))
    (when all-defs
      (setf defs (reverse all-defs))
      (loop :for form :in forms :do
         (match form
           (((:or "#def" "#var") :_ e)
            (match e
              (("#fn" . :_))
              (t (verify-not-using defs all-defs e)))
            (pop defs))
           (((:or "#data" "#class") . :_))
           (t (verify-not-using defs all-defs form)))))))

(defun verify-not-using (future-defs all-defs expr)
  (walk expr (expr)
    (:word
     (let* ((b (word-binding expr))
            (fut (cdr (assoc b future-defs)))
            (func (or fut (cdr (assoc b all-defs)))))
       (when (eq (car fut) :expr)
         (syntax-error expr "using variable `~a` before it is defined" expr))
       (when (eq (car func) :function)
         (dolist (b (find-use-closure func all-defs))
           (when (eq (car (cdr (assoc b future-defs))) :expr)
             (syntax-error expr "using variable `~a` before it is defined (via function `~a`)" (binding-name b) expr))))))))

(defun find-use-closure (fn all-defs)
  (when (eq (third fn) :todo)
    (let ((found (setf (third fn) ())))
      (walk (second fn) (expr)
        (:word
         (let* ((b (word-binding expr))
                (known (cdr (assoc b all-defs))))
           (case (car known)
             (:expr
              (pushnew b found))
             (:function
              (dolist (b (find-use-closure found all-defs))
                (pushnew b found)))))))
      (setf (third fn) found)))
  (third fn))
