(in-package :hob)

(defun expand-matches (expr)
  (match expr
    (("#match" vals cases) (expand-match vals cases))
    (((:or "#def" "#var") :word :_) (transform-expr expr #'expand-matches))
    (((:as (:or "#def" "#var") head) pat val) (values (expand-destructuring-bind head pat val) t))
    (t (transform-expr expr #'expand-matches))))

(defstruct br pats guard bound body)

(defgeneric type-form-disc (tp expr)
  (:method ((tp data) expr)
    (cdr (lookup-word (if (h-word-p expr) expr (h-app-head expr)) :pattern :type)))
  (:method ((tp array*) expr)
    (length (h-app-args expr)))
  (:method ((tp prim) expr)
    (h-lit-val expr)))

(defgeneric type-get-disc (tp expr)
  (:method ((tp data) expr)
    (h-app "#fld" expr (h-lit 0)))
  (:method ((tp array*) expr)
    (h-app "#len" expr))
  (:method ((tp prim) expr)
    expr))

(defgeneric type-form-extract-fields (tp n-args expr)
  (:method ((tp data) n-args expr)
    (loop :for i :below n-args :collect (h-app "#fld" expr (h-lit (1+ i)))))
  (:method ((tp array*) n-args expr)
    (loop :for i :below n-args :collect (h-app "#fld" expr (h-lit i))))
  (:method (tp n-args expr)
    (assert (eq n-args 0))
    ()))

(defgeneric type-needs-form-test (tp)
  (:method ((tp data)) (cdr (data-forms tp)))
  (:method (tp) (declare (ignore tp)) t))

(defun h-gensym (name &optional scope)
  (unless scope (setf scope (scope nil)))
  (let ((name (string-downcase (symbol-name (gensym name)))))
    (get-binding scope :value name)
    (h-sym name scope)))

(defun expand-destructuring-bind (head pat val)
  (let ((scope (scope nil))
        (statements ()))
    (labels ((explore (pat input)
               (match pat
                 ((:word nm)
                  (cond ((is-const nm)
                         (push (h-app "#assert" pat input) statements))
                        ((not (equal nm "_"))
                         (push (h-app head pat input) statements))))
                 (:lit (push (h-app "#assert" pat input) statements))
                 ((head . args)
                  (let ((type (car (lookup-word head :pattern :type))))
                    (unless (= (count-forms type) 1)
                      (push (h-app "#assert" (h-lit (type-form-disc type pat))
                                   (h-app "#fld" input (h-lit 0))) statements))
                    (loop :for arg :in args :for i :from 1 :do
                       (let ((sym (h-gensym "val" scope)))
                         (push (h-app "#def" sym (h-app "#fld" input (h-lit i))) statements)
                         (explore arg sym))))))))
      (unless (h-word-p val)
        (let ((top (h-gensym "val" scope)))
          (push (h-app "#def" top val) statements)
          (setf val top)))
      (explore pat val)
      (nreverse statements))))

;; FIXME only wrap bodies in functions when actually necessary (delay
;; decision, refcount)

(defun expand-match (vals cases)
  (let* ((defs ())
         (scope (scope nil))
         (n-pats)
         (branches (lmapseq (("->" pats body) cases)
                     (let ((name (h-gensym "body" scope))
                           (body (expand-matches body)))
                       (multiple-value-bind (pats guard)
                           (match pats (("#guard" g p) (values p g)) (:_ pats))
                         (unless n-pats (setf n-pats (seq-len pats)))
                         (when guard
                           (setf body
                             (h-app "#match" (h-app "#fld" guard (h-lit 0))
                                    (h-seq (list (h-app "->" (h-lit 1) (h-app (h-sym "some" *top*) body))
                                                 (h-app "->" (h-lit 2) (h-sym "$none" *top*)))))))
                         (push (h-app "#def" name (h-app "#fn" (flatten-patterns pats) (h-nil) (h-nil) body)) defs)
                         (make-br :pats (seq-list pats) :guard (and guard t) :bound () :body name)))))
         (val-vars (lmapseq (val vals)
                     (if (h-word-p val)
                         val
                         (let ((name (h-gensym "val" scope)))
                           (push (h-app "#def" name val) defs)
                           name))))
         (fallthrough (make-br :pats (loop :repeat n-pats :collect (h-word "_")) :guard nil :bound ()
                               :body (h-app "#assert" (h-sym "()" *top*))))) ;; FIXME () vs h-nil?
    (setf (cdr (last branches)) (list fallthrough))
    (push (expand-cases val-vars branches) defs)
    (h-seq (nreverse defs))))

(defun flatten-pattern (pat)
  (match pat
    ((:word nm) (unless (or (is-const nm) (equal nm "_"))
                  (list pat)))
    (:lit ())
    ((:_ . args) (mapcan #'flatten-pattern args))))
               
(defun flatten-patterns (pats)
  (h-seq (or (mapcan #'flatten-pattern (seq-list pats))
             (list (h-word "_")))))

(defstruct opt type disc n-args)

(defun sort-branches (branches val)
  (let (sorted default)
    (labels ((extend-default (br n)
               (if (zerop n)
                   br
                   (let ((br (copy-br br)))
                     (loop :repeat n :do (push (h-word "_") (br-pats br)))
                     br)))
             (add-default (br &optional val)
               (let ((br (make-br :pats (cdr (br-pats br))
                                  :guard (br-guard br)
                                  :bound (if val (cons val (br-bound br)) (br-bound br))
                                  :body (br-body br))))
                 (push br default)
                 (loop :for cons :in sorted :do
                    (push (extend-default br (opt-n-args (car cons)))
                          (cdr cons)))))
             (add-opt (type disc br &optional args)
               (let ((br (make-br :pats (if args (append args (cdr (br-pats br))) (cdr (br-pats br)))
                                  :guard (br-guard br)
                                  :bound (br-bound br)
                                  :body (br-body br))))
                 (let ((found (find-if (lambda (cons) (equal (opt-disc (car cons)) disc)) sorted)))
                   (if found
                       (push br (cdr found))
                       (let* ((n-args (length args))
                              (opt (make-opt :type type :disc disc :n-args n-args))
                              (defs (if (zerop n-args) default
                                        (loop :for d :in default :collect (extend-default d (length args))))))
                         (push (cons opt (cons br defs)) sorted)))))))
      (loop :for br :in branches :for pat := (car (br-pats br)) :do
         (match pat
           (:lit (add-opt (type-of-lit pat) (h-lit-val pat) br))
           ((:word nm)
            (if (is-const nm)
                (let ((tp (car (lookup-word pat :pattern :type))))
                  (add-opt tp (type-form-disc tp pat) br))
                (add-default br (unless (equal nm "_") val))))
           (((:as :word nm) . args)
            (let ((tp (car (lookup-word nm :pattern :type))))
              (add-opt tp (type-form-disc tp pat) br args)))))
      (values (loop :for (opt . brs) :in sorted :collect (cons opt (nreverse brs)))
              (nreverse default)))))

(defun count-forms (type)
  (vcase type
    ((data forms) (length forms))
    (t most-positive-fixnum)))

(defun expand-cases (inputs branches)
  (let ((first (car branches)))
    (unless (br-pats first)
      (let* ((args (or (reverse (br-bound first)) (list (h-sym "()" *top*)))) ;; FIXME () vs h-nil
             (body (h-app* (br-body first) args)))
        (when (br-guard first)
          (let ((sym (h-gensym "val")))
            (setf body (h-seq (list
              (h-app "#def" sym body)
              (h-app "#match" (h-app "#fld" sym (h-lit 0))
                     (h-seq (list (h-app "->" (h-lit 1) (h-app "#fld" sym (h-lit 1)))
                                  (h-app "->" (h-lit 2) (expand-cases inputs (cdr branches)))))))))))
        (return-from expand-cases body))))
  (multiple-value-bind (sorted default) (sort-branches branches (car inputs))
    ;; Only condition-less branches, simply continue through
    (unless sorted
      (return-from expand-cases (expand-cases (cdr inputs) default)))
    (let ((type (opt-type (caar sorted))))
      (if (not (type-needs-form-test type))
          (expand-data-match (caar sorted) (car inputs) (cdr inputs) (cdar sorted))
          (let ((val (type-get-disc type (car inputs)))
                (cases (loop :for (opt . branches) :in sorted :collect
                          (h-app "->" (h-lit (opt-disc opt))
                                 (expand-data-match opt (car inputs) (cdr inputs) branches)))))
            (when (and default (< (length sorted) (count-forms type)))
              (let ((ft (h-app "->" (h-word "_") (expand-cases (cdr inputs) default))))
                (setf cases (nconc cases (list ft)))))
            (h-app "#match" val (h-seq cases)))))))

(defun expand-data-match (opt base inputs branches)
  (let ((fields (type-form-extract-fields (opt-type opt) (opt-n-args opt) base)))
    (if fields
        (let* ((scope (scope nil))
               syms
               (defs (loop :for fld :in fields :collect
                        (let ((s (h-gensym "val" scope)))
                          (push s syms)
                          (h-app "#def" s fld)))))
          (h-seq (nconc defs (list (expand-cases (nconc (nreverse syms) inputs) branches)))))
        (expand-cases inputs branches))))
