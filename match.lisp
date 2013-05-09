(in-package :hob)

;; FIXME use a transforming helper that prevent unneccesary copying

(defun expand-matches (expr)
  (match expr
    (("#match" vals cases) (expand-match vals cases))
    (:word expr)
    (:lit expr)
    ((:seq vals) (h-seq (mapcar #'expand-matches vals)))
    ((f . args) (h-app* (expand-matches f) (mapcar #'expand-matches args)))))

(defstruct br pats bound body)

(defun h-gensym (name &optional scope)
  (unless scope (setf scope (scope nil)))
  (let ((name (string-downcase (symbol-name (gensym name)))))
    (get-binding scope :value name)
    (h-word name nil (scope nil))))

(defun expand-match (vals cases)
  (let* ((defs ())
         (scope (scope nil))
         (branches (lmapseq (("->" pats body) cases)
                     (let ((name (h-gensym "body" scope)))
                       (push (h-app "#def" name
                                    (h-app "#fn" (flatten-patterns pats)
                                           (expand-matches body))) defs)
                       (make-br :pats (seq-list pats) :bound () :body name))))
         (val-vars (lmapseq (val vals)
                     (let ((name (h-gensym "val" scope)))
                       (push (h-app "#def" name val) defs)
                       name))))
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

(defstruct opt name type disc n-args)

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
                                  :bound (if val (cons val (br-bound br)) (br-bound br))
                                  :body (br-body br))))
                 (push br default)
                 (loop :for cons :in sorted :do
                    (push (extend-default br (opt-n-args (car cons)))
                          (cdr cons)))))
             (add-opt (type name br &optional args)
               (let ((br (make-br :pats (if args (append args (cdr (br-pats br))) (cdr (br-pats br)))
                                  :bound (br-bound br)
                                  :body (br-body br))))
                 (let ((found (find-if (lambda (cons) (equal (opt-name (car cons)) name)) sorted)))
                   (if found
                       (push br (cdr found))
                       (let* ((n-args (length args))
                              (opt (make-opt :name name :type type :disc (find-disc type name) :n-args n-args))
                              (defs (if (zerop n-args) default
                                        (loop :for d :in default :collect (extend-default d (length args))))))
                         (push (cons opt (cons br defs)) sorted)))))))
      (loop :for br :in branches :for pat := (car (br-pats br)) :do
         (let ((type (evcase (resolve (gethash pat (context-pat-types *context*)))
                       ((inst type) type)
                       (t nil))))
           (match pat
             (:lit (add-opt type (h-lit-val pat) br))
             ((:word nm)
              (if (is-const nm)
                  (add-opt type nm br)
                  (add-default br (unless (equal nm "_") val))))
             (((:word nm) . args) (add-opt type nm br args)))))
      (values (loop :for (opt . brs) :in sorted :collect (cons opt (nreverse brs)))
              (nreverse default)))))

(defun find-disc (type name)
  (vcase type
    ((data forms) (loop :for f :in forms :for i :from 1 :do
                     (when (equal name f) (return i))))
    (t name)))

(defun expand-cases (inputs branches)
  (let ((first (car branches)))
    (unless (br-pats first)
      (return-from expand-cases
        (h-app* (br-body first)
                (or (reverse (br-bound first)) (list (h-word "()")))))))
  (multiple-value-bind (sorted default) (sort-branches branches (car inputs))
    ;; Only condition-less branches, simply continue through
    (unless sorted
      (return-from expand-cases (expand-cases (cdr inputs) default)))
    (let* ((type (opt-type (caar sorted)))
           (is-data (typep type 'data)))
      (if (and is-data (not (cdr (data-forms type))))
          (expand-data-match (caar sorted) (car inputs) (cdr inputs) (cdar sorted))
          (let ((val (if is-data (h-app "#fld" (car inputs) (h-lit 0)) (car inputs)))
                (cases (loop :for (opt . branches) :in sorted :collect
                          (let ((sub (if is-data
                                         (expand-data-match opt (car inputs) (cdr inputs) branches)
                                         (expand-cases (cdr inputs) branches))))
                            (h-app "->" (h-lit (opt-disc opt)) sub)))))
            (when default
              (let ((ft (h-app "->" (h-word "_") (expand-cases (cdr inputs) default))))
                (setf cases (nconc cases (list ft)))))
            (h-app "#match" val (h-seq cases)))))))

(defun expand-data-match (opt base inputs branches)
  (let* ((scope (scope nil))
         (syms (loop :repeat (opt-n-args opt) :collect (h-gensym "val" scope))))
    (h-seq (nconc (loop :for sym :in syms :for i :from 1 :collect
                     (h-app "#def" sym (h-app "#fld" base (h-lit i))))
                  (list (expand-cases (nconc syms inputs) branches))))))
