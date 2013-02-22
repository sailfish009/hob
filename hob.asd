(asdf:defsystem :hob
  :depends-on (:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "expr")
               (:file "lex")
               (:file "parse")
               (:file "syntax-err")))
