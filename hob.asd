(asdf:defsystem :hob
  :depends-on (:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "syntax-err")
               (:file "expr")
               (:file "lex")
               (:file "parse")
               (:file "expand")
               (:file "test")))
