(asdf:defsystem :hob
  :depends-on (:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "variant")
               (:file "syntax-err")
               (:file "expr")
               (:file "lex")
               (:file "parse")
               (:file "scope")
               (:file "std")
               (:file "expand")
               (:file "test")
               (:file "type")))
