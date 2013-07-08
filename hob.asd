(asdf:defsystem :hob
  :depends-on (:cl-ppcre)
  :serial t
  :components ((:file "package")
               (:file "misc")
               (:file "variant")
               (:file "syntax-err")
               (:file "expr")
               (:file "expr-util")
               (:file "lex")
               (:file "parse")
               (:file "scope")
               (:file "type")
               (:file "compile")
               (:file "std")
               (:file "expand")
               (:file "match")
               (:file "order")
               (:file "test")))
