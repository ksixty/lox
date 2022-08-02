;;;; lox.asd

(asdf:defsystem #:lox
  :description "Untyped λ-calculus interpreter"
  :author "Vanya Klimenko <k60+git@fmap.me>"
  :license  "GPLv3"
  :version "0.0.1"
  :serial t
  :depends-on (#:parsnip #:cffi)
  :components ((:file "package")
               (:file "lox-parser")))
