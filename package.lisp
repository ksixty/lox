;;;; package.lisp

(defpackage #:lox-parser
  (:use #:cl #:parsnip)
  (:export #:parse-term))

(defpackage #:lox
  (:use #:cl))
