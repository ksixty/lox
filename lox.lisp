;;;; lox.lisp

(in-package #:lox)

(defun prompt ()
  (loop
    (let ((input (read-line)))
      (format t "~A~%" (lox-parser:parse-term input)))))
