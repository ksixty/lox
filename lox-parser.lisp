(in-package #:lox-parser)

(defparameter *whitespace* '(#\space #\newline #\tab #\return))
(defparameter *special-characters* (list* #\λ #\( #\) #\. *whitespace*))

(defstruct abstraction variable term binding)
(defstruct application left-term right-term)
(defstruct var         db-index scope)

(defmethod print-object ((obj abstraction) stream)
  (with-slots (variable term) obj
    (format stream "λ~A.~A" variable term)))

(defmethod print-object ((obj application) stream)
  (with-slots (left-term right-term) obj
      (format stream "(~A ~A)" left-term right-term)))

(defun parse-with (parser string)
  (with-input-from-string (stream string)
    (parse parser stream)))

(defparser =whitespace ()
    (skip-many (char-in *whitespace*)))

(defun trim-whitespace (parser)
  (prog2! '=whitespace parser '=whitespace))

(defparser =symbol-character ()
  (char-if (lambda (char) (not (member char *special-characters*)))))

(defparser =symbol ()
  (let! ((char-list (collect1 '=symbol-character)))
        (ok (coerce char-list 'string))))

(defun parenthesis (parser)
  (prog2! (char-of #\() parser (char-of #\))))

(defparser =binding ()
  (let! ((variable '=symbol)
         (walrus   (trim-whitespace (string-of ":=")))
         (term     '=symbol))
        (ok (make-abstraction :variable variable
                              :term term
                              :binding true))))

(defparser =abstraction ()
  (let! ((λ        (char-of #\λ))
         (variable '=symbol)
         (dot      (char-of #\.))
         (term     '=term))
        (ok (make-abstraction :variable variable
                              :term term))))

(defparser =applicative ()
  (or! (parenthesis '=term)
       '=abstraction
       '=symbol))

(defmacro let@ ((&rest bindings) &body body)
  "Anaphoric self-callable LET."
  (let ((names (mapcar #'first bindings))
        (values (mapcar #'second bindings)))
    `(labels ((@ ,names ,@body))
       (@ ,@values))))

(defun reduce-right! (function parser &key initial-parser)
  "Return a parser that keeps running until failure, and reduces its results into one value.
If INITIAL-PARSER is supplied, the parser may succeed without calling FUNCTION by returning INITIAL-PARSER's response."
  (let! ((initial-value (or initial-parser parser)))
    (let@ ((result initial-value))
      (handle
        (let! ((obj parser))
          (@ (funcall function result obj)))
        (lambda (expected trace)
          (declare (ignore expected trace))
          (ok result))))))

(defparser =term ()
  (let! ((applications (sep '=applicative '=whitespace)))
    (ok (reduce (lambda (a b) (make-application :left-term a :right-term b))
                applications))))


(defun parse-term (string)
  (parse-with '=term string))
