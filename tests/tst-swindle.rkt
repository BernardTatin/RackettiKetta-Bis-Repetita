#lang swindle

(defgeneric (show-me x))

(defmethod (no-applicable-method [m (singleton show-me)] xs)
  (echo "No method in" m "for" :w xs))

(defmethod (show-me [x <integer>])
    (display (format "~a is an integer\n" x)))

(defmethod (show-me [x <number>])
    (display (format "~a is a number\n" x)))

(defmethod (show-me [x <string>])
    (display (format "~a is a string\n" x)))

(show-me 1)
(show-me 1.2)
(show-me 1/2)
(show-me "Coucou")
(show-me #f)
(show-me 'a)