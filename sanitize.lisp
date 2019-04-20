(in-package #:hacksthenet)

(defun html-quote-char (char stream)
  (case char
    (#\< (write-string "&lt;" stream))
    (#\> (write-string "&gt;" stream))
    (#\& (write-string "&amp;" stream))
    (#\" (write-string "&quot;" stream))
    (otherwise (write-char char stream))))

(defun sanitize (string)
  (with-output-to-string (stream)
    (loop for char across string
       do (html-quote-char char stream))))
