(defpackage sample-cl-web-audio/view/utils
  (:use :cl)
  (:export :with-cl-markup))
(in-package :sample-cl-web-audio/view/utils)

(defmacro with-cl-markup (() &body body)
  (let ((str (gensym "STR")))
    `(with-output-to-string (,str)
       (let ((cl-markup:*output-stream* ,str))
         ,@body))))
