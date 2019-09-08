(defpackage sample-cl-web-audio/js/utils
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :get-elem
           :get-value
           :set-inner
           :add-event-listener))
(in-package :sample-cl-web-audio/js/utils)

(enable-ps-experiment-syntax)

(defun.ps get-elem (id)
  (#j.document.getElementById# id))

(defun.ps get-value (id)
  (@ (get-elem id) value))

(defun.ps set-inner (id value)
  (setf (@ (get-elem id) #j.innerHTML#)
        value))

(defun.ps add-event-listener (id kind callback)
  ((@ (get-elem id) add-event-listener) kind callback))
