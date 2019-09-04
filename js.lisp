(defpackage sample-cl-web-audio/js
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :play))
(in-package :sample-cl-web-audio/js)

(enable-ps-experiment-syntax)

;; --- the followings are only sample --- ;;

(defun.ps play ()
  (let* ((audioctx (new (#j.AudioContext#)))
         (osc (new (#j.OscillatorNode# audioctx))))
    (osc.connect audioctx.destination)
    (osc.start)))


