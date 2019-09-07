(defpackage sample-cl-web-audio/js
  (:use :cl
        :ps-experiment
        :parenscript)
  (:export :play
           :setup))
(in-package :sample-cl-web-audio/js)

(enable-ps-experiment-syntax)

;; --- the followings are only sample --- ;;

(defvar.ps+ *playing-p* nil)

(defvar.ps+ *osc* nil)
(defvar.ps+ *gain* nil)

(defun.ps init-if-required ()
  (when *osc*
    (return-from init-if-required))
  (let ((audioctx (new (#j.AudioContext#))))
    (setf *osc*  (new (#j.OscillatorNode# audioctx))
          *gain* (new (#j.GainNode# audioctx)))
    (chain *osc*
           (connect *gain*)
           (connect audioctx.destination))))

(defun.ps play ()
  (when *playing-p*
    (return-from play))
  (init-if-required)
  (*osc*.start)
  (setf *playing-p* t))

(defun.ps setup ()
  (init-if-required)
  (labels ((get-elem (id)
             (#j.document.getElementById# id))
           (get-value (id)
             (@ (get-elem id) value))
           (set-inner (id value)
             (setf (@ (get-elem id) #j.innerHTML#)
                   value)))
    (let ((type  (get-value "type"))
          (freq  (get-value "freq"))
          (level (get-value "level")))
      (set-inner "freqdisp"  freq)
      (set-inner "leveldisp" level)
      (setf *osc*.type            type
            *osc*.frequency.value freq
            *gain*.gain.value     level))))
