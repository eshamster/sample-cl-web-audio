(defpackage sample-cl-web-audio/js/panner
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-cl-web-audio/js/utils
                :get-elem
                :get-value
                :set-inner
                :add-event-listener))
(in-package :sample-cl-web-audio/js/panner)

(enable-ps-experiment-syntax)

(defun.ps init-input ()
  (dolist (id '("pan" "gain"))
    (add-event-listener id "input" #'setup)))

(defun.ps init-button ()
  (add-event-listener "play-btn" "click" #'start-play))

(def-top-level-form.ps "initialize"
  (window.add-event-listener
   "load" (lambda ()
            (init-input)
            (init-button))))

(defvar.ps+ *audioctx* nil)
(defvar.ps+ *osc* nil)
(defvar.ps+ *gain* nil)
(defvar.ps+ *panner* nil)

(defun.ps setup ()
  (let ((pan  (get-value "pan"))
        (gain (get-value "gain")))
    (set-inner "panval"  pan)
    (set-inner "gainval" gain)
    (when *audioctx*
      (setf *gain*.gain.value gain)
      (setf *panner*.pan.value pan))))

(defun.ps init-audio-if-required ()
  (when *audioctx*
    (return-from init-osc-if-required))
  (setf *audioctx* (new (#j.AudioContext#))
        *osc*      (new (#j.OscillatorNode# *audioctx*))
        *gain*     (new (#j.GainNode# *audioctx*))
        *panner*   (new (#j.StereoPannerNode# *audioctx*)))
  (chain *osc*
         (connect *gain*)
         (connect *panner*)
         (connect *audioctx*.destination)))

(defvar.ps+ *playing-p* nil)

(defun.ps start-play ()
  (when *playing-p*
    (return-from start-play))
  (init-audio-if-required)
  (setup)
  (*osc*.start)
  (setf *playing-p* t))
