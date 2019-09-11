(defpackage sample-cl-web-audio/js/wave-shaper
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-cl-web-audio/js/utils
                :get-elem
                :get-value
                :set-inner
                :add-event-listener))
(in-package :sample-cl-web-audio/js/wave-shaper)

(enable-ps-experiment-syntax)

(defvar.ps+ *canvas-width* 512)
(defvar.ps+ *canvas-height* 256)

(defun.ps get-canvasctx ()
  ((@ (get-elem "canvas") get-context) "2d"))

(defun.ps reset-canvas ()
  (let ((canvasctx (get-canvasctx)))
    (setf canvasctx.fill-style "#222222")
    (canvasctx.fill-rect 0 0 *canvas-width* *canvas-height*)))

(defun.ps init-drawing ()
  (reset-canvas)
  (set-interval #'draw-graph 500))

(defun.ps init-input ()
  (dolist (id '("steps" "gain"))
    (add-event-listener id "input" #'setup)))

(defun.ps init-button ()
  (add-event-listener "play-btn" "click" #'start-play))

(def-top-level-form.ps "initialize"
  (window.add-event-listener
   "load" (lambda ()
            (init-drawing)
            (init-input)
            (init-button))))

(defvar.ps+ *audioctx* nil)
(defvar.ps+ *gain* nil)
(defvar.ps+ *osc* nil)
(defvar.ps+ *shaper* nil)
(defvar.ps+ *ana* nil)

(defun.ps setup ()
  (let ((steps (get-value "steps"))
        (gain  (get-value "gain")))
    (set-inner "stepsval" steps)
    (set-inner "gainval"  gain)
    (when *audioctx*
      (setf *gain*.gain.value gain)
      (let ((curve (new (#j.Float32Array# 4096))))
        (dotimes (i 4096)
          (setf (nth i curve)
                (1- (* (/ (logior (* (/ i 4096) steps)
                                  0)
                          (1- steps))
                       2))))
        (setf *shaper*.curve curve)))))

(defun.ps init-audio-if-required ()
  (when *audioctx*
    (return-from init-osc-if-required))
  (setf *audioctx* (new (#j.AudioContext#))
        *osc*      (new (#j.OscillatorNode# *audioctx*))
        *shaper*   (new (#j.WaveShaperNode# *audioctx*))
        *gain*     (new (#j.GainNode# *audioctx*))
        *ana*      (new (#j.AnalyserNode# *audioctx*)))
  (chain *osc*
         (connect *shaper*)
         (connect *gain*)
         (connect *ana*)
         (connect *audioctx*.destination)))

(defvar.ps+ *playing-p* nil)

(defun.ps start-play ()
  (when *playing-p*
    (return-from start-play))
  (init-audio-if-required)
  (setup)
  (*osc*.start)
  (setf *playing-p* t))

;; ;; ;; --- for graph --- ;;

(defvar.ps+ *capture-buf-len* 512)
(defvar.ps *capture-buf* (new (#j.Float32Array# *capture-buf-len*)))

(defun.ps draw-graph ()
  (unless *ana*
    (return-from draw-graph))
  (reset-canvas)
  (let ((ctx (get-canvasctx))
        (y-offset (/ *canvas-height* 2)))
    (setf ctx.fill-style "#00ff44")
    (*ana*.get-float-time-domain-data *capture-buf*)
    (ctx.fill-rect 0 y-offset *canvas-width* 1)
    (dotimes (i *capture-buf-len*)
      (let ((v (- y-offset
                  (* (nth i *capture-buf*) 128))))
        (ctx.fill-rect i v 1 (- 128 v))))))
