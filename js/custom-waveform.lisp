(defpackage sample-cl-web-audio/js/custom-waveform
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-cl-web-audio/js/utils
                :get-elem
                :get-value
                :set-inner
                :add-event-listener))
(in-package :sample-cl-web-audio/js/custom-waveform)

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
  (set-interval #'draw-graph 1000))

(defun.ps init-input ()
  (dolist (id '("freq" "gain"))
    (add-event-listener id "input" #'setup-freq-and-gain))
  (dotimes (i *table-len*)
    (add-event-listener (+ "real" i) "input" #'setup-wave)
    (add-event-listener (+ "imag" i) "input" #'setup-wave)))

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
(defvar.ps+ *ana* nil)

(defun.ps setup-freq-and-gain ()
  (let ((freq (get-value "freq"))
        (gain (get-value "gain")))
    (set-inner "freqval" freq)
    (set-inner "gainval" gain)
    (when *audioctx*
      (setf *osc*.frequency.value freq
            *gain*.gain.value     gain))))

(defvar.ps+ *table-len* 8)
(defvar.ps *real* (new (#j.Float32Array# *table-len*)))
(defvar.ps *imag* (new (#j.Float32Array# *table-len*)))

(defun.ps setup-wave ()
  (dotimes (i *table-len*)
    (let ((real (get-value (+ "real" i)))
          (imag (get-value (+ "imag" i))))
      (set-inner (+ "real" i "val") real)
      (set-inner (+ "imag" i "val") imag)
      (setf (nth i *real*) real
            (nth i *imag*) imag)))
  (when *audioctx*
    (*osc*.set-periodic-wave
     (*audioctx*.create-periodic-wave *real* *imag*))))

(defun.ps init-audio-if-required ()
  (when *audioctx*
    (return-from init-osc-if-required))
  (setf *audioctx* (new (#j.AudioContext#))
        *osc*      (new (#j.OscillatorNode# *audioctx*))
        *gain*     (new (#j.GainNode# *audioctx*))
        *ana*      (new (#j.AnalyserNode# *audioctx*)))
  (chain *osc*
         (connect *gain*)
         (connect *ana*)
         (connect *audioctx*.destination)))

(defvar.ps+ *playing-p* nil)

(defun.ps start-play ()
  (init-audio-if-required)
  (setup-wave)
  (when *playing-p*
    (return-from start-play))
  (*osc*.start)
  (setf *playing-p* t))

;; ;; --- for graph --- ;;

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
