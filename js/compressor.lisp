(defpackage sample-cl-web-audio/js/compressor
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-cl-web-audio/js/utils
                :get-elem
                :get-value
                :set-inner
                :add-event-listener))
(in-package :sample-cl-web-audio/js/compressor)

(enable-ps-experiment-syntax)

(defvar.ps+ *canvas-width* 364)
(defvar.ps+ *canvas-height* 364)

(defun.ps get-canvasctx ()
  ((@ (get-elem "canvas") get-context) "2d"))

(defun.ps reset-canvas ()
  (let ((canvasctx (get-canvasctx)))
    (setf canvasctx.fill-style "#222222")
    (canvasctx.fill-rect 0 0 *canvas-width* *canvas-height*)))

(defun.ps init-drawing ()
  (draw-graph -3))

(defun.ps init-input ()
  (dolist (id '("thresh" "knee" "ratio" "atk" "rel"))
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
(defvar.ps+ *comp* nil)
(defvar.ps+ *ana* nil)

(defun.ps setup ()
  (let ((thresh (get-value "thresh"))
        (knee   (get-value "knee"))
        (ratio  (get-value "ratio"))
        (atk    (get-value "atk"))
        (rel    (get-value "rel")))
    (set-inner "threshval" thresh)
    (set-inner "kneeval"   knee)
    (set-inner "ratioval"  ratio)
    (set-inner "atkval"    atk)
    (set-inner "relval"    rel)
    (when *audioctx*
      (setf *comp*.threshold.value thresh
            *comp*.knee.value      knee
            *comp*.ratio.value     ratio
            *comp*.attack.value    atk
            *comp*.release.value   rel))))

(defun.ps init-audio-if-required ()
  (when *audioctx*
    (return-from init-osc-if-required))
  (setf *audioctx* (new (#j.AudioContext#))
        *osc*      (new (#j.OscillatorNode# *audioctx*))
        *comp*     (new (#j.DynamicsCompressorNode# *audioctx*))
        *gain*     (new (#j.GainNode# *audioctx*))
        *ana*      (new (#j.AnalyserNode# *audioctx*)))
  (chain *osc*
         (connect *gain*)
         (connect *comp*)
         (connect *ana*)
         (connect *audioctx*.destination))
  (setup)
  (*osc*.start))

(defvar.ps+ *timer* nil)
(defvar.ps+ *frame-counter* -2)
(defvar.ps  *wav-data* (new (#j.Float32Array# 512)))

(defun.ps start-play ()
  (init-audio-if-required)
  (when (string= *audioctx*.state "suspended")
    (*audioctx*.resume))
  (setf *gain*.gain.value 0
        *frame-counter*   -2)
  (unless *timer*
    (setf *timer* (set-interval #'update 50))))

;; ;; ;; ;; --- for graph --- ;;

(defvar.ps+ *gain-table-len* 100)
(defvar.ps+ *gain-table* nil)

(defun.ps update ()
  (when (> *frame-counter* 0)
    (*ana*.get-float-time-domain-data *wav-data*)
    (let ((max-level 0))
      (dotimes (i *wav-data*.length)
        (setf max-level (max max-level
                             (abs (nth i *wav-data*)))))
      (setf (nth (1- *frame-counter*) *gain-table*)
            max-level)))
  (draw-graph (1- *frame-counter*))
  (setf *gain*.gain.value
        (expt 10 (/ (- *frame-counter* 80) 20)))
  (when (> (incf *frame-counter*) *gain-table-len*)
    (setf *gain*.gain.value 0)
    (clear-interval *timer*)
    (setf *timer* nil)))

(defun.ps draw-graph (n)
  (unless *gain-table*
    (setf *gain-table* (new (#j.Array# *gain-table-len*)))
    (dotimes (i *gain-table-len*)
      (setf (nth i *gain-table*) 0)))
  (reset-canvas)
  (let ((ctx (get-canvasctx))
        (offset 32)
        (graph-size 300))
    (setf ctx.fill-style "#20c040")
    (loop :for i :from 0 :below *gain-table-len* :do
         (let ((v (nth i *gain-table*)))
           (when (< v 1e-128)
             (setf v 1e-128))
           (setf v (max -80 (* 20 (log v 10))))
           (setf v (* 3 (- 20 v)))
           (ctx.fill-rect (+ (* i 3) offset)
                          (+ v offset)
                          3 (- 300 v))))
    (setf ctx.fill-style "#c06060")
    (loop :for i :from 0 :to *gain-table-len* :by 10 :do
         (ctx.fill-rect offset (+ offset (* i 3))
                        graph-size 1)
         (ctx.fill-rect (+ offset (* i 3)) offset
                        1 graph-size)
         (ctx.fill-text (+ (- 20 i) "dB") 5 (+ (* i 3) 35))
         (ctx.fill-text (+ (- 20 i) "dB") (- 320 (* i 3)) 345))
    (setf ctx.fill-style "#f0e480")
    (ctx.fill-rect (+ 34 (* n 3)) 32 1 graph-size)))
