(defpackage sample-cl-web-audio/js/biquad-filter
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-cl-web-audio/js/utils
                :get-elem
                :get-value
                :set-inner
                :add-event-listener))
(in-package :sample-cl-web-audio/js/biquad-filter)

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
  (set-interval #'draw-graph 100))

(defun.ps init-input ()
  (add-event-listener "type" "change" #'setup-filter)
  (dolist (id '("freq" "q" "gain"))
    (add-event-listener id "input" #'setup-filter)))

(defun.ps init-button ()
  (add-event-listener "play-btn" "click" #'start-play))

(def-top-level-form.ps "initialize"
  (window.add-event-listener
   "load" (lambda ()
            (init-drawing)
            (init-input)
            (init-button))))

(defvar.ps+ *audioctx* nil)
(defvar.ps+ *noisebuf* nil)
(defvar.ps+ *filter* nil)
(defvar.ps+ *ana* nil)

(defun.ps setup-filter ()
  (let ((freq (get-value "freq"))
        (q    (get-value "q"))
        (gain (get-value "gain")))
    (set-inner "freqval" freq)
    (set-inner "qval"    q)
    (set-inner "gainval" gain)
    (when *filter*
      (setf *filter*.frequency.value freq
            *filter*.-q.value        q
            *filter*.gain.value      gain)
      (setf *filter*.type
            (nth (@ (get-elem "type") selected-index)
                 (list "lowpass" "highpass" "bandpass" "lowshelf"
                       "highshelf" "peaking" "notch" "allpass"))))))

(defun.ps init-audio-if-required ()
  (when *audioctx*
    (return-from init-osc-if-required))
  (setf *audioctx* (new (#j.AudioContext#)))
  (setf *noisebuf* (new (#j.AudioBuffer# (create "channels"   1
                                                 "length"     *audioctx*.sample-rate
                                                 "sampleRate" *audioctx*.sample-rate)))
        *filter*   (new (#j.BiquadFilterNode# *audioctx*
                                              (create "frequency" 5000 "q" 5)))
        *ana*      (new (#j.AnalyserNode# *audioctx*
                                          (create "smoothingTimeConstant" 0.7
                                                  "fftSize" 1024))))
  (chain *filter*
         (connect *ana*)
         (connect *audioctx*.destination))
  ;; setup noise
  (let ((noise-buf-data (*noisebuf*.get-channel-data 0)))
    (dotimes (i *audioctx*.sample-rate)
      (setf (nth i noise-buf-data)
            (* (- (random) 0.5)
               0.5)))))

(defvar.ps+ *playing-p* nil)

(defun.ps start-play ()
  (init-audio-if-required)
  (when *playing-p*
    (return-from start-play))
  (let ((src (new (#j.AudioBufferSourceNode#
                   *audioctx* (create "buffer" *noisebuf*
                                      "loop"   t)))))
    (src.connect *filter*)
    (src.start)
    (setf *playing-p* t)))

;; --- for graph --- ;;

(defvar.ps+ *analyse-data-len* 1024)
(defvar.ps *analyse-data* (new (#j.Float32Array# *analyse-data-len*)))
(defvar.ps+ *min-gain* -50)
(defvar.ps+ *max-gain*  50)

(defun.ps draw-graph ()
  (unless *ana*
    (return-from draw-graph))
  (reset-canvas)
  (let ((ctx (get-canvasctx))
        (y-offset (/ *canvas-height* 2)))
    ;; graph
    (setf ctx.fill-style "#007700")
    (*ana*.get-float-frequency-data *analyse-data*)
    (dotimes (x *canvas-width*)
      (let* ((f (/ (* *audioctx*.sample-rate x)
                   *analyse-data-len*))
             (y (+ y-offset (* (+ (nth x *analyse-data*) 48.16)
                               2.56))))
        (ctx.fill-rect x (- *canvas-height* y) 1 y)))
    (let ((x-margin 20)
          (y-margin 10))
      ;; x axis
      (setf ctx.fill-style "#ff9955")
      (loop :for d :from *min-gain* :below *max-gain* :by 10
         :do (let ((y (- y-offset (logior (* d *canvas-height* 1/100)
                                          0))))
               (ctx.fill-rect x-margin y *canvas-width* 1)
               (ctx.fill-text (+ d "dB") 5 y)))
      ;; y axis
      (loop :for freq :from 2000 :below (/ *audioctx*.sample-rate 2) :by 2000
         :do (let ((x (logior (/ (* freq 1024) *audioctx*.sample-rate)
                              0)))
               (ctx.fill-rect x 0 1 (- *canvas-height* y-margin 1))
               (ctx.fill-text (+ freq "Hz") (- x 10) (1- *canvas-height*)))))))
