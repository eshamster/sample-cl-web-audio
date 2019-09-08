(defpackage sample-cl-web-audio/js/automation
  (:use :cl
        :ps-experiment
        :parenscript)
  (:import-from :sample-cl-web-audio/js/utils
                :get-elem
                :get-value
                :set-inner
                :add-event-listener))
(in-package :sample-cl-web-audio/js/automation)

(enable-ps-experiment-syntax)

(defvar.ps+ *canvas-x* 0)
(defvar.ps+ *canvas-width* 500)
(defvar.ps+ *canvas-height* 256)
(defvar.ps *graphdata* (new (-uint8-array 128)))

(defun.ps get-canvasctx ()
  ((@ (get-elem "canvas") get-context) "2d"))

(defun.ps reset-canvas ()
  (setf *canvas-x* 0)
  (let ((canvasctx (get-canvasctx)))
    (setf canvasctx.fill-style "#222222")
    (canvasctx.fill-rect 0 0 *canvas-width* *canvas-height*)))

(defun.ps init-drawing ()
  (reset-canvas)
  (set-interval
   (lambda ()
     (when (and *ana* (< *canvas-x* *canvas-width*))
       (*ana*.get-byte-time-domain-data *graphdata*)
       (let ((x *canvas-x*)
             (y 0))
         (dolist (val *graphdata*)
           (let ((d (abs (- val 128))))
             (when (> d y)
               (setf y d))))
         (let ((canvasctx (get-canvasctx)))
           (setf canvasctx.fill-style "#222222")
           (canvasctx.fill-rect x 0 2 *canvas-height*)
           (setf canvasctx.fill-style "#00ff00")
           (canvasctx.fill-rect x (- 256 (* 2 y)) 2 (* 2 y)))))
     (incf *canvas-x* 2))
   50))

(defun.ps init-range-input ()
  (dolist (pair '(("atk" "atkval")
                  ("dcy" "dcyval")
                  ("sus" "susval")
                  ("rel" "relval")))
    (let ((input (car pair))
          (val   (cadr pair)))
      (add-event-listener
       input "input" (lambda (ev)
                       (set-inner val ev.target.value))))))

(defun.ps init-button ()
  (add-event-listener "play-btn" "mousedown" #'start-play)
  (add-event-listener "play-btn" "mouseup"   #'start-decrease-to-zero))

(def-top-level-form.ps "initialize"
  (window.add-event-listener
   "load" (lambda ()
            (init-drawing)
            (init-range-input)
            (init-button))))

(defvar.ps+ *audioctx* nil)
(defvar.ps+ *osc* nil)
(defvar.ps+ *gain* nil)
(defvar.ps+ *ana* nil)

(defun.ps init-osc-if-required ()
  (when *osc*
    (return-from init-osc-if-required))
  (setf *audioctx* (new (#j.AudioContext#)))
  (setf *osc*  (new (#j.OscillatorNode# *audioctx*))
        *gain* (new (#j.GainNode# *audioctx* (create :gain 0)))
        *ana*  (new (#j.AnalyserNode# *audioctx*)))
  (chain *osc*
         (connect *gain*)
         (connect *ana*)
         (connect *audioctx*.destination))
  (*osc*.start))

(defun.ps start-play ()
  (init-osc-if-required)
  (when (string= *audioctx*.state "suspended")
    (*audioctx*.resume))
  (reset-canvas)
  (let* ((t0 *audioctx*.current-time)
         (t1 (+ t0 (parse-float (get-value "atk"))))
         (d (parse-float (get-value "dcy")))
         (s (parse-float (get-value "sus")))
         (gain *gain*.gain))
    (gain.set-value-at-time 0 t0)
    (gain.linear-ramp-to-value-at-time 1 t1)
    (gain.set-target-at-time s t1 d)))

(defun.ps start-decrease-to-zero ()
  (let ((t0 *audioctx*.current-time)
        (r (parse-float (get-value "rel")))
        (gain *gain*.gain))
    (when gain.cancel-and-hold-at-time
      (gain.cancel-and-hold-at-time t0))
    (gain.set-target-at-time 0 t0 r)))
