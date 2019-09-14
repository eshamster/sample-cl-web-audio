(defpackage sample-cl-web-audio/view/wave-shaper
  (:use :cl
        :cl-markup)
  (:export :make-wave-shaper-router)
  (:import-from :sample-cl-web-audio/js/wave-shaper)
  (:import-from :sample-cl-web-audio/view/utils
                :with-cl-markup))
(in-package :sample-cl-web-audio/view/wave-shaper)

(defun make-wave-shaper-router (&key js-path)
  (assert js-path)
  (lambda (params)
    (declare (ignore params))
    (with-cl-markup ()
      (let ((title "Wave Shaper")
            (ref-url "https://www.g200kg.com/jp/docs/webaudio/waveshaper.html"))
        (html5 (:head
                (:title title))
               (:h1 title)
               (:body
                (:div
                 (:a :href ref-url ref-url))
                (:table
                 (:tr (:th "Step")
                      (:td (:input :type "range" :id "steps"
                                   :min 2 :max 32 :value 4))
                      (:td :id "stepsval" 4))
                 (:tr (:th "Gain")
                      (:td (:input :type "range" :id "gain"
                                   :min 0 :max 1 :step 0.01 :value 0.5))
                      (:td :id "gainval" 0.5)))

                (:div
                 (:button :id "play-btn" "Play"))
                (:canvas :id "canvas" :width 512 :height 256 nil)
                (:script :src js-path nil)))))))
