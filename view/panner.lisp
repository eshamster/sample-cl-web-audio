(defpackage sample-cl-web-audio/view/panner
  (:use :cl
        :cl-markup)
  (:export :make-panner-router)
  (:import-from :sample-cl-web-audio/js/panner)
  (:import-from :sample-cl-web-audio/view/utils
                :with-cl-markup))
(in-package :sample-cl-web-audio/view/panner)

(defun make-panner-router (&key js-path)
  (assert js-path)
  (lambda (params)
    (declare (ignore params))
    (with-cl-markup ()
      (let ((title "Stereo Panner")
            (ref-url "https://www.g200kg.com/jp/docs/webaudio/panner.html"))
        (html5 (:head
                (:title title))
               (:h1 title)
               (:body
                (:div
                 (:a :href ref-url ref-url))
                (:div "The sample in the above URL uses \"Panner\", but the following sample uses \"Stereo Panner\"")
                (:table
                 (:tr (:th "Gain")
                      (:td (:input :type "range" :id "gain"
                                   :min 0 :max 1 :step 0.01 :value 0.5))
                      (:td :id "gainval" 0.5))
                 (:tr (:th "Pan")
                      (:td (:input :type "range" :id "pan"
                                   :min -1 :max 1 :step 0.01 :value 0))
                      (:td :id "panval" 0)))

                (:div
                 (:button :id "play-btn" "Play"))
                (:script :src js-path nil)))))))
