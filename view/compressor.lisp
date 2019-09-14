(defpackage sample-cl-web-audio/view/compressor
  (:use :cl
        :cl-markup)
  (:export :make-compressor-router)
  (:import-from :sample-cl-web-audio/js/compressor)
  (:import-from :sample-cl-web-audio/view/utils
                :with-cl-markup))
(in-package :sample-cl-web-audio/view/compressor)

(defun make-compressor-router (&key js-path)
  (assert js-path)
  (lambda (params)
    (declare (ignore params))
    (with-cl-markup ()
      (let ((title "Dynamic Compressor")
            (ref-url "https://www.g200kg.com/jp/docs/webaudio/compressor.html"))
        (html5 (:head
                (:title title))
               (:h1 title)
               (:body
                (:div
                 (:a :href ref-url ref-url))
                (:table
                 (:tr (:th "Threshold")
                      (:td (:input :type "range" :id "thresh"
                                   :min -100 :max 0 :step 0.1 :value -50))
                      (:td :id "threshval" -50))
                 (:tr (:th "Knee")
                      (:td (:input :type "range" :id "knee"
                                   :min 0 :max 40 :step 0.1 :value 20))
                      (:td :id "kneeval" 20))
                 (:tr (:th "Ratio")
                      (:td (:input :type "range" :id "ratio"
                                   :min 1 :max 20 :step 0.1 :value 12))
                      (:td :id "ratioval" 12))
                 (:tr (:th "Attack")
                      (:td (:input :type "range" :id "atk"
                                   :min 0 :max 0.1 :step 0.001 :value 0.003))
                      (:td :id "atkval" 0.003))
                 (:tr (:th "Release")
                      (:td (:input :type "range" :id "rel"
                                   :min 0 :max 1 :step 0.01 :value 0.25))
                      (:td :id "relval" 0.25)))

                (:div
                 (:button :id "play-btn" "Play"))
                (:canvas :id "canvas" :width 364 :height 364 nil)
                (:script :src js-path nil)))))))
