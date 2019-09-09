(defpackage sample-cl-web-audio/view/biquad-filter
  (:use :cl
        :cl-markup)
  (:export :make-biquad-filter-router)
  (:import-from :sample-cl-web-audio/js/biquad-filter)
  (:import-from :sample-cl-web-audio/view/utils
                :with-cl-markup))
(in-package :sample-cl-web-audio/view/biquad-filter)

(defun make-biquad-filter-router (&key js-path)
  (assert js-path)
  (lambda (params)
    (declare (ignore params))
    (with-cl-markup ()
      (let ((title "Samples of BiquadFilter")
            (ref-url "https://www.g200kg.com/jp/docs/webaudio/audioparam.html"))
        (html5 (:head
                (:title title))
               (:body
                (:div
                 (:a :href ref-url ref-url))
                (:table
                 (:tr (:td "Type")
                      (:td (:select :id "type"
                                    (:option "LPF")
                                    (:option "HPF")
                                    (:option "BPF")
                                    (:option "LowShelf")
                                    (:option "HighShelf")
                                    (:option "Peaking")
                                    (:option "Notch")
                                    (:option "AllPass"))))
                 (:tr (:td "Freq")
                      (:td (:input :type "range" :id "freq"
                                   :min 100 :max 20000 :value 5000))
                      (:td :id "freqval" 5000))
                 (:tr (:td "Q")
                      (:td (:input :type "range" :id "q"
                                   :min 0 :max 50 :step 0.1 :value 5))
                      (:td :id "qval" 5))
                 (:tr (:td "Gain")
                      (:td (:input :type "range" :id "gain"
                                   :min -50 :max 50 :value 0))
                      (:td :id "gainval" 0)))

                (:div
                 (:button :id "play-btn" "Play Noise"))
                (:canvas :id "canvas" :width 512 :height 256 nil)
                (:script :src js-path nil)))))))
