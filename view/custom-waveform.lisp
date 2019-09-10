(defpackage sample-cl-web-audio/view/custom-waveform
  (:use :cl
        :cl-markup)
  (:export :make-custom-waveform-router)
  (:import-from :sample-cl-web-audio/js/custom-waveform)
  (:import-from :sample-cl-web-audio/view/utils
                :with-cl-markup))
(in-package :sample-cl-web-audio/view/custom-waveform)

(defun make-custom-waveform-router (&key js-path)
  (assert js-path)
  (lambda (params)
    (declare (ignore params))
    (with-cl-markup ()
      (let ((title "Oscillator Custom waveform")
            (ref-url "https://www.g200kg.com/jp/docs/webaudio/osccustom.html"))
        (html5 (:head
                (:title title))
               (:h1 title)
               (:body
                (:div
                 (:a :href ref-url ref-url))
                (:table
                 (:tr (:th "Freq")
                      (:td (:input :type "range" :id "freq"
                                   :min 50 :max 1000 :value 440))
                      (:td :id "freqval" 440))
                 (:tr (:th "Gain")
                      (:td (:input :type "range" :id "gain"
                                   :min 0 :max 1 :step 0.01 :value 0.5))
                      (:td :id "gainval" 0.5)))
                (:br)
                (:table
                 (:tr (:th "Harmonics")
                      (:th "real")
                      (:th nil)
                      (:th "imag")
                      (:th nil))
                 (dotimes (i 8)
                   (let ((val (case i
                                (1 1)
                                (2 0.5)
                                (t 0)))
                         (id-real (format nil "real~D" i))
                         (id-imag (format nil "imag~D" i)))
                     (markup
                      (:tr (:th i)
                           (:td (:input :type "range" :id id-real
                                        :min 0 :max 1 :step 0.01 :value 0))
                           (:td :id (concatenate 'string id-real "val") 0)
                           (:td (:input :type "range" :id id-imag
                                        :min 0 :max 1 :step 0.01 :value val))
                           (:td :id (concatenate 'string id-imag "val") val))))))

                (:div
                 (:button :id "play-btn" "Play"))
                (:canvas :id "canvas" :width 512 :height 256 nil)
                (:script :src js-path nil)))))))
