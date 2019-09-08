(defpackage sample-cl-web-audio/view/simple
  (:use :cl
        :cl-markup)
  (:export :make-simple-router)
  (:import-from :sample-cl-web-audio/js/simple
                :play
                :setup)
  (:import-from :sample-cl-web-audio/view/utils
                :with-cl-markup)
  (:import-from :ps-experiment
                :funcall-to-full-js-string))
(in-package :sample-cl-web-audio/view/simple)

(defun make-simple-router (&key js-path)
  (assert js-path)
  (lambda (params)
    (declare (ignorable params))
    (with-output-to-string (str)
      (let ((cl-markup:*output-stream* str)
            (setup-func (funcall-to-full-js-string 'setup)))
        (html5 (:head
                (:title "Simple samples"))
               (:body
                (:h1 "Hello Web Audio API")
                (:button :onclick (funcall-to-full-js-string 'play) "Play")
                (:h2 "Base Oscillator")
                (:table
                 (:tr (:td "Type")
                      (:td (:select :id "type"
                                    :onchange setup-func
                                    (:option :value "sine"     "Sine")
                                    (:option :value "square"   "Square")
                                    (:option :value "sawtooth" "SawTooth")
                                    (:option :value "triangle" "Triangle")))
                      (:td nil))
                 (:tr (:td "Freq(Hz)")
                      (:td (:input :type "range" :id "freq"
                                   :min 50 :max 3000 :value 440
                                   :oninput setup-func))
                      (:td :id "freqdisp" 440))
                 (:tr (:td "Level")
                      (:td (:input :type "range" :id "level"
                                   :min 0 :max 1 :step 0.01 :value 0.5
                                   :oninput setup-func))
                      (:td :id "leveldisp" 0.5)))

                (:h2 "LFO Oscillator (Vibrato)")
                (:table
                 (:tr (:td "Freq(Hz)")
                      (:td (:input :type "range" :id "lfo-freq"
                                   :min 0.1 :max 20 :step 0.1 :value 5
                                   :oninput setup-func))
                      (:td :id "lfo-freqdisp" 5))
                 (:tr (:td "Depth")
                      (:td (:input :type "range" :id "lfo-depth"
                                   :min 0 :max 100 :value 10
                                   :oninput setup-func))
                      (:td :id "lfo-depthdisp" 10)))

                (:script :src js-path nil)))))))
