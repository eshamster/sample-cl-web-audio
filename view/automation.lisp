(defpackage sample-cl-web-audio/view/automation
  (:use :cl
        :cl-markup)
  (:export :make-automation-router)
  (:import-from :sample-cl-web-audio/js/automation)
  (:import-from :sample-cl-web-audio/view/utils
                :with-cl-markup))
(in-package :sample-cl-web-audio/view/automation)

(defun make-automation-router (&key js-path)
  (assert js-path)
  (lambda (params)
    (declare (ignore params))
    (with-output-to-string (str)
      (let ((cl-markup:*output-stream* str)
            (title "Samples of Parameter and Automation"))
        (html5 (:head
                (:title title))
               (:body
                (:h1 title)
                (:div
                 (:a :href "https://www.g200kg.com/jp/docs/webaudio/audioparam.html"
                     "https://www.g200kg.com/jp/docs/webaudio/audioparam.html"))
                (:ul (:li "During mouse down")
                     (:ul (:li "Volume up over \"Attack\" time")
                          (:li "After that volume down to \"Sustain\" over \"Decay\" time"))
                     (:li "After mouse up")
                     (:ul (:li "Volume down to 0 over \"Release\" time")))
                (:table
                 (:tr (:td "Attack")
                      (:td (:input :type "range" :id "atk"
                                   :min 0 :max 5 :step 0.01 :value 0.3))
                      (:td :id "atkval" 0.3))
                 (:tr (:td "Decay")
                      (:td (:input :type "range" :id "dcy"
                                   :min 0 :max 5 :step 0.01 :value 1))
                      (:td :id "dcyval" 1))
                 (:tr (:td "Sustain")
                      (:td (:input :type "range" :id "sus"
                                   :min 0 :max 1 :step 0.01 :value 0.5))
                      (:td :id "susval" 0.5))
                 (:tr (:td "Decay after Release")
                      (:td (:input :type "range" :id "rel"
                                   :min 0 :max 5 :step 0.01 :value 1))
                      (:td :id "relval" 1)))

                (:div
                 (:button :id "play-btn" "Play"))
                (:canvas :id "canvas" :width 500 :height 256 nil)
                (:script :src js-path nil)))))))
