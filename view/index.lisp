(defpackage sample-cl-web-audio/view/index
  (:use :cl
        :cl-markup)
  (:export :make-index-router)
  (:import-from :sample-cl-web-audio/view/utils
                :with-cl-markup))
(in-package :sample-cl-web-audio/view/index)

(defun make-index-router ()
  (lambda (params)
    (declare (ignore params))
    (with-cl-markup ()
      (let ((ref-url "https://www.g200kg.com/jp/docs/webaudio/generatesound.html"))
        (html5 (:head
                (:title "Samples of Web Audio API"))
               (:body
                (:div "This sample refers to "
                      (:a :href ref-url :target "_blank" ref-url))
                (:ul (dolist (sample (list "simple"
                                           "automation"
                                           "biquad-filter"
                                           "custom-waveform"))
                       (markup (:li (:a :href (format nil "/~A" sample) sample)))))))))))
