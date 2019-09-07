(defpackage sample-cl-web-audio/server
  (:use :cl
        :cl-markup
        :sample-cl-web-audio/js)
  (:export :start
           :stop)
  (:import-from :sample-cl-web-audio/js
                :play
                :setup)
  (:import-from :ps-experiment
                :funcall-to-full-js-string))
(in-package :sample-cl-web-audio/server)

;; --- Definitions about directories --- ;;

(defvar *script-dir*
  (merge-pathnames "static/"
                   (asdf:component-pathname
                    (asdf:find-system :sample-cl-web-audio))))

(defvar *js-dir*
  (merge-pathnames "js/" *script-dir*))

(defvar *js-main-file*
  (merge-pathnames "main.js" *js-dir*))

;; --- Make js main file --- ;;

(defun make-js-main-file ()
  (with-open-file (out *js-main-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (princ
     (pse:with-use-ps-pack (:sample-cl-web-audio/js))
     out)))

;; --- Server --- ;;

(defvar *app* (make-instance 'ningle:<app>))

(defvar *server* nil)

(setf (ningle:route *app* "/" :method :GET)
      (lambda (params)
        (declare (ignorable params))
        (make-js-main-file)
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str)
                (setup-func (funcall-to-full-js-string 'setup)))
            (html5 (:head
                    (:title "sample-cl-web-audio"))
                   (:body
                    (:h1 "Hello Web Audio API")
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

                    (:button :onclick (funcall-to-full-js-string 'play) "Play")
                    (:script :src "js/main.js" nil)))))))

(defun stop ()
  (when *server*
    (clack:stop *server*)
    (setf *server* nil)))

(defun start (&key (port 5000) (address "0.0.0.0"))
  (stop)
  (setf *server*
        (clack:clackup
         (lack:builder
          (:static :path (lambda (path)
                           (print path)
                           (if (ppcre:scan "^(?:/images/|/css/|/js/|/robot\\.txt$|/favicon\\.ico$)"
                                           path)
                               path
                               nil))
                   :root *script-dir*)
          *app*)
         :port port
         :address address)))
