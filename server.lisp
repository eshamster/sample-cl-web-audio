(defpackage sample-cl-web-audio/server
  (:use :cl
        :cl-markup)
  (:export :start
           :stop)
  (:import-from :sample-cl-web-audio/js/automation)
  (:import-from :sample-cl-web-audio/js/simple
                :play
                :setup)
  (:import-from :alexandria
                :make-keyword)
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

(defun make-js-main-file (target)
  (with-open-file (out *js-main-file*
                       :direction :output
                       :if-exists :supersede
                       :if-does-not-exist :create)
    (princ (eval `(pse:with-use-ps-pack
                      (,(make-keyword (format nil "~A/~A"
                                              :sample-cl-web-audio/js target)))))
           out)))

;; --- Server --- ;;

(defvar *app* (make-instance 'ningle:<app>))

(defvar *server* nil)

(setf (ningle:route *app* "/" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "Samples of Web Audio API"))
                   (:body
                    (:ul (dolist (sample (list "simple" "automation"))
                           (markup (:li (:a :href (format nil "/~A" sample) sample)))))))))))

(setf (ningle:route *app* "/simple" :method :GET)
      (lambda (params)
        (declare (ignorable params))
        (make-js-main-file :simple)
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str)
                (setup-func (funcall-to-full-js-string 'setup)))
            (html5 (:head
                    (:title "sample-cl-web-audio"))
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

                    (:script :src "js/main.js" nil)))))))

(setf (ningle:route *app* "/automation" :method :GET)
      (lambda (params)
        (declare (ignore params))
        (make-js-main-file :automation)
        (with-output-to-string (str)
          (let ((cl-markup:*output-stream* str)
                (setup-func (funcall-to-full-js-string 'setup))
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
