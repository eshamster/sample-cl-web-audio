(defpackage sample-cl-web-audio/server
  (:use :cl
        :cl-markup
        :sample-cl-web-audio/js)
  (:export :start
           :stop))
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
          (let ((cl-markup:*output-stream* str))
            (html5 (:head
                    (:title "sample-cl-web-audio"))
                   (:body
                    (:h1 "Hello ps-experiment!!")
                    (:button :onclick "sampleClWebAudio_js.play()" "Play")
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
