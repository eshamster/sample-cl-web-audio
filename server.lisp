(defpackage sample-cl-web-audio/server
  (:use :cl
        :cl-markup)
  (:export :start
           :stop)
  (:import-from :sample-cl-web-audio/view/index
                :make-index-router)
  (:import-from :sample-cl-web-audio/view/simple
                :make-simple-router)
  (:import-from :sample-cl-web-audio/view/automation
                :make-automation-router)
  (:import-from :alexandria
                :make-keyword))
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
      (make-index-router))

(setf (ningle:route *app* "/simple" :method :GET)
      (lambda (params)
        (make-js-main-file :simple)
        (funcall (make-simple-router :js-path "js/main.js")
                 params)))

(setf (ningle:route *app* "/automation" :method :GET)
      (lambda (params)
        (make-js-main-file :automation)
        (funcall (make-automation-router :js-path "js/main.js")
                 params)))

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
