#|
  This file is a part of sample-cl-web-audio project.
  Copyright (c) 2018 eshamster (hamgoostar@gmail.com)
|#

#|
  The sample-cl-web-audio is a sample project of Web Audio API using Parenscript

  Author: eshamster (hamgoostar@gmail.com)
|#

(defsystem sample-cl-web-audio
  :version "0.1"
  :class :package-inferred-system
  :author "eshamster"
  :license "MIT"
  :depends-on (:parenscript
               :ps-experiment
               :alexandria
               :ningle
               :cl-markup
               :clack
               :sample-cl-web-audio/main)
  :description "The sample-cl-web-audio is a sample project of Web Audio API using Parenscript"
  :long-description
  #.(with-open-file (stream (merge-pathnames
                             #p"README.markdown"
                             (or *load-pathname* *compile-file-pathname*))
                            :if-does-not-exist nil
                            :direction :input)
      (when stream
        (let ((seq (make-array (file-length stream)
                               :element-type 'character
                               :fill-pointer t)))
          (setf (fill-pointer seq) (read-sequence seq stream))
          seq)))
  :in-order-to ((test-op (test-op sample-cl-web-audio/t))))

(defsystem sample-cl-web-audio/t
  :class :package-inferred-system
  :depends-on (:ps-experiment-test
               :rove
               "ps-experiment/t/test-utils"
               "sample-cl-web-audio/t/main")
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
