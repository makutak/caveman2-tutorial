(in-package :cl-user)
(defpackage caveman2-tutorial.view
  (:use :cl)
  (:import-from :caveman2-tutorial.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :djula
                :add-template-directory
                :compile-template*
                :render-template*
                :*djula-execute-package*
                :*current-language*)
  (:import-from :datafly
                :encode-json)
  (:export :render
           :render-json))
(in-package :caveman2-tutorial.view)

(djula:add-template-directory *template-directory*)

(defparameter *template-registry* (make-hash-table :test 'equal))
(defparameter *current-language* :ja)

(defun render (template-path &optional env)
  (let ((template (gethash template-path *template-registry*)))
    (unless template
      (setf template (djula:compile-template* (princ-to-string template-path)))
      (setf (gethash template-path *template-registry*) template))
    (apply #'djula:render-template*
           template nil
           env)))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))


;;
;; Execute package definition

(defpackage caveman2-tutorial.djula
  (:use :cl)
  (:import-from :caveman2-tutorial.config
                :config
                :appenv
                :developmentp
                :productionp)
  (:import-from :caveman2
                :url-for)
  (:import-from :ironclad
                :byte-array-to-hex-string)
  (:import-from :ironclad
                :digest-sequence)
  (:import-from :ironclad
                :ascii-string-to-byte-array))

(setf djula:*djula-execute-package* (find-package :caveman2-tutorial.djula))


;;
;; Custom fileter

(djula::def-filter :md5-hexdigest (it)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence :md5 (ironclad:ascii-string-to-byte-array it))))
