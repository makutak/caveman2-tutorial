(in-package :cl-user)
(defpackage caveman2-tutorial.util
  (:use :cl
        :caveman2
        :caveman2-tutorial.config
        :caveman2-tutorial.db
        :mito
        :sxql)
  (:import-from :ironclad
                :byte-array-to-hex-string)
  (:import-from :ironclad
                :digest-sequence)
  (:import-from :ironclad
                :ascii-string-to-byte-array)
  (:export :get-value-from-params
           :make-md5-hexdigest))
(in-package :caveman2-tutorial.util)


;;
;; Util functions


(defun get-value-from-params (key params)
  (cdr (assoc key params :test #'string=)))

(defun make-md5-hexdigest (string)
  (byte-array-to-hex-string
   (digest-sequence :md5 (ascii-string-to-byte-array string))))
