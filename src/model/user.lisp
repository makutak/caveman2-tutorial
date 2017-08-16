(in-package :cl-user)
(defpackage caveman2-tutorial.model.user
  (:use :cl
        :caveman2-tutorial.db
        :caveman2-tutorial.util
        :mito
        :sxql
        :local-time)
  (:import-from :ironclad
                :byte-array-to-hex-string)
  (:import-from :ironclad
                :digest-sequence)
  (:import-from :ironclad
                :ascii-string-to-byte-array)

  (:export :user
           :user-name
           :user-email
           :user-birth-date
           :user-info
           :create-user
           :valid-user
           :authenticate-user))
(in-package :caveman2-tutorial.model.user)

(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (:varchar 128)
          :initarg :email
          :accessor user-email)
   (birth-date :col-type (:datetime)
               :initargs :birth-date
               :accessor user-birth-date)
   (password :col-type (:varchar 256)
             :initarg :password))
  (:metaclass mito:dao-table-class)
  (:unique-keys name email))

(defun user-info (user-instance)
  (list :user (list :name (user-name user-instance)
                    :email (make-md5-hexdigest (user-email user-instance)))))

(defun create-user (params)
  (setf new-user
        (make-instance 'user
                       :name (get-value-from-params "name" params)
                       :email (get-value-from-params "email" params)
                       :password (cl-pass:hash
                                  (get-value-from-params "password" params))
                       :birth-date (parse-timestring "1992-03-06")))
  (with-connection (db) (insert-dao new-user)))

(defmacro valid (key params)
  `(not (= 0 (length (get-value-from-params ,key ,params)))))

(defun valid-user (params)
  (and (valid "name" params)
       (valid "email" params)
       (valid "password" params)))

(defun authenticate-user (user-instance input-password)
  (let ((password-hash (slot-value user-instance 'password)))
    (if password-hash
        (values (cl-pass:check-password input-password password-hash) t)
        (values nil nil))))
