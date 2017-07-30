(in-package :cl-user)
(defpackage caveman2-tutorial.model.user
  (:use :cl
        :caveman2-tutorial.db
        :mito
        :sxql
        :local-time)
  (:export :user-name
           :user-email
           :user-birth-date
           :find-user
           :find-users))
(in-package :caveman2-tutorial.model.user)

(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (:varchar 128)
          :initarg :email
          :accessor user-email)
   (birth-date :col-type (:datetime)
               :initargs :birty-date
               :accessor user-birth-date)
   (password :col-type (:varchar 16)
             :initarg :password))
  (:metaclass mito:dao-table-class)
  (:unique-key name email))


(defun find-user (id)
  (with-connection (db)
    (find-dao 'user :id id)))

(defun find-users ()
  (with-connection (db)
    (retrieve-dao 'user)))
