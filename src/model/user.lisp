(in-package :cl-user)
(defpackage caveman2-tutorial.model.user
  (:use :cl
        :caveman2-tutorial.db
        :mito
        :sxql))
(in-package :caveman2-tutorial.model.user)

(defclass user ()
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (or (:varchar 128) :null)
          :initarg :email
          :accessor user-email))
  (:metaclass mito:dao-table-class))
