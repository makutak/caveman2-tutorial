(in-package :cl-user)
(defpackage caveman2-tutorial.model.micropost
  (:use :cl
        :caveman2-tutorial.db
        :caveman2-tutorial.util
        :caveman2-tutorial.config
        :caveman2-tutorial.model.user
        :mito
        :sxql
        :local-time
        :cl-csv)

  (:export :micropost
           :user-content
           :micropost-user))
(in-package :caveman2-tutorial.model.micropost)

(defclass micropost ()
  ((content :col-type :text
            :initarg :content
            :accessor user-content)
   (user :col-type user
         :initarg :user
         :accessor micropost-user))
  (:metaclass mito:dao-table-class))
