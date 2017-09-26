(in-package :cl-user)
(defpackage caveman2-tutorial.model.relationship
  (:use :cl
        :caveman2-tutorial.db
        :caveman2-tutorial.util
        :caveman2-tutorial.config
        :caveman2-tutorial.model.user
        :mito
        :sxql
        :local-time)

  (:export :relationship
           :relationship-follower-id
           :relationship-followed-id))
(in-package :caveman2-tutorial.model.relationship)

(defclass relationship ()
  ((follower-id :references (user id)
                :initarg :follower-id
                :accessor relationship-follower-id)
   (followed-id :references (user id)
                :initarg :followed-id
                :accessor relationship-followed-id))
  (:metaclass mito:dao-table-class))

(defun follow ())

(defun unfollow ())

(defun following-p ())
