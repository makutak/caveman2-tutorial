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
           :relationship-follower
           :relationship-followed))
(in-package :caveman2-tutorial.model.relationship)

(defclass relationship ()
  ((follower-id :col-type :integer
            :initarg :follower-id
            :accessor relationship-follower)
   (followed-id  :col-type :integer
                   :initarg :followed-id
                   :accessor relationship-followed))
  (:metaclass mito:dao-table-class))