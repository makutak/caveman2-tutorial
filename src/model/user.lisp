(in-package :cl-user)
(defpackage caveman2-tutorial.model.user
  (:use :cl
        :caveman2-tutorial.db
        :caveman2-tutorial.util
        :caveman2-tutorial.config
        :mito
        :mito-auth
        :sxql
        :local-time
        :cl-csv)

  (:export :user
           :user-name
           :user-email
           :user-birth-date
           :user-admin
           :create-user
           :update-user
           :valid-user
           :authenticate-user))
(in-package :caveman2-tutorial.model.user)

(defclass user (has-secure-password)
  ((name :col-type (:varchar 64)
         :initarg :name
         :accessor user-name)
   (email :col-type (:varchar 128)
          :initarg :email
          :accessor user-email)
   (birth-date :col-type (:datetime)
               :initargs :birth-date
               :accessor user-birth-date)
   (admin :col-type :boolean
          :initarg :admin
          :initform nil
          :accessor user-admin))
  (:metaclass mito:dao-table-class)
  (:unique-keys name email))

(defun create-user (params)
  (setf new-user
        (make-instance 'user
                       :name (get-value-from-params "name" params)
                       :email (get-value-from-params "email" params)
                       :password (get-value-from-params "password" params)
                       :birth-date (parse-timestring "1992-03-06")))
  (with-connection (db) (insert-dao new-user)))

(defmethod update-user ((this user) params)
  (setf (slot-value this 'name) (get-value-from-params "name" params))
  (setf (slot-value this 'email) (get-value-from-params "email" params))
  (setf (password this) (get-value-from-params "password" params))
  (with-connection (db) (save-dao this)))

(defmacro valid (key params)
  `(not (= 0 (length (get-value-from-params ,key ,params)))))

(defun valid-user (params)
  (and (valid "name" params)
       (valid "email" params)
       (valid "password" params)))

(defun seed-user ()
  (read-csv (merge-pathnames #P"seed-user.csv" *database-directory*)
            :map-fn #'(lambda (row)
                        (with-connection (db)
                          (create-dao 'user
                                      :name (nth 0 row)
                                      :email (nth 1 row)
                                      :birth-date (parse-timestring (nth 2 row))
                                      :password "password")))))
