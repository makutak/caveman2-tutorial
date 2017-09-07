(in-package :cl-user)
(defpackage caveman2-tutorial.util
  (:use :cl
        :caveman2
        :caveman2-tutorial.config
        :caveman2-tutorial.db
        :mito
        :sxql)
  (:export :get-value-from-params
           :flash
           :current-user-id
           :reset-current-user
           :log-in
           :logged-in-p
           :logged-in-user
           :correct-user
           :admin-p
           :redirect-back-or
           :store-location))
(in-package :caveman2-tutorial.util)


;;
;; Util functions

(defun flash (&optional value)
  (if value
      (setf (gethash :flash *session*) value)
      (let ((msg (gethash :flash *session*)))
        (remhash :flash *session*)
        msg)))

(defun get-value-from-params (key params)
  (cdr (assoc key params :test #'string=)))

(defun current-user-id ()
  (gethash :user-id *session* nil))

(defun reset-current-user ()
  (remhash :user-id *session*))

(defun log-in (user)
  (reset-current-user)
  (setf (gethash :user-id *session*) (object-id user)))

(defun logged-in-p ()
  (not (null (current-user-id))))

(defun logged-in-user ()
  (unless (logged-in-p)
    (progn
      (store-location)
      (flash "Please login.")
      (redirect "/login"))))

(defun correct-user (id)
  (unless (equal
           (format nil "~A" (current-user-id))
           (format nil "~A" id))
    (redirect "/home")))

(defun admin-p ()
  (user-admin (find-dao 'user :id (gethash :user-id *session* nil))))

(defun redirect-back-or (default)
  (let ((forwarding-url (gethash :forwarding-url *session* nil)))
    (remhash :forwarding-url *session*)
    (redirect
     (or forwarding-url
         default))))

(defun store-location ()
  (if (equal "GET" (format nil "~A" (request-method *request*)))
      (setf (gethash :forwarding-url *session*)
            (request-path-info *request*))))
