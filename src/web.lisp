(in-package :cl-user)
(defpackage caveman2-tutorial.web
  (:use :cl
        :caveman2
        :caveman2-tutorial.config
        :caveman2-tutorial.view
        :caveman2-tutorial.db
        :caveman2-tutorial.util
        :caveman2-tutorial.model.user
        :mito
        :sxql)
  (:import-from :lack.component
                :call)
  (:export :*web*))
(in-package :caveman2-tutorial.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(defmethod lack.component:call :around ((app <web>) env)
  (with-connection (db)
    (call-next-method)))
(clear-routing-rules *web*)

;;
;; Routing rules

(defun render-with-current (template-path &optional args)
  (render template-path (append args (list :current
                                           (current-user-id)))))

(defroute "/" ()
  (redirect "/home"))

(defroute "/home" ()
  (render-with-current #P"static_pages/home.html"))

(defroute "/flash" ()
  (format nil "~A" (gethash :flash *session*)))

(defroute "/help" ()
  (render-with-current #P"static_pages/help.html"))

(defroute "/about" ()
  (render-with-current #P"static_pages/about.html"))

(defroute "/signup" ()
  (redirect "/users/new"))

(defroute "/users" ()
  (setf users (retrieve-dao 'user))
  (render-with-current #P"users/index.html"
                       (list :users
                             (mapcar #'(lambda (user)
                                         (list :name (user-name user)
                                               :email (make-md5-hexdigest
                                                       (user-email user))))
                                     users))))

(defroute "/users/new" ()
  (flash "Please input infomation.")
  (render-with-current #P"users/new.html"
                       (list :flash (flash)
                             :type "info")))

(defroute ("/users/create" :method :POST) (&key _parsed)
  (setf params (get-value-from-params "user" _parsed))
  (when (valid-user params)
    (if (gethash :user-id *session*)
        (reset-current-user))
    (log-in (create-user params))
    (flash "Welcome to the Sample App!")
    (redirect-back-or (format nil "/users/~A" (current-user-id))))
  (redirect "/users/new"))

(defroute "/users/:id" (&key id)
  (setf u (find-dao 'user :id id))
  (if (null u)
      (render-with-current #P"_errors/404.html")
      (render-with-current #P"users/show.html"
                           (append (user-info u)
                                   (list :flash (flash) :type "success")))))

(defroute "/users/:id/edit" (&key id)
  (logged-in-user)
  (correct-user id)
  (setf current-user (find-dao 'user :id id))
  (render-with-current #P"users/edit.html"
          (list :user (list :id (object-id current-user)
                            :name (user-name current-user)
                            :email (user-email current-user)
                            :hash-email (make-md5-hexdigest
                                         (user-email current-user))))))

(defroute ("/users/:id/update" :method :POST) (&key id _parsed)
  (logged-in-user)
  (correct-user id)
  (setf params (get-value-from-params "user" _parsed))
  (when (valid-user params)
    (update-user (find-dao 'user :id (current-user-id)) params)
    (flash "update success")
    (redirect (format nil "/users/~A" (current-user-id))))
  (redirect (format nil  "/users/~A/edit" (current-user-id))))

(defroute "/login" ()
  (render-with-current #P"sessions/new.html"
                       (list :flash (flash)
                             :type "danger")))

(defroute ("/login" :method :POST) (&key _parsed)
  (setf params (get-value-from-params "session" _parsed))
  (setf login-user (find-dao 'user
                             :email
                             (get-value-from-params "email" params)))
  (when login-user
    (when (authenticate-user login-user
                             (get-value-from-params "password" params))
      (log-in login-user)
      (flash "Welcome to the Sample App!")
      (redirect-back-or (format nil  "/users/~A" (current-user-id)))))
  (flash "Invalid email/password combination")
  (render-with-current #P"sessions/new.html"
                       (list :flash (flash)
                             :type "danger")))

(defroute ("/logout" :method :POST) ()
  (reset-current-user)
  (redirect "/home"))

(defroute "/api/users" ()
  (setf users (retrieve-dao 'user))
  (render-json users))

(defroute "/api/user/:id" (&key id)
  (setf user (find-dao 'user :id id))
  (render-json user))

(defroute "/counter" ()
  (format nil "You came here ~A times."
          (incf (gethash :counter *session* 0))))

;;
;; Helper functions
(defun current-user-id ()
  (gethash :user-id *session* nil))

(defun reset-current-user ()
  (remhash :user-id *session*)
  (remhash :forwarding-url *session*))

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
;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
