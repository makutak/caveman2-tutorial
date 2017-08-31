(in-package :cl-user)
(defpackage caveman2-tutorial.web
  (:use :cl
        :caveman2
        :caveman2-tutorial.config
        :caveman2-tutorial.view
        :caveman2-tutorial.db
        :caveman2-tutorial.util
        :caveman2-tutorial.model.user
        :caveman2-tutorial.model.micropost
        :mito
        :mito-auth
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

(defvar limit-number 30)

(defroute "/users" (&key |page|)
  (logged-in-user)
  (setf query (or |page|
                  "1"))
  (handler-case (setf current-page (parse-integer query))
    (error (c) (on-exception *web* 404)))
  (if (>= 0 current-page)
      (throw-code 404))
  (setf users (select-dao 'user
                (limit limit-number)
                (offset (* limit-number (if (= current-page 1)
                                            0
                                            (1- current-page))))))
  (if (null users)
      (on-exception *web* 404)
      (render-with-current #P"users/index.html"
                           (append
                            (list :next-page (1+ current-page)
                                  :users users)
                            (list :flash (flash) :type "success")
                            (list :admin (user-admin (find-dao 'user :id (current-user-id))))))))

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
  (setf user (find-dao 'user :id id))
  (setf posts (select-dao 'micropost
                (includes 'user)
                (where (:= :user user))
                (order-by (:desc :created-at))))
  (if (null user)
      (render-with-current #P"_errors/404.html")
      (render-with-current #P"users/show.html"
                           (append (list :user user)
                                   (list :posts posts)
                                   (list :flash (flash) :type "success")))))

(defroute "/users/:id/edit" (&key id)
  (logged-in-user)
  (correct-user id)
  (setf current-user (find-dao 'user :id id))
  (render-with-current #P"users/edit.html"
                       (append
                        (list :flash (flash) :type "success")
                        (list :user current-user))))

(defroute ("/users/:id/update" :method :POST) (&key id _parsed)
  (logged-in-user)
  (correct-user id)
  (setf params (get-value-from-params "user" _parsed))
  (when (valid-user params)
    (update-user (find-dao 'user :id (current-user-id)) params)
    (flash "update success")
    (redirect (format nil "/users/~A" (current-user-id))))
  (redirect (format nil  "/users/~A/edit" (current-user-id))))

(defroute  ("/users/:id/delete" :method :POST) (&key id)
  (logged-in-user)
  (when (admin-p)
      (handler-case (delete-by-values 'user :id id)
        (error (c) (on-exception *web* 404)))
      (flash "User deleted")
      (redirect "/users" ))
  (redirect "/home"))

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
    (when (auth login-user (get-value-from-params "password" params))
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

(defroute "/api/users/:id" (&key id)
  (setf user (find-dao 'user :id id))
  (render-json user))

(defroute "/api/users/:id/posts" (&key id)
  (setf posts (select-dao 'micropost
                (includes 'user)
                (where (:= :user-id id))
                (order-by (:desc :created-at))))
  (render-json posts))

;;
;; Helper functions
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
;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
