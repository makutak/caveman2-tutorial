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

;;
;; 1 page limit

(defvar limit-number 30)

;;
;; Pagination

(defmacro paginate (model current-page &body body)
  `(select-dao ,model
     ,@body
     (limit limit-number)
     (offset (* limit-number (if (= ,current-page 1)
                                 0
                                 (1- ,current-page))))))

(defroute "/" ()
  (redirect "/home"))

(defroute "/home" ()
  (render-with-current #P"static_pages/home.html"
                       (if (logged-in-p)
                           (list :current-user (find-dao 'user :id (current-user-id))
                                 :posts (count-dao 'micropost :user-id (current-user-id))
                                 :feed-items (select-dao 'micropost
                                               (includes 'user)
                                               (where (:= :user-id (current-user-id)))
                                               (order-by (:desc :created-at)))
                                 :flash (flash) :type"success"))))

(defroute "/help" ()
  (render-with-current #P"static_pages/help.html"))

(defroute "/about" ()
  (render-with-current #P"static_pages/about.html"))

(defroute "/signup" ()
  (redirect "/users/new"))

;;
;; User

(defroute "/users" (&key |page|)
  (logged-in-user)
  (setf query (or |page| "1"))
  (handler-case (setf current-page (parse-integer query))
    (error (c) (on-exception *web* 404)))
  (if (>= 0 current-page)
      (throw-code 404))
  (setf users (paginate 'user current-page))
  (if (null users)
      (on-exception *web* 404)
      (render-with-current #P"users/index.html"
                           (list :next-page (1+ current-page)
                                 :users users
                                 :flash (flash) :type "success"
                                 :admin (user-admin (find-dao 'user :id (current-user-id)))))))

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
  (if (null user)
      (throw-code 404))
  (setf posts (select-dao 'micropost
                (includes 'user)
                (where (:= :user user))
                (order-by (:desc :created-at))))
  (render-with-current #P"users/show.html"
                       (list :user user
                             :posts posts
                             :flash (flash) :type "success")))

(defroute "/users/:id/edit" (&key id)
  (logged-in-user)
  (correct-user id)
  (setf current-user (find-dao 'user :id id))
  (render-with-current #P"users/edit.html"
                       (list :flash (flash) :type "success"
                             :user current-user)))

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

;;
;; Micropost

(defroute ("/microposts/create" :method :POST) (&key _parsed)
  (logged-in-user)
  (setf params (get-value-from-params "micropost" _parsed))
  (setf post (make-instance 'micropost
                             :content (get-value-from-params "content" params)
                             :user (find-dao 'user :id (current-user-id))))
  (handler-case (insert-dao post)
    (error (c) (redirect "/home")))
  (flash "Micropost created!")
  (redirect "/home"))

(defroute ("/microposts/:id/delete" :method :POST) (&key id)
  (logged-in-user)
  (setf post (find-dao 'micropost :id id))
  (when (equal (current-user-id) (object-id (micropost-user post)))
    (delete-dao post)
    (flash "Micropost deleted"))
  (redirect "/home"))

;;
;; login, logout

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


;;
;; response check
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

;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
