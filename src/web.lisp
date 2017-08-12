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
  (:export :*web*
           :current-user))
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

(defroute "/" ()
  (redirect "/home"))

(defroute "/home" ()
  (render #P"static_pages/home.html"))

(defroute "/help" ()
  (render #P"static_pages/help.html"))

(defroute "/about" ()
  (render #P"static_pages/about.html"))

(defroute "/signup" ()
  (redirect "/users/new"))

(defroute "/users/new" ()
  (render #P"users/new.html"))

(defroute ("/users/create" :method :POST) (&key _parsed)
  (setf params (get-value-from-params "user" _parsed))
  (when (valid-user params)
    (if (gethash :user-id *session*)
        (reset-current-user))
    (log-in (create-user params))
    (redirect (format nil "/users/~A" (object-id (current-user)))))
  (redirect "/users/new"))

(defroute "/users/:id" (&key id)
  (setf u (find-dao 'user :id id))
  (if (null u)
      (render #P"_errors/404.html")
      (render #P"users/show.html" (user-info u))))

(defroute "/login" ()
  (render #P"sessions/new.html"))

(defroute ("/login" :method :POST) (&key _parsed)
  (setf params (get-value-from-params "session" _parsed))
  (setf login-user (find-dao 'user
                             :email
                             (get-value-from-params "email" params)
                             :password
                             (get-value-from-params "password" params)))
  (if (null login-user)
      (render #P"sessions/new.html")
      (progn
        (log-in login-user)
        (redirect (format nil  "/users/~A" (gethash :user-id *session*))))))

(defroute ("/logout" :method :DESTROY) ()
  (format nil "This is logout page"))

(defroute "/api/users" ()
  (setf users (retrieve-dao 'user))
  (render-json users))

(defroute "/api/user/:id" (&key id)
  (setf user (find-dao 'user :id id))
  (render-json user))

(defroute "/counter" ()
  (format nil "You came here ~A times."
          (incf (gethash :counter *session* 0))))

(defroute "/current-user" ()
  (format nil "~A" (user-name (current-user))))

(defroute "/check-logged-in" ()
  (format nil "~A" (logged-in-p)))

(defroute "/test" ()
  (format nil "~A" (user-name (current-user))))
;;
;; Helper functions
(defun current-user ()
  (find-dao 'user :id (gethash :user-id *session* 0)))

(defun reset-current-user ()
  (setf (gethash :user-id *session*) nil))

(defun log-in (user)
  (reset-current-user)
  (setf (gethash :user-id *session*) (object-id user)))

(defun logged-in-p ()
  (not (null (current-user))))


;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
