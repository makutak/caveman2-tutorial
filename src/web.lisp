(in-package :cl-user)
(defpackage caveman2-tutorial.web
  (:use :cl
        :caveman2
        :caveman2-tutorial.config
        :caveman2-tutorial.view
        :caveman2-tutorial.db
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

(defroute "/" ()
  (redirect "/home"))

(defroute "/home" ()
  (render #P "static_pages/home.html"))

(defroute "/help" ()
  (render #P "static_pages/help.html"))

(defroute "/about" ()
  (render #P "static_pages/about.html"))

(defroute "/signup" ()
  (redirect "/users/new"))

(defroute "/users/new" ()
  (render #P "users/new.html"))

(defroute ("/users/create" :method :POST) (&key _parsed)
  (setf params (cdr (assoc "user" _parsed :test #'string=)))
  (if (valid-user params)
      (redirect (format nil "/users/~A" (object-id (create-user params))))
      (redirect "/users/new")))

(defroute "/users/:id" (&key id)
  (setf u (find-dao 'user :id id))
  (if (null u)
      (render #P"_errors/404.html")
      (render #P"users/show.html" (user-info u))))

(defroute "/login" ()
  (render #P"sessions/new.html"))

(defroute ("/login" :method :POST) (&key _parsed)
  (setf params (cdr (assoc "session" _parsed :test #'string=)))
  (setf login-user (find-dao 'user
                             :email
                             (get-value-from-params "email" params)
                             :password
                             (get-value-from-params "password" params)))
  (if (null login-user)
      (render #P"sessions/new.html")
      (progn
        (setf (gethash :user-id *session*) (object-id login-user))
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
  (current-user)
  (format nil "~A" (user-name (current-user))))


;;
;; Helper functions
(defparameter *current-user* nil)

(defun current-user ()
  (or  *current-user*
       (setf *current-user*
             (find-dao 'user :id (gethash :user-id *session*)))))


;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
