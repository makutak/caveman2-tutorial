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
  (:export :*web*))
(in-package :caveman2-tutorial.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

;;
;; Routing rules

(defroute "/" ()
  (render #P"index.html"
          (list :users (find-users))))

(defroute "/home" ()
  (render #P "static_pages/home.html"))

(defroute "/help" ()
  (render #P "static_pages/help.html"))

(defroute "/about" ()
  (render #P "static_pages/about.html"))

(defroute "/users/new" ()
  (render #P "users/new.html"))

(defroute "/params" (&key _parsed)
  (format nil "~A" _parsed))

(defroute ("/users/create" :method :POST) (&key _parsed)
  (setf params (cdr (assoc "user" _parsed :test #'string=)))
  (if (valid-user params)
      (redirect (format nil "/users/~A" (object-id (create-user params))))
      (redirect "/users/new")))

(defroute "/users/:id" (&key id)
  (setf u (find-user id))
  (if (null u)
      (render #P"_errors/404.html")
      (render #P"users/show.html" (user-info u))))

(defroute "/api/users" ()
  (setf users (find-users))
  (render-json users))

(defroute "/api/user/:id" (&key id)
  (setf user (find-user id))
  (render-json user))



;;
;; Error pages

(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (merge-pathnames #P"_errors/404.html"
                   *template-directory*))
