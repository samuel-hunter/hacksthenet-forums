(ql:quickload '(:hunchentoot :cl-who))

(defpackage hacksthenet
  (:use :cl :hunchentoot :cl-who)
  (:export :run))

(in-package #:hacksthenet)

(defparameter *acceptor* nil)
(defparameter *port* 8080)
(setf (html-mode) :html5)

(load "auth.lisp")

(defun make-simple-dispatcher (uri handler)
  "Make a simple dispatcher that will dispatch to the provided
function provided the URI matches."
  (lambda (request)
    (and (string= uri (script-name request))
         handler)))

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output*
                                nil :prologue t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             ,(if title
                  `(:title (princ (concatenate 'string ,title " | Hacksthenet Forum")))
                  `(:title "Hacksthenet Forum"))
             (:body
              (:header
               (:h1 "Hacksthenet Forum")
               (:nav :class "link-nav"
                     (:ul
                      (:li (:a :href "/" "Home"))
                      (:li (:a :href "/members" "Members"))
                      (if (session-account)
                          (htm (:li (:a :href "/logout" "Logout")))
                          (htm (:li (:a :href "/login" "Login")))))))
              ,@body)))))

(defun home-page ()
  (standard-page ()))

(defun members-page ()
  (standard-page (:title "Members")
    (:h2 "Members")))

(defun login-page ()
  (macrolet ((login-html (&key type)
               `(standard-page (:title "Login")
                  (:h2 "Login")
                  ,(when (eq type :fail)
                     `(:strong "Bad username or password."))
                  (:form :action "/login" :method "post"
                         (:input :type "text" :name "user" :placeholder "Username")
                         (:input :type "password" :name "pass" :placeholder "Password")
                         (:input :type "submit")))))
    ;; Redirect early if already logged in
    (when (session-value 'account)
      (return-from login-page (redirect "/")))

    (if (eq (request-method*) :get)
        (login-html :type :standard)
        (if (login (post-parameter "user")
                   (post-parameter "pass"))
            (redirect "/")
            (login-html :type :fail)))))

(defun logout-page ()
  (setf (session-value 'account) nil)
  (redirect "/"))

(defun run ()
  (when (and *acceptor*
               (started-p *acceptor*))
    (stop *acceptor*))

  (setf *dispatch-table*
        (list
         (make-simple-dispatcher "/" 'home-page)
         (make-simple-dispatcher "/members" 'members-page)
         (make-simple-dispatcher "/login" 'login-page)
         (make-simple-dispatcher "/logout" 'logout-page)))

  (setf *acceptor* (make-instance 'easy-acceptor :port *port*))
  (start *acceptor*))
