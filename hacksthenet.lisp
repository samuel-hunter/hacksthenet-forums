(ql:quickload '(:hunchentoot :cl-who :cl-ppcre))

(defpackage hacksthenet
  (:use :cl :hunchentoot :cl-who :cl-ppcre)
  (:export :run))

(in-package #:hacksthenet)

(defvar *acceptor* nil)
(defvar *port* 8080)
(setf (html-mode) :html5)

(load "auth.lisp")
(load "forums.lisp")
(load "pages.lisp")

(defun create-simple-dispatcher (uri handler)
  "Make a simple dispatcher that will dispatch to the provided
function provided the URI matches."
  (lambda (request)
    (and (string= uri (script-name request))
         handler)))

(defun run ()
  (when (and *acceptor*
               (started-p *acceptor*))
    (stop *acceptor*))

  (setf *dispatch-table*
        (list
         (create-simple-dispatcher "/" 'home-page)
         (create-simple-dispatcher "/users" 'users-page)
         (create-simple-dispatcher "/login" 'login-page)
         (create-simple-dispatcher "/logout" 'logout-page)

         (create-forum-dispatcher)))

  (setf *acceptor* (make-instance 'easy-acceptor
                                  :port *port*
                                  :document-root (merge-pathnames #P"static/"
                                                                  *default-pathname-defaults*)))
  (start *acceptor*))
