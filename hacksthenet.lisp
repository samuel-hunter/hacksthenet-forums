(ql:quickload '(:hunchentoot :cl-who :cl-ppcre))

(defpackage hacksthenet
  (:use :cl :hunchentoot :cl-who :cl-ppcre)
  (:export :run :start-demo))

(in-package #:hacksthenet)

(defvar *acceptor* nil)
(defvar *port* 8080)
(setf (html-mode) :html5)

(load "sanitize.lisp")
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
         (create-folder-dispatcher-and-handler "/assets/" #P "assets/")
         (create-simple-dispatcher "/" 'home-page)
         (create-simple-dispatcher "/users" 'users-page)
         (create-simple-dispatcher "/login" 'login-page)
         (create-simple-dispatcher "/register" 'register-page)
         (create-simple-dispatcher "/logout" 'logout-page)

         (create-regex-dispatcher "^/users/(\\w+)" 'wip-page)
         (create-regex-dispatcher "^/forum/(\\w+)/$" 'forum-page)
         (create-regex-dispatcher "^/forum/(\\w+)/new$" 'new-thread-page)
         (create-regex-dispatcher "^/forum/(\\w+)/(\\d+)$" 'thread-page)
         (create-regex-dispatcher "^/forum/(\\w+)/(\\d+)/reply$" 'thread-reply-page)
         (lambda (request) (declare (ignore request)) '404-page)))

  (setf *acceptor* (make-instance 'easy-acceptor
                                  :port *port*
                                  :document-root nil))
  (start *acceptor*))

(defun start-demo ()
  "Start the server with a few forums pre-setup for hacksthenet."
  (make-forum "general" "Just general things")
  (make-forum "tech" "Discuss technical topics here")
  (make-forum "web-dev" "Topics on Web Development")

  (add-account "admin" "emmetcortez07051")
  (add-account "system" "alfonsogood67276")
  (add-account "moderator" "nitanielsen67276")
  (run))
