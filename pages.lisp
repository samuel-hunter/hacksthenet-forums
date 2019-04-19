(in-package #:hacksthenet)

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output*
                                nil :prologue t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             ,(if title
                  `(:title (format t "~a | Hacksthenet Forum" ,title))
                  `(:title "Hacksthenet Forum"))
             (:body
              (:header
               (:h1 "Hacksthenet Forum")
               (:nav :class "link-nav"
                     (:ul
                      (:li (:a :href "/" "Home"))
                      (:li (:a :href "/users" "Users"))
                      (if (session-account)
                          (htm (:li (:a :href "/logout" "Logout")))
                          (htm (:li (:a :href "/login" "Login")))))))
              ,@body)))))

(defun home-page ()
  (standard-page ()
    (:ul :class "forums"
         (loop for forum in *forums*
            do (htm (:li (:p (:a :href (format nil "/forum/~a" (name forum)) (:strong (princ (name forum)))))
                          (:p (:em (princ (description forum))))))))))

(defun users-page ()
  (standard-page (:title "Users")
    (:h2 "Users")
    (:ul :class "users"
         (loop for account in *accounts*
            do (htm (:li (:a :href (format nil "/users/~a" (username account))
                             (princ (username account)))))))))

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

(defun page-forum (&optional (request *request*))
  (multiple-value-bind (_ results)
      (scan-to-strings "^/forum/(\\w+)" (script-name request))
    (declare (ignore _))
    (find-forum (aref results 0))))

(defun forum-page ()
  (let ((forum (page-forum)))
    (standard-page (:title (name forum))
      (:h2 (name forum)))))

(defun create-forum-dispatcher ()
  (lambda (request)
    (and (page-forum request)
         'forum-page)))
