(in-package #:hacksthenet)

(defmethod link (obj))

(defmethod link ((forum forum))
  (format nil "/forum/~a/" (name forum)))

(defmethod link ((thread thread))
  (format nil "/forum/~a/~d" (name (parent-forum thread)) (id thread)))

(defmethod link ((account account))
  (format nil "/users/~a" (username account)))

(defmethod format-time (time)
  (multiple-value-bind (second minute hour date month year) (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(defmacro standard-page ((&key title (breadcrumbs ''(("/" . "Home")))) &body body)
  `(with-html-output-to-string (*standard-output*
                                nil :prologue t)
     (:html :lang "en"
            (:head
             (:meta :charset "utf-8")
             ,(if title
                  `(:title (format t "~a | Hacksthenet Forum" ,title))
                  `(:title "Hacksthenet Forum"))
             (:link :rel "stylesheet" :href "/app.css")
             (:body
              (:header
               (:h1 "Hacksthenet Forum")
               (:nav :class "link-nav"
                     (:ul
                      (:li (:a :href "/" "Home"))
                      (:li (:a :href "/users" "Users"))
                      (if (session-account)
                          (htm (:li (:a :href "/logout" "Logout")))
                          (htm (:li (:a :href "/login" "Login"))))))
               (:nav :class "breadcrumbs"
                     (loop for ((link . name) . rest) on ,breadcrumbs
                        do (htm (:a :href (princ link) (princ name))
                                (when rest
                                  (htm (:span " > ")))))))
              ,@body)))))

(defun home-page ()
  (standard-page ()
    (:ul :class "forums"
         (loop for forum in *forums*
            do (htm (:li (:p (:a :href (link forum) (:strong (princ (name forum)))))
                          (:p (:em (princ (description forum))))))))))

(defun users-page ()
  (standard-page (:title "Users")
    (:h2 "Users")
    (:ul :class "users"
         (loop for account in *accounts*
            do (htm (:li (:a :href (link account)
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
  (multiple-value-bind (status results)
      (scan-to-strings "^/forum/(\\w+)/" (script-name request))
    (when status
      (find-forum (aref results 0)))))

(defun page-thread (&optional (request *request*))
  "On success return two values: the thread, and its forum. On
failure returns NIL."
  (multiple-value-bind (status results)
      (scan-to-strings "^/forum/(\\w+)/(\\d+)" (script-name request))
    (when status
      (find-thread (aref results 0) (parse-integer (aref results 1))))))

(defun forum-page ()
  (let ((forum (page-forum)))
    (standard-page (:title (name forum)
                           :breadcrumbs `(("/" . "Home")
                                          (,(link forum) . ,(name forum))))
      (:h2 (princ (name forum)))
      (:small (princ (description forum)))
      (:ul :class "threads"
           (loop for thread in (threads forum)
              do (htm (:li (:p (:a :href (link thread)
                                   (princ (title thread)))))))))))

(defun thread-page ()
  (multiple-value-bind (thread forum) (page-thread)
    (let ((author (author thread)))
      (standard-page (:title (title thread)
                             :breadcrumbs
                             `(("/" . "Home")
                               (,(link forum) . ,(name forum))
                               (,(link thread) . ,(title thread))))
        (:div :class "post op"
              (:div :class "post-author"
                    (:p :class "author-name"
                        (if author
                            (htm (:a :href (link author)
                                     (princ (username author))))
                            (princ (author-name thread)))))
              (:div :class "post-content"
                    (:header
                     (:small :class "timestamp" (princ (format-time (post-time thread))))
                     (:h2 (princ (title thread))))
                    (:hr)
                    (:p (princ (content thread)))))
        (loop for post in (posts thread)
           do (let ((post-author (author post)))
               (htm (:div :class "post"
                          (:div :class "post-author"
                                (:p :class "author-name"
                                    (if post-author
                                        (htm (:a :href (link post-author)
                                                 (princ (username post-author))))
                                        (princ (author-name post)))))
                          (:div :class "post-content"
                                (:header
                                 (:small :class "timestamp" (princ (format-time (post-time thread)))))
                                (:p (princ (content post))))))))))))

(defun create-forum-dispatcher ()
  (lambda (request)
    (or (and (page-thread request)
             'thread-page)
        (and (page-forum request)
             'forum-page))))
