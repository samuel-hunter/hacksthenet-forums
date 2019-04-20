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
             (:link :rel "stylesheet" :href "/assets/app.css")
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

(defun 404-page ()
  (setf (return-code*) 404)
  (standard-page (:title "404")
    (:h2 "404 - File Not Found.")
    (:p "The reqeusted URL " (princ (script-name*)) " was not found.")))

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
    (unless forum
      (return-from forum-page (404-page)))

    (standard-page (:title (name forum)
                           :breadcrumbs `(("/" . "Home")
                                          (,(link forum) . ,(name forum))))
      (:h2 (princ (name forum)))
      (:small (princ (description forum)))
      (when (session-account)
        (htm (:p (:a :href (concatenate 'string (link forum) "new")
                  "New Thread"))))
      (:ul :class "threads"
           (loop for thread in (threads forum)
              do (htm (:li (:p (:a :href (link thread)
                                   (princ (name thread)))))))))))

(defun thread-page ()
  (multiple-value-bind (thread forum) (page-thread)
    (unless thread
      (return-from thread-page (404-page)))

    (let ((author (author thread)))

      (standard-page (:title (name thread)
                            :breadcrumbs
                            `(("/" . "Home")
                              (,(link forum) . ,(name forum))
                              (,(link thread) . ,(name thread))))
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
                    (:h2 (princ (name thread))))
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

(defun new-thread-page ()
  (let ((forum (page-forum)))
    (unless forum
      (return-from new-thread-page (404-page)))

    (unless (session-account)
      (return-from new-thread-page (redirect "/login")))

    (macrolet ((render-page (&optional error)
               `(standard-page (:title (name forum)
                                       :breadcrumbs
                                       `(("/" . "Home")
                                         (,(link forum) . ,(name forum))))

                  ,(when error
                     `(:strong :class "error" ,error))
                  (:h2 "New Thread for " (princ (name forum)))
                  (:form :action (concatenate 'string (link forum) "new") :method "post"
                         (:label :for "name" "Name")
                         (:input :type "text" :id "name" :name "name")
                         (:label :for "content" "Body")
                         (:textarea :id "content" :name "content")
                         (:input :type "submit")))))

      (if (eq (request-method*) :post)
          (let ((content (post-parameter "content"))
                (name (post-parameter "name")))
            (if (and content name)
                (redirect (format nil "~a~d" (link forum) (make-thread* (name forum) name content)))
                (render-page "The name or content was empty.")))
          (render-page)))))
