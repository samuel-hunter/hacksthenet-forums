(in-package #:hacksthenet)

(defmethod link (obj))

(defmethod link ((forum forum))
  (format nil "/forum/~a/" (name forum)))

(defmethod link ((thread thread))
  (format nil "/forum/~a/~d" (name (parent-forum thread)) (id thread)))

(defmethod link ((account account))
  (format nil "/users/~a" (username account)))

(defun format-time (time)
  "Return the time as a formatted string"
  (multiple-value-bind (second minute hour date month year) (decode-universal-time time)
    (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d"
            year month date hour minute second)))

(defun count-words (string)
  "Count the words in a given string"
  (length (cl-ppcre:split "\\s+" string)))

(defun login-link ()
  "Return the link to the login page that will redirect to the
current page."
  (concatenate 'string "/login?redirect=" (script-name*)))

(defmacro standard-page ((&key title small
                               (breadcrumbs ''(("/" . "Home")))) &body body)
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
               (:nav :id "header-nav"
                     (:a :href "/" "Home")
                     (:a :href "/users" "Users")
                     (if (session-account)
                         (htm (:a :href "/logout" "Log out")
                              (:small :id "user" (princ (concatenate 'string
                                                                     "Welcome, "
                                                                     (username (session-account))))))
                         (htm (:a :href (login-link) "Log in")
                              (:a :href "/register" "Register"))))
               (:h1 "Hacksthenet Forum"
                    ,(if small
                         `(:small ,small))))
              (:main :id "main"
                     (:nav :class "breadcrumbs"
                           (loop for ((link . name) . rest) on ,breadcrumbs
                              do (htm (:a :href (princ link) (princ name))
                                      (when rest
                                        (htm (:span " > "))))))
                     ,@body)
              (:footer
               (:p "See this project on " (:a :href "https://github.com/samuel-hunter/hacksthenet-forums"
                                           "GitHub!"))))))))

(defun 404-page ()
  (setf (return-code*) 404)
  (standard-page (:title "404")
    (:h2 "404 - File Not Found.")
    (:p "The reqeusted URL " (princ (script-name*)) " was not found.")))

(defun home-page ()
  (standard-page ()
    (:ul :class "forums"
         (loop for forum across *forums*
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
                     `(:strong :class "error" "Incorrect username or password."))
                  (:form :action "/login" :method "post"
                         (:label :for "username" "Username")
                         (:input :type "text" :name "username" :id "username"
                                 :required t :placeholder "Username")
                         (:label :for "password" "Password")
                         (:input :type "password" :name "password" :id "password"
                                 :required t :placeholder "Password")
                         (:input :type "hidden" :name "redirect" :value (or (get-parameter "redirect")
                                                                            "/"))
                         (:input :type "submit")))))
    ;; Redirect early if already logged in
    (when (session-value 'account)
      (return-from login-page (redirect "/")))

    (if (eq (request-method*) :get)
        (login-html :type :standard)
        (if (login (post-parameter "username")
                   (post-parameter "password"))
            (redirect (or (post-parameter "redirect")
                          "/"))
            (login-html :type :fail)))))

(defun register-page ()
  (macrolet ((register-html (&optional error)
               `(standard-page (:title "Register")
                  (:h2 "Register")
                  ,(when error
                     `(:strong :class "error" ,error))
                  (:form :action "register" :method "post"
                         (:label :for "username" "Username")
                         (:input :type "text" :id "username" :name "username"
                                 :required t :placeholder "Username")
                         (:label :for "password" "Password")
                         (:input :type "password" :id "password" :name "password"
                                 :required t :placeholder "Password")
                         (:label :for "confirm" "Confirm Password")
                         (:input :type "password" :id "confirm" :name "confirm"
                                 :required t :placeholder "Password")
                         (:input :type "submit")))))
    ;; Redirect early if already logged in
    (when (session-value 'account)
      (return-from register-page (redirect "/")))

    (if (eq (request-method*) :get)
        (register-html)
        (let ((username (post-parameter "username"))
              (password (post-parameter "password"))
              (confirm (post-parameter "confirm")))
          (unless (and username password confirm)
            (return-from register-page
              (register-html "Make sure to fill in all required forms.")))

          (unless (string= password confirm)
            (return-from register-page
              (register-html "Make sure that your password and confirmation matches.")))

          (add-account* username password)
          (redirect "/")))
    ))

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

(defmacro standard-thread-page (thread forum &optional error)
  `(let ((author (author ,thread)))
     (standard-page (:title (name ,thread)
                            :breadcrumbs
                            `(("/" . "Home")
                              (,(link ,forum) . ,(name ,forum))
                              (,(link ,thread) . ,(name ,thread))))
       ,(when error
          `(:strong :class "error" ,error))
       (:div :class "post op"
             (:div :class "post-author"
                   (:p :class "author-name"
                       (if author
                           (htm (:a :href (link author)
                                    (princ (username author))))
                           (princ (author-name ,thread)))))
             (:div :class "post-content"
                   (:header
                    (:small :class "timestamp" (princ (format-time (post-time ,thread))))
                    (:h2 (princ (name ,thread))))
                   (:hr)
                   (:p (princ (content ,thread)))))
       (loop for post across (posts ,thread)
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
                                 (:small :class "timestamp" (princ (format-time (post-time post)))))
                                (:p (princ (content post))))))))
       (when (session-account)
         (htm (:form :class "reply"
                     :action (concatenate 'string (link ,thread) "/reply") :method "post"
                     (:label :for "content" "Reply")
                     (:textarea :required t :name "content" :id "content")
                     (:input :type "submit")))))))

(defun thread-page ()
  (multiple-value-bind (thread forum) (page-thread)
    (unless thread
      (return-from thread-page (404-page)))
    (standard-thread-page thread forum)))

(defun thread-reply-page ()
  (unless (session-account)
    (return-from thread-reply-page (redirect (login-link))))

  (multiple-value-bind (thread forum) (page-thread)
    (unless thread
      (return-from thread-reply-page (404-page)))
    (let ((content (string-trim " " (post-parameter "content"))))
      (if (>= (count-words content) 5)
          (progn
            (make-post* thread content)
            (redirect (link thread)))
          (standard-thread-page thread forum "Your reply must have at least 5 words.")))))

(defun new-thread-page ()
  (let ((forum (page-forum)))
    (unless forum
      (return-from new-thread-page (404-page)))

    (unless (session-account)
      (return-from new-thread-page (redirect (login-link))))

    (macrolet ((render-page (&optional error)
               `(standard-page (:title (name forum) :small (name forum)
                                       :breadcrumbs
                                       `(("/" . "Home")
                                         (,(link forum) . ,(name forum))))

                  ,(when error
                     `(:strong :class "error" ,error))
                  (:h2 "New Thread for " (princ (name forum)))
                  (:form :action (concatenate 'string (link forum) "new") :method "post"
                         (:label :for "name" "Name")
                         (:input :type "text" :id "name" :name "name" :required t)
                         (:label :for "content" "Body")
                         (:textarea :id "content" :name "content" :required t)
                         (:input :type "submit")))))

      (if (eq (request-method*) :post)
          (let ((content (string-trim " " (post-parameter "content")))
                (name (string-trim " "(post-parameter "name"))))
            (if (and (>= (length content) 5)
                     (>= (length name) 1))
                (redirect (format nil "~a~d" (link forum) (make-thread* forum name content)))
                (render-page "Either the name was empty or the content wasn't at least 5 words long.")))
          (render-page)))))
