(in-package #:hacksthenet)

(defclass account ()
  ((username :type string
             :initarg :username
             :accessor username)
   (password :type string
             :initarg :password
             :accessor password)))

(defvar *accounts* ())

(defun find-account (username)
  (find-if (lambda (acc)
             (string= username
                      (username acc)))
           *accounts*))

(defun add-account (username password)
  (unless (find-account username)
    (push (make-instance 'account
                         :username username
                         :password password)
          *accounts*)))

(defun login (username password)
  "Log in if the username and password matches an account.
Return the account if successful; NIL otherwise."
  (let ((account (find-account username)))
    (and account
         (string= password (password account))
         (setf (session-value 'account) account))))

(defun change-pass (username password)
  (let ((account (find-account username)))
    (when account
      (setf (password account) password))))

(defun session-account ()
  (session-value 'account))
