(in-package #:hacksthenet)

(defclass post ()
  ((parent-thread :type thread
                  :initarg :parent
                  :reader parent-thread)
   (author-name :type string
           :initarg :author
           :reader author-name)
   (content :type string
            :initarg :content
            :accessor content)
   (time :type integer
         :initform (get-universal-time)
         :reader post-time)
   (id :type integer
       :initarg :id
       :reader id)))

(defclass thread ()
  ((parent-forum :type forum
                 :initarg :parent
                 :reader parent-forum)
   (posts :type (or cons null)
          :initform ()
          :accessor posts)
   (author-name :type string
                :initarg :author
                :reader author-name)
   (content :type string
            :initarg :content
            :accessor content)
   (time :type integer
         :initform (get-universal-time)
         :reader post-time)
   (title :type string
          :initarg :title
          :accessor title)
   (id :type integer
       :initarg :id
       :reader id)))

(defclass forum ()
  ((name :type string
         :initarg :name
         :accessor name)
   (description :type string
                :initarg :description
                :accessor description)
   (threads :type (or cons null)
            :initform ()
            :accessor threads)
   (post-counter :type integer
                 :initform 0
                 :accessor post-counter)
   (thread-counter :type integer
                   :initform 0
                   :accessor thread-counter)))

(defvar *forums* ())

(defun find-forum (name)
  (find-if (lambda (forum)
             (string= name
                      (name forum)))
           *forums*))

(defun make-forum (name description)
  (unless (find-forum name)
    (push (make-instance 'forum
                         :name name
                         :description description)
          *forums*)
    name))

(defun delete-forum (name)
  (setf *forums* (delete-if
                  (lambda (forum)
                    (string= name
                             (name forum)))
                  *forums*)))

(defun make-thread (forum author-name title content)
  (let ((thread (make-instance 'thread
                               :parent forum
                               :author author-name
                               :content content
                               :title title
                               :id (incf (thread-counter forum)))))
    (push thread (threads forum))
    (id thread)))

(defun make-thread* (forum-name title content)
  (make-thread (find-forum forum-name)
               (username (session-account))
               title content))

(defun find-thread (forum-name id)
  (find-if (lambda (thread)
             (= id (id thread)))
           (threads (find-forum forum-name))))

(defun make-post (thread author-name content)
  (let ((post (make-instance 'post
                             :parent thread
                             :author author-name
                             :content content
                             :id (incf (post-counter (parent-forum thread))))))
    (push post (posts thread))
    (id post)))

(defun make-post* (forum-name thread-id content)
  (make-post (find-thread forum-name thread-id)
             (username (session-account))
             content))

(defmethod posts ((forum forum))
  (loop for thread in (threads forum)
     append (posts thread)))

(defun find-post (forum id)
  (find-if (lambda (post)
             (= id (id post)))
           (posts forum)))
