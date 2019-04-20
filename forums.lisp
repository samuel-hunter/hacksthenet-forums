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
            :accessor dirty-content)
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
            :accessor dirty-content)
   (time :type integer
         :initform (get-universal-time)
         :reader post-time)
   (name :type string
          :initarg :name
          :accessor dirty-name)
   (id :type integer
       :initarg :id
       :reader id)))

(defclass forum ()
  ((name :type string
         :initarg :name
         :accessor dirty-name)
   (description :type string
                :initarg :description
                :accessor dirty-description)
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


(defun author (obj)
  (find-account (author-name obj)))

(defun content (obj)
  (sanitize (dirty-content obj)))

(defun name (obj)
  (sanitize (dirty-name obj)))

(defun description (obj)
  (sanitize (dirty-description obj)))

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

(defun make-thread (forum author-name name content)
  (let ((thread (make-instance 'thread
                               :parent forum
                               :author author-name
                               :content content
                               :name name
                               :id (incf (thread-counter forum)))))
    (push thread (threads forum))
    (id thread)))

(defun make-thread* (forum name content)
  (make-thread forum
               (username (session-account))
               name content))

(defun find-thread (forum-name id)
  "On success returns two values: the thread, and its forum. On
failure returns NIL."
  (let ((forum (find-forum forum-name)))
    (when forum
      (values (find-if (lambda (thread)
                         (= id (id thread)))
                       (threads forum))
              forum))))

(defun make-post (thread author content)
  (let ((post (make-instance 'post
                             :parent thread
                             :author (username author)
                             :content content
                             :id (incf (post-counter (parent-forum thread))))))
    (push post (posts thread))
    (id post)))

(defun make-post* (thread content)
  "On success returns the post's ID. On failure returns NIL."
  (make-post thread (session-account) content))

(defmethod posts ((forum forum))
  (loop for thread in (threads forum)
     append (posts thread)))

(defun find-post (forum id)
  (find-if (lambda (post)
             (= id (id post)))
           (posts forum)))
