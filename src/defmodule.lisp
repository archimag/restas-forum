;;;; packages.lisp
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(restas:define-module #:restas.forum
  (:use #:cl #:iter)
  (:export #:*storage*
           #:*finalize-page*
           #:*max-topic-on-page*
           #:*max-reply-on-page*
           #:*user-name-function*
           #:*rss-item-count*

           ;; storage interface
           #:storage-admin-p
           #:storage-list-forums
           #:storage-list-topics
           #:storage-create-topic
           #:storage-delete-topic
           #:storage-forum-info
           #:storage-topic-message
           #:storage-topic-reply-count
           #:storage-topic-replies
           #:storage-create-reply
           #:storage-delete-reply
           #:storage-reply-position
           #:storage-all-news
           #:storage-forum-news
           #:storage-topic-news))

(in-package #:restas.forum)

(defparameter *storage* nil)

(defparameter *rss-item-count* 20)

(defparameter *max-topic-on-page* 10)

(defparameter *max-reply-on-page* 6)

(defparameter *user-name-function* nil)

(defparameter *restas-forum-pathname* 
  (asdf:component-pathname (asdf:find-system '#:restas-forum)))

(restas:define-submodule resources (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* (merge-pathnames "resources/"
                                                           *restas-forum-pathname*))
  (restas.directory-publisher:*autoindex* nil))

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/forum.tmpl"
                                                    *restas-forum-pathname*))

(setf *default-render-method*
      (lambda (obj)
        (closure-template.standard:xhtml-strict-frame
         (list :title (getf obj :title)
               :js (getf obj :js)
               :body (restas:render-object (find-package '#:restas.forum.view)
                                           obj)))))
