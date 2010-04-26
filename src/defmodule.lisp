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
           
           #:storage-list-forums
           #:storage-list-topics
           #:storage-forum-info
           #:storage-topic-message
           #:storage-topic-replies))

(in-package #:restas.forum)

(defparameter *storage* nil)

(defparameter *max-topic-on-page* 4)

(defparameter *max-reply-on-page* 50)

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
