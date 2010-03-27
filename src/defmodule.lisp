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
           
           #:storage-list-forums
           #:storage-list-topics
           #:storage-forum-info
           #:storage-topic-message
           #:storage-topic-replies))

(in-package #:restas.forum)

(defparameter *storage* nil)

(defparameter *max-topic-on-page* 10)

(defparameter *max-reply-on-page* 50)

(defparameter *restas-forum-pathname* 
  (asdf:component-pathname (asdf:find-system '#:restas-forum)))

(restas:define-submodule resources (#:restas.directory-publisher)
  (restas.directory-publisher:*baseurl* '("css"))
  (restas.directory-publisher:*directory* (merge-pathnames "resources/css/"
                                                           *restas-forum-pathname*))
  (restas.directory-publisher:*autoindex* nil))

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/forum.tmpl"
                                                    *restas-forum-pathname*))

(setf *default-render-method*
      (lambda (obj)
        (closure-template.standard:xhtml-strict-frame
         (list :title (getf obj :title)
               :body (restas:render-object (find-package '#:restas.forum.view)
                                           obj)
               :css (loop for item in '("style.css")
                       collect (restas:genurl 'css :file item))))))
