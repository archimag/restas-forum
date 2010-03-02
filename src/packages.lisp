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
           
           #:list-forums))

(in-package #:restas.forum)

(defparameter *restas-forum-pathname* (asdf:component-pathname (asdf:find-system '#:restas-forum)))

(closure-template:compile-template :common-lisp-backend
                                   (merge-pathnames "src/forum.tmpl"
                                                    *restas-forum-pathname*))