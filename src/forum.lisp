;;;; forum.lisp
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.forum)

(defparameter *storage* (make-instance 'memory-storage))

(defvar *finalize-page* #'restas.forum.view:default-standalone-frame)

(defun finalize-page (content title)
  (funcall *finalize-page* 
           (list :title title
                 :content content
                 :css (loop for item in '("style.css")
                         collect (restas:genurl 'css :file item)))))

;;;; css

(restas:define-route css ("css/:file")
  (merge-pathnames (format nil "resources/css/~A" file)
                   *restas-forum-pathname*))

;;;; list all forums

(restas:define-route main ("")
  (finalize-page
   (restas.forum.view:show-all-forums (list :forums
                                            (iter (for forum in (all-forums *storage*))
                                                  (collect (list :title (second forum)
                                                                 :href (restas:genurl 'forum :forum-id (first forum)))))))
   "Все форумы"))

;;;; forum
  