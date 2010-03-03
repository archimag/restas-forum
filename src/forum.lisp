;;;; forum.lisp
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.forum)

;;;; css

(define-route css ("css/:file")
  (merge-pathnames (format nil "resources/css/~A" file)
                   *restas-forum-pathname*))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; list all forums
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route main ("")
  (finalize-page
   (restas.forum.view:show-all-forums 
    (list :forums
          (iter (for forum in (list-forums))
                (collect (list :title (second forum)
                               :href (restas:genurl 'forum :forum-id (first forum)))))))
   "Все форумы"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; view forum topics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define-route forum (":forum-id")
  (finalize-page
   (restas.forum.view:show-list-topics (list* :topics (list-topics forum-id 10 0)
                                              :first 0
                                              :total-count 10
                                              (forum-info forum-id)))
   "Форум"))


  