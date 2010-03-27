;;;; forum.lisp
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.forum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; aux
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun parse-start ()
  (or (ignore-errors (parse-integer (hunchentoot:get-parameter "start")))
      0))

(defun forum-info-plist (info)
  (list :title (second info)
        :href (restas:genurl 'list-topics
                             :forum-id (first info))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; list all forums
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route list-forums ("")
  (list :forums (iter (for forum in (storage-list-forums *storage*))
                      (collect (forum-info-plist forum)))
        :title "Все форумы"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; view forum topics
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route list-topics (":forum-id")
  (let ((start (parse-start)))
    (list* :topics (iter (for topic in (storage-list-topics *storage* forum-id *max-topic-on-page* start))
                         (collect (list* :href (restas:genurl 'topic-message-replies
                                                              :topic-id (getf topic :id))
                                         topic)))
           :first start
           (storage-forum-info *storage* forum-id))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; view topic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route topic-message-replies ("thread/:topic-id"
                                     :parse-vars (list :topic-id #'parse-integer))
  (let ((message (storage-topic-message *storage* topic-id))
        (start (parse-start)))
    (list :list-forums-href (genurl 'list-forums)
          :parent-forum (forum-info-plist (getf message :forum))
          :message message
          :replies (storage-topic-replies *storage*
                                          topic-id
                                          *max-reply-on-page*
                                          start)
          :title (getf message :title))))
                                          
                                          
  