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

(defun user-name ()
  (if *user-name-function*
      (funcall *user-name-function*)))

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
  (bind:bind ((start (parse-start))
              ((title total-count) (storage-forum-info *storage* forum-id))
              (href (restas:genurl 'list-topics :forum-id forum-id))
              (adminp (storage-admin-p *storage* (user-name))))
    (flet ((self-url (start)
             (format nil "~A?start=~A" href start)))
      (list :title title
            :js (iter (for item in '("jquery.js" "jquery.wysiwyg.js" "forum.js"))
                      (collect (format nil "../js/~A" item)))
            :css '("jquery.wysiwyg.css")
            :total-count total-count
            :list-forums-href (genurl 'list-forums)
            :href-before (if (< (+ (1- start) *max-topic-on-page*)
                                total-count)
                             (self-url (+ start *max-topic-on-page*)))
            :href-after (if (> start 0)
                            (self-url (max (- start *max-topic-on-page*) 0)))
            :topics (iter (for topic in (storage-list-topics *storage* forum-id *max-topic-on-page* start))
                          (collect (list* :href (restas:genurl 'topic-message-replies
                                                               :topic-id (getf topic :id))
                                          :href-delete (if adminp
                                                           (restas:genurl 'delete-topic
                                                                          :topic-id (getf topic :id)))
                                          topic)))
            :first (1+ start)
            :can-create-new-topic (user-name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; create new topic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route create-topic (":forum-id"
                            :method :post
                            :requirement 'user-name)
  (let ((title (hunchentoot:post-parameter "title"))
        (body (hunchentoot:post-parameter "body")))
    (unless (or (string= title "")
                (string= body ""))
      (storage-create-topic *storage*
                            forum-id
                            title
                            body
                            (user-name)))
    (restas:redirect 'list-topics :forum-id forum-id)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; delete topic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route delete-topic ("thread/delete/:topic-id"
                            :requirement 'user-name
                            :parse-vars (list :topic-id #'parse-integer))
  (if (storage-admin-p *storage* (user-name))
      (restas:redirect 'list-topics
                       :forum-id (storage-delete-topic *storage* topic-id))
      hunchentoot:+http-forbidden+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; view topic
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route topic-message-replies ("thread/:topic-id"
                                     :parse-vars (list :topic-id #'parse-integer))
  (let* ((message (storage-topic-message *storage* topic-id))
         (start (parse-start))
         (user (user-name))
         (adminp (if user (storage-admin-p *storage* user))))
    (list :list-forums-href (genurl 'list-forums)
          :js (iter (for item in '("jquery.js" "jquery.wysiwyg.js" "forum.js"))
                    (collect (format nil "../../js/~A" item)))
          :parent-forum (forum-info-plist (getf message :forum))
          :message message
          :replies (if adminp
                       (iter (for item in (storage-topic-replies *storage*
                                                                 topic-id
                                                                 *max-reply-on-page*
                                                                 start))
                             (collect (list* :href-delete (restas:genurl 'delete-message
                                                                         :reply-id (getf item :id))
                                             item)))
                       (storage-topic-replies *storage*
                                              topic-id
                                              *max-reply-on-page*
                                              start))
          :can-create-message user
          :title (getf message :title))))

(define-route create-reply ("thread/:topic-id"
                            :method :post
                            :parse-vars (list :topic-id #'parse-integer)
                            :requirement 'user-name)
  (let ((body (hunchentoot:post-parameter "body")))
    (unless (string= body "")
      (storage-create-reply *storage* topic-id body (user-name)))
    (restas:redirect 'topic-message-replies
                     :topic-id topic-id)))
  


(define-route delete-message ("message/delete/:(reply-id)"
                              :requirement 'user-name)
  (if (storage-admin-p *storage* (user-name))
      (restas:redirect 'topic-message-replies
                       :topic-id (storage-delete-reply *storage* reply-id))
      hunchentoot:+http-forbidden+))