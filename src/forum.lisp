;;;; forum.lisp
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.forum)

;;; aux

(defun user-name ()
  (if *user-name-function*
      (funcall *user-name-function*)))

(defun parse-start ()
  (or (ignore-errors (parse-integer (hunchentoot:get-parameter "start")))
      0))

(defun forum-info-plist (info)
  (list :title (second info)
        :href (restas:genurl 'list-topics
                             :forum-id (first info))))

(defun topic-last-page-id (topic-id &optional topic-reply-count)
  (ceiling (or topic-reply-count
               (storage-topic-reply-count *storage* topic-id))
           *max-reply-on-page*))

(defun topic-pages (topic-id current &key topic-reply-count )
  (cons (list :number 1
              :href (restas:genurl 'view-topic
                                   :topic-id topic-id)
              :current (= 1 current))
        (iter (for i from 2 to (topic-last-page-id topic-id topic-reply-count))
              (collect (list :number i
                             :href (restas:genurl 'view-topic-page
                                                  :topic-id topic-id
                                                  :page-id i)
                             :current (= i current))))))

(defun site-name ()
  (or *site-name*
      (if (boundp 'hunchentoot:*request*)
          (hunchentoot:host))
      "RESTAS-FORUMS"))

(defun js-urls ()
  (iter (for item in '("jquery.js" "jquery.wysiwyg.js" "jqModal.js" "forum.js"))
        (collect (restas:genurl-submodule 'resources
                                          'restas.directory-publisher:route
                                          :path (list "js" item)))))

(defun colorize-traits ()
  (list :href (restas:genurl 'colorize-code)
        :langs (iter (for (id . title) in (colorize:coloring-types))
                     (collect (list :id (symbol-name id)
                                    :title title)))))

;;;; list all forums

(define-route list-forums ("")
  (list :forums (iter (for forum in (storage-list-forums *storage*))
                      (collect (forum-info-plist forum)))
        :feed-href (restas:genurl 'all-forums-rss)
        :title "Все форумы"))

;;;; view forum topics

(define-route list-topics (":forum-id")
  (bind:bind ((start (parse-start))
              ((title total-count) (storage-forum-info *storage* forum-id))
              (href (restas:genurl 'list-topics :forum-id forum-id))
              (adminp (storage-admin-p *storage* (user-name))))
    (flet ((self-url (start)
             (format nil "~A?start=~A" href start)))
      (list :title title
            :js (js-urls)
            :css '("jquery.wysiwyg.css")
            :href-rss (genurl 'forum-rss :forum-id forum-id)
            :total-count total-count
            :list-forums-href (genurl 'list-forums)
            :href-before (if (< (+ (1- start) *max-topic-on-page*)
                                total-count)
                             (self-url (+ start *max-topic-on-page*)))
            :href-after (if (> start 0)
                            (self-url (max (- start *max-topic-on-page*) 0)))
            :topics (iter (for topic in (storage-list-topics *storage* forum-id *max-topic-on-page* start))
                          (collect (list* :href (restas:genurl 'view-topic
                                                               :topic-id (getf topic :id))
                                          :pages (topic-pages (getf topic :id)
                                                              -1
                                                              :topic-reply-count (getf topic :message-count))
                                          :href-delete (if adminp
                                                           (restas:genurl 'delete-topic
                                                                          :topic-id (getf topic :id)))
                                          topic)))
            :first (1+ start)
            :can-create-new-topic (user-name)))))

;;;; create new topic

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


;;;; delete topic

(define-route delete-topic ("thread/delete/:topic-id"
                            :requirement 'user-name
                            :parse-vars (list :topic-id #'parse-integer))
  (if (storage-admin-p *storage* (user-name))
      (restas:redirect 'list-topics
                       :forum-id (storage-delete-topic *storage* topic-id))
      hunchentoot:+http-forbidden+))

;;;; view-topic-page

  
(define-route view-topic-page ("thread/:topic-id/page:(page-id)"
                               :parse-vars (list :topic-id #'parse-integer
                                                 :page-id #'parse-integer))
  (let* ((message (storage-topic-message *storage* topic-id))
         (origin-message-id (getf message :message-id))
         (start (max (* *max-reply-on-page*
                        (1- page-id))
                     0))
         (user (user-name))
         (adminp (if user (storage-admin-p *storage* user))))
    (list :list-forums-href (genurl 'list-forums)
          :js (js-urls)
          :rss-href (restas:genurl 'topic-rss
                                   :topic-id topic-id)
          :parent-forum (forum-info-plist (getf message :forum))
          :message (list* :href-reply (restas:genurl 'create-reply
                                                     :message-id origin-message-id)
                          message)
          :pages (topic-pages topic-id page-id)
          :replies (iter (for item in (storage-topic-replies *storage*
                                                             topic-id
                                                             *max-reply-on-page*
                                                             start))
                         (for reply-id = (getf item :id))
                         (collect (list* :href-delete (if adminp
                                                          (restas:genurl 'delete-message
                                                                         :reply-id reply-id))
                                         :href (restas:genurl 'view-reply
                                                              :reply-id reply-id)
                                         :href-reply (restas:genurl 'create-reply
                                                                    :message-id reply-id)
                                         :prev-msg (let ((prev-id (getf item :prev-id)))
                                                     (if (and (not (eql prev-id :null))
                                                              (not (eql prev-id origin-message-id)))
                                                         (list :author (getf item :prev-author)
                                                               :created (getf item :prev-created)
                                                               :href (restas:genurl 'view-reply
                                                                                    :reply-id (getf item :prev-id)))))
                                         item)))
          :can-create-message user
          :title (getf message :title)
          :colorize (colorize-traits))))

;;;; view topic

(define-route view-topic ("thread/:topic-id"
                          :parse-vars (list :topic-id #'parse-integer))
  (view-topic-page :topic-id topic-id
                   :page-id 1))

;;;; view-reply

(define-route view-reply ("messages/:reply-id"
                          :parse-vars (list :reply-id #'parse-integer))
  (multiple-value-bind (pos topic-id) (storage-reply-position *storage* reply-id)
    (unless topic-id
      (return-from view-reply hunchentoot:+http-not-found+))
    (let ((page (ceiling pos
                         *max-reply-on-page*)))
      (hunchentoot:redirect
       (format nil
               "~A#comment-~A"
               (if (= page 1)
                   (restas:genurl 'view-topic
                                  :topic-id topic-id)
                   (restas:genurl 'view-topic-page
                                  :topic-id topic-id
                                  :page-id page))
               reply-id)))))

;;;; create reply on message

(define-route create-reply ("messages/reply/:message-id"
                            :parse-vars (list :message-id #'parse-integer)
                            :requirement 'user-name)
  (declare (ignore message-id))
  )

(define-route create-reply/post ("messages/reply/:message-id"
                                 :method :post
                                 :parse-vars (list :message-id #'parse-integer)
                                 :requirement 'user-name)
  (let ((body (hunchentoot:post-parameter "body")))
    (when (string= body "")
      (view-reply :reply-id message-id))
    (view-reply :reply-id (storage-create-reply *storage*
                                                message-id
                                                body
                                                (user-name)))))

;;;; delete reply

(define-route delete-message ("message/delete/:(reply-id)"
                              :requirement 'user-name)
  (if (storage-admin-p *storage* (user-name))
      (restas:redirect 'view-topic
                       :topic-id (storage-delete-reply *storage* reply-id))
      hunchentoot:+http-forbidden+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; RSS
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun make-rss-items (items)
  (iter (for item in items)
        (collect (list* :href (restas:genurl-with-host 'view-reply 
                                                       :reply-id (getf item :id))
                        item))))

(define-route all-forums-rss ("rss/all.rss"
                              :content-type "application/rss+xml"
                              :render-method 'restas.forum.view:rss-feed)
  (let ((title (format nil "~A: Форумы" (site-name))))
    (list :title title
          :description title
          :link (restas:genurl-with-host 'list-forums)
          :messages (make-rss-items (storage-all-news *storage* *rss-item-count*)))))
                        
(define-route forum-rss ("rss/:(forum-id).rss"
                         :content-type "application/rss+xml"
                         :render-method 'restas.forum.view:rss-feed)
  (let ((title (format nil
                       "~A: Форум - ~A"
                       (site-name)
                       (first (storage-forum-info *storage* forum-id)))))
    (list :title title
          :description title
          :link (restas:genurl-with-host 'list-topics
                                         :forum-id forum-id)
          :messages (make-rss-items (storage-forum-news *storage* forum-id *rss-item-count*)))))

(define-route topic-rss ("rss/threads/:(topic-id).rss"
                         :parse-vars `(:topic-id ,#'parse-integer)
                         :content-type "application/rss+xml"
                         :render-method 'restas.forum.view:rss-feed)
  (let ((message (storage-topic-message *storage* topic-id)))
    (list :title (format nil
                         "~A: ~A"
                         (site-name)
                         (getf message :title))
          :description (getf message :body)
          :link (restas:genurl-with-host 'view-topic
                                         :topic-id topic-id)
          :messages (make-rss-items (storage-topic-news *storage* topic-id *rss-item-count*)))))
                                       

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Colorize
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-route colorize-code ("colorize"
                             :method :post
                             :render-method 'identity)
  (let ((code (hunchentoot:post-parameter "code"))
        (lang (hunchentoot:post-parameter "lang")))
    (colorize::html-colorization (or (find-symbol lang :keyword)
                                     (error "Unknow coloring type: ~A" lang))
                                 code)))

  
