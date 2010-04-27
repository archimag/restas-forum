;;;; storage.lisp
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.forum)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; storage generic interface
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric storage-admin-p (storage user)
  (:documentation "Return T if USER is forum admin")
  (:method (storage user)
    nil))

(defgeneric storage-list-forums (storage)
  (:documentation "Returns a list of forums")
  (:method (storage)
    (error "#'storage-list-forums not implemented")))

(defgeneric storage-list-topics (storage forum limit offset)
  (:documentation "Returns a list of topics is starting from a position
 of 'offset and not more than 'limit")
  (:method (storage forum limit offset)
    (error "#'storage-list-topics not implemented")))

(defgeneric storage-create-topic (storage forum title body user)
  (:documentation "Create new forum topic")
  (:method (storage forum title body user)
    (error "#'storage-create-topic not implemented")))

(defgeneric storage-delete-topic (storage topic)
  (:documentation "Delete topic")
  (:method (storage topic)
    (error "#'storage-delete-topic not implemented")))

(defgeneric storage-forum-info (storage forum)
  (:method (storage forum)
    (error "#'storage-forum-info not implemented")))

(defgeneric storage-topic-message (storage topic)
  (:method (storage topic)
    (error "#'storage-list-messages not implemented")))

(defgeneric storage-topic-replies (storage topic limit offset)
  (:method (storage topic limit offset)
    (error "#'storage-topic-replies not implemented")))

(defgeneric storage-create-reply (storage topic body user)
  (:documentation "Create new reply")
  (:method (storage topic body user)
    (error "#'storage-create-reply not implemented")))

(defgeneric storage-delete-reply (storage reply)
  (:documentation "Delete reply")
  (:method (storage reply)
    (error "#'stroage-delete-reply not implemented")))

(defgeneric storage-all-news (storage limit)
  (:documentation "Return all new message")
  (:method (storage limit)
    (error "#'storage-all-news not implemented")))

(defgeneric storage-forum-news (storage forum limit)
  (:documentation "Return all new messages on one forum")
  (:method (storage forum limit)
    (error "#'storage-forum-news not implemented")))

(defgeneric storage-topic-news (storage topic limit)
  (:documentation "Return all new messages on one topic")
  (:method (storage topic limit)
    (error "#'storage-topic-news not implemented")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; implementation in-memory storage
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass memory-storage ()
  ())

(defmethod storage-list-forums ((storage memory-storage))
  '(("common-lisp" "Common Lisp")
    ("rulisp" "Обсуждение проекта")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; set default value for *storage*
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setf *storage*
      (make-instance 'memory-storage))