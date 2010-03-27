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

(defgeneric storage-list-forums (storage)
  (:documentation "Returns a list of forums")
  (:method (storage)
    (error "#'storage-list-forums not implemented")))

(defgeneric storage-list-topics (storage forum limit offset)
  (:documentation "Returns a list of topics is starting from a position
 of 'offset and not more than 'limit")
  (:method (storage forum limit offset)
    (error "#'storage-list-topics not implemented")))

(defgeneric storage-forum-info (storage forum)
  (:method (storage forum)
    (error "#'storage-forum-info not implemented")))

(defgeneric storage-topic-message (storage topic)
  (:method (storage topic)
    (error "#'storage-list-messages not implemented")))

(defgeneric storage-topic-replies (storage topic limit offset)
  (:method (storage topic limit offset)
    (error "#'storage-topic-replies not implemented")))

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