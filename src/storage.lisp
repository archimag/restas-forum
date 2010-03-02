;;;; storage.lisp
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:restas.forum)


(defgeneric all-forums (storage))

;;;; storage in memory

(defclass memory-storage ()
  ())

(defmethod all-forums ((storage memory-storage))
  '(("common-lisp" "Common Lisp")
    ("rulisp" "Обсуждение проекта")))
