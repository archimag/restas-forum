;;;; simple-forum.lisp
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(asdf:operate 'asdf:load-op '#:restas-forum)

;; (restas:define-module #:simple.forum
;;   (:use #:cl))

;; (in-package #:simple.forum)

;; (restas:define- core  (#:restas.forum))

;; (restas:start-site '#:simple.forum :port 8080)

(restas:start '#:simple.forum :port 8080)