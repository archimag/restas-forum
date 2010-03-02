;;;; restas-forum.asd
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem restas-forum
  :depends-on (#:restas #:closure-template #:local-time)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "storage" :depends-on ("packages"))
                                     (:file "forum" :depends-on ("storage"))))))