;;;; restas-forum.asd
;;;;
;;;; This file is part of the restas-forum library, released under Lisp-LGPL.
;;;; See file COPYING for details.
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(defsystem restas-forum
  :depends-on (#:restas #:closure-template #:restas-directory-publisher
                        #:local-time #:metabang-bind)
  :components ((:module "src"
                        :components ((:file "defmodule")
                                     (:file "storage" :depends-on ("defmodule"))
                                     (:file "forum" :depends-on ("storage"))))))