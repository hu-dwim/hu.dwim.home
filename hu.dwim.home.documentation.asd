;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.home.documentation
  :class hu.dwim.documentation-system
  :depends-on (:hu.dwim.home.test
               :hu.dwim.presentation)
  :components ((:module "documentation"
                :components ((:file "home" :depends-on ("package"))
                             (:file "package")))))
