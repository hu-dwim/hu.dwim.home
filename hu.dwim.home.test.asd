;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.home.test
  :defsystem-depends-on (hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.test-system
  :licence "BSD / Public domain"
  :depends-on (:hu.dwim.home
               :hu.dwim.stefil+hu.dwim.def)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "home" :depends-on ("suite"))))))
