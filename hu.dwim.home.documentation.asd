;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :hu.dwim.home.documentation
  :class hu.dwim.documentation-system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :description "Documentation for hu.dwim.home"
  :licence "BSD / Public domain"
  :depends-on (:hu.dwim.home.test
               :hu.dwim.wui)
  :components ((:module "documentation"
                :components ((:file "package")
                             (:file "home" :depends-on ("package"))))))
