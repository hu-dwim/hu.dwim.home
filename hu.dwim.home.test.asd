;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.home.test
  :class hu.dwim.test-system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :description "Test suite for hu.dwim.home"
  :licence "BSD / Public domain"
  :depends-on (:hu.dwim.home
               :hu.dwim.def+hu.dwim.stefil)
  :components ((:module "test"
                :components ((:file "package")
                             (:file "suite" :depends-on ("package"))
                             (:file "home" :depends-on ("suite"))))))

(defmethod perform :after ((op develop-op) (system (eql (find-system :hu.dwim.home))))
  (let ((*package* (find-package :hu.dwim.home)))
    (eval
     (read-from-string
      "(progn
         (setf (log-level 'hu.dwim.wui::wui) +debug+)
         (setf *debug-on-error* t))")))
  (warn "Set WUI log level to +debug+; enabled server-side debugging"))
