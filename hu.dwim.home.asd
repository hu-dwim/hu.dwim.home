;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.home
  :defsystem-depends-on (hu.dwim.asdf)
  :class hu.dwim.asdf:hu.dwim.system
  :description "The web application running at http://dwim.hu"
  :depends-on (:cl-smtp
               :hu.dwim.logger
               :hu.dwim.meta-model
               :hu.dwim.model
               :hu.dwim.perec+iolib
               :hu.dwim.presentation.test
               :hu.dwim.presentation+cl-typesetting
               :hu.dwim.presentation+hu.dwim.perec
               :hu.dwim.presentation+hu.dwim.reader
               :hu.dwim.presentation+hu.dwim.stefil
               :sb-cover)
  :components ((:module "source"
                :components ((:file "authentication" :depends-on ("server"))
                             (:file "echo-server" :depends-on ("server"))
                             (:file "entry-point" :depends-on ("server"))
                             (:file "hello-world-server" :depends-on ("server"))
                             (:file "install-guide" :depends-on ("screen"))
                             (:file "logger" :depends-on ("package" "variables"))
                             (:file "package")
                             (:file "screen" :depends-on ("test" "server"))
                             (:file "server" :depends-on ("test"))
                             (:file "test" :depends-on ("logger"))
                             (:file "tutorial" :depends-on ("logger"))
                             (:file "variables" :depends-on ("package"))))))

(defmethod perform :after ((o hu.dwim.asdf:develop-op) (c (eql (find-system :hu.dwim.home))))
  (eval (let ((*package* (find-package :hu.dwim.home)))
          (read-from-string
           "(progn
              (setf *database* *home-database*)
              (unless (connection-specification-of *database*)
                (setf (connection-specification-of *database*)
                      `(:host \"localhost\" :port ,hu.dwim.rdbms.postgresql::+default-postgresql-database-server-port+
                        :database ,+default-database-name+ :user-name ,+default-database-user-name+ :password ,+default-database-password+)))
              (setf hu.dwim.perec::*compiled-query-cache* (make-compiled-query-cache))
              (setf *debug-on-error* t)
              (setf (current-locale) (list \"en\"))
              (setf (running-in-test-mode? *home-application*) t)
              (startup-server *home-server*)
              (export-persistent-classes-to-database-schema))")))
  (warn "Made sideffects on the following global variables: *database*, *compiled-query-cache*, *debug-on-error*, *locale*."))
