;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.home
  :class hu.dwim.system
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
                             (:file "logger" :depends-on ("package"))
                             (:file "package")
                             (:file "screen" :depends-on ("test" "server"))
                             (:file "server" :depends-on ("test"))
                             (:file "test" :depends-on ("logger"))
                             (:file "tutorial" :depends-on ("logger"))))))

(defmethod perform :after ((o develop-op) (c (eql (find-system :hu.dwim.home))))
  (eval (let ((*package* (find-package :hu.dwim.home)))
          (read-from-string
           "(progn
              (unless (connection-specification-of *model*)
                (setf (connection-specification-of *model*)
                      `(:host \"localhost\" :port ,hu.dwim.rdbms.postgresql::+default-postgresql-database-server-port+
                        :database ,+default-database-name+ :user-name ,+default-database-user-name+ :password ,+default-database-password+)))
              (setf *database* (database-of *model*))
              (setf hu.dwim.perec::*compiled-query-cache* (make-compiled-query-cache))
              (setf *debug-on-error* t)
              (setf (current-locale) (list \"en\"))
              (setf (running-in-test-mode? *home-application*) t)
              (startup-server *home-server*)
              (hu.dwim.meta-model::export-model))")))
  (warn "Made sideffects on the following global variables: *database*, *compiled-query-cache*, *debug-on-error*, *locale*."))
