;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Constants

(def (constant e) +default-database-name+ "hu.dwim.home")

(def (constant e) +default-database-user-name+ "hu.dwim.home")

(def (constant e) +default-database-password+ "engedjbe")

;;;;;;
;;; Application

(def (class* e) home-application (application-with-persistent-login-support
                                  application-with-perec-support
                                  application-with-home-package
                                  application-with-dojo-support)
  ())

(def (special-variable e) *home-application* (make-instance 'home-application
                                                            :database *home-database*))

;;;;;;
;;; Server

(def (class* e) home-server (broker-based-server)
  ())

(def (special-variable e) *home-server* (make-instance 'home-server
                                                       :listen-entries `((:host ,+any-host+ :port 8080)
                                                                         (:host ,+any-host+ :port 8443
                                                                                :ssl-certificate ,(namestring (system-relative-pathname :hu.dwim.web-server "etc/ssl-key/server.crt"))
                                                                                :ssl-key ,(namestring (system-relative-pathname :hu.dwim.web-server "etc/ssl-key/server.key"))))
                                                       :brokers (list* *home-application*
                                                                       ;; TODO this should come from the equivalent of make-default-broker-list in hu.dwim.presentation
                                                                       (make-instance 'js-component-hierarchy-serving-broker :priority 100)
                                                                       (make-default-broker-list))))

(def localization-loader-callback home-localization-loader :hu.dwim.home "localization/")

(def method startup-server :after ((server home-server) &key &allow-other-keys)
  (register-timer-entry/periodic-standalone-test (hu.dwim.web-server::timer-of server)))

;;;;;;
;;; Production

(def function executable-toplevel ()
  "The toplevel function that is called when the dwim Server is started from the command line. For development use (asdf:develop-system :hu.dwim.home) instead."
  (with-standard-toplevel-restarts
    (bind ((options (append (list +help-command-line-option+)
                            (list +quiet-command-line-option+)
                            (copy-command-line-options +database-command-line-options+
                                                       :database-port hu.dwim.rdbms.postgresql::+default-postgresql-database-server-port+
                                                       :database-name +default-database-name+
                                                       :database-user-name +default-database-user-name+
                                                       :database-password +default-database-password+)
                            +generic-command-line-options+))
           (arguments (process-command-line-options options (get-command-line-arguments))))
      (process-help-command-line-argument options arguments)
      (process-quiet-command-line-argument arguments)
      (home.debug "Parsed command line arguments are: ~S" arguments)
      (run-production-server arguments :hu.dwim.home *home-server* *home-application* :database *home-database*))
    +process-return-code/no-error+))
