;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Application

(def (class* e) home-application (application-with-persistent-login-support
                                  application-with-perec-support
                                  application-with-home-package
                                  application-with-dojo-support)
  ())

(def (special-variable e) *home-application* (make-instance 'home-application
                                                            :database *home-database*
                                                            :dojo-script-uri "static/home/dojotoolkit-1.12.2/dojo/dojo.js"
                                                            :dojo-base-uri "static/home/dojotoolkit-1.12.2/"))

;;;;;;
;;; Server

(def (class* e) home-server (broker-based-server)
  ())

(def (special-variable e) *home-server* (make-instance 'home-server
                                                       :listen-entries `((:host ,+any-host+ :port 8080)
                                                                         #+nil
                                                                         (:host ,+any-host+ :port 8443
                                                                          :ssl-certificate ,(namestring (system-relative-pathname :hu.dwim.web-server "etc/ssl-key/server.crt"))
                                                                          :ssl-key ,(namestring (system-relative-pathname :hu.dwim.web-server "etc/ssl-key/server.key"))))
                                                       :brokers (list* *home-application*
                                                                       ;; TODO this should come from the equivalent of make-default-broker-list in hu.dwim.presentation
                                                                       (make-instance 'js-component-hierarchy-serving-broker :priority 100)
                                                                       (make-default-broker-list))))

;; register a callback that will load our own localization files
(def localization-loader-callback home-localization-loader :hu.dwim.home "localization/")

#+nil
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
           (arguments (parse-command-line-arguments options)))
      (home.debug "Parsed command line arguments are: ~S" arguments)
      (assert (not (null arguments)) (arguments) "Something is wrong: command line arguments are empty, but at least the default values should be there.")
      (process-help-command-line-argument options arguments)
      (process-quiet-command-line-argument arguments)
      (run-production-server arguments :hu.dwim.home *home-server* *home-application* :database *home-database*))
    +process-return-code/no-error+))
