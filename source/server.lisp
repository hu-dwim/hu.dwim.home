;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Application

(def (class* e) home-application (application-with-home-package application-with-dojo-support)
  ()
  (:metaclass funcallable-standard-class))

(def (special-variable e) *home-application* (make-instance 'home-application
                                                            :path-prefix "/"
                                                            :ajax-enabled #t))

(def layered-method make-frame-component-with-content ((application home-application) session frame content)
  (make-frame-component content))

;;;;;;
;;; Server

(def (constant :test #'equal) +default-home-server-hostname+ "localhost.localdomain")

(def constant +default-home-server-port+ 8080)

(def (special-variable e) *home-server* (make-instance 'broker-based-server
                                                       :host +any-host+
                                                       :port +default-home-server-port+
                                                       :brokers (list *home-application*)))

;;;;;;
;;; Production

(def constant +http-port-command-line-option+
  '("http-port"
    :type integer
    :initial-value #.+default-home-server-port+
    :documentation "The HTTP port where the server will be listening"))

(def function process-http-port-command-line-argument (arguments)
  (when-bind http-port (getf arguments :http-port)
    (setf (hu.dwim.wui::port-of (find +default-home-server-port+ (hu.dwim.wui::listen-entries-of *home-server*)
                                      :key #'hu.dwim.wui::port-of))
          http-port)))

(def function executable-toplevel ()
  (bind ((options (sort (append (list +help-command-line-option+)
                                (list +http-port-command-line-option+)
                                (list +quiet-command-line-option+)
                                +database-command-line-options+
                                +generic-command-line-options+)
                        #'string< :key #'first))
         (arguments (process-command-line-options options (get-command-line-arguments))))
    (process-help-command-line-argument options arguments)
    (process-http-port-command-line-argument arguments)
    (process-quiet-command-line-argument arguments)
    (startup-dwim-server arguments :hu.dwim.home *home-server* *home-application*)))
