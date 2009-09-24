;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

(def constant +default-echo-server-port+ (+ 1 +default-home-server-port+))

(def (special-variable e) *echo-server* (make-instance 'server
                                                       :host +any-host+
                                                       :port +default-echo-server-port+
                                                       :handler (lambda ()
                                                                  (send-response (make-request-echo-response)))))

;;;;;;
;;; Instead of starting the server make an entry point in the application.
;;; The server would need other ports to be open.

(def entry-point (*home-application* :path-prefix "echo" :with-session-logic #f) ()
  (make-request-echo-response))
