;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Echo server

(def constant +default-echo-server-port+ (+ 1 +default-http-server-port+))

(def special-variable *echo-server* (make-instance 'server
                                                   :host +any-host+
                                                   :port +default-echo-server-port+
                                                   :handler (lambda ()
                                                              (hu.dwim.web-server::send-response (make-request-echo-response))))
  "The echo server echoes back various parts of the received HTTP request, such as request path, HTTP headers, request parameters, etc.")

(def function startup-echo-server ()
  "Starts up the echo server"
  (startup-server *echo-server*))

(def function shutdown-echo-server ()
  "Shuts down the echo server"
  (shutdown-server *echo-server*))

;;;;;;
;;; Instead of starting up the echo server, we make an entry point in the home application.
;;; The echo server would need another port to be open, and we don't want to do that for a simple example.

(def entry-point (*home-application* :path-prefix "echo")
  (make-request-echo-response))
