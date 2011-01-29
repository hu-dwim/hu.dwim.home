;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Hello world server

(def constant +default-hello-world-server-port+ (+ 2 +default-http-server-port+))

(def special-variable *hello-world-server* (make-instance 'server
                                                          :host +any-host+
                                                          :port +default-hello-world-server-port+
                                                          :handler (lambda ()
                                                                     (hu.dwim.web-server::send-response (make-hello-world-response))))
  "The hello world server unconditionally sends back a simple HTML page with the usual content.")

(def function make-hello-world-response ()
  (make-functional-html-response ()
    <html
      <head
        <title "hu.dwim.web-server hello world server">>
      <body
        <h1 (:style "font-style: italic;")
          "hello world">>>))

(def function startup-hello-world-server ()
  "Starts up the hello world server"
  (startup-server *hello-world-server*))

(def function shutdown-hello-world-server ()
  "Shuts down the hello world server"
  (shutdown-server *hello-world-server*))

;;;;;;
;;; Instead of starting the hello world server, we make an entry point in the home application.
;;; The hello world server would need another port to be open, and we don't want to do that for a simple example.

(def entry-point (*home-application* :path-prefix "hello-world")
  (make-hello-world-response))
