;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

(def constant +default-hello-world-server-port+ (+ 2 +default-home-server-port+))

(def special-variable *hello-world-server* (make-instance 'server
                                                          :host +any-host+
                                                          :port +default-hello-world-server-port+
                                                          :handler (lambda ()
                                                                     (send-response (make-hello-world-response)))))

(def function make-hello-world-response ()
  (make-functional-html-response ()
    <html
      <head
        <title "hu.dwim.wui hello world server">>
      <body
        <h1 (:style "font-style: italic;")
          "hello world">>>))

;;;;;;
;;; Instead of starting the server make an entry point in the application.
;;; The server would need other ports to be open.

(def entry-point (*home-application* :path-prefix "hello-world" :with-session-logic #f) ()
  (make-hello-world-response))
