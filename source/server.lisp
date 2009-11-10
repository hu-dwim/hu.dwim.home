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

;; to build dojo:
;; $ svn up --revision 20743     # in each of the first descendant dirs of the dojo checkout
;; $ ~/workspace/hu.dwim.wui/etc/build-dojo.sh --dojo ~/workspace/dojo/ --dojo-release-dir ~/workspace/hu.dwim.home/www/ --profile ~/workspace/hu.dwim.home/etc/hu.dwim.home.profile.js --locales "en-us,hu"
(def (special-variable e) *home-application* (make-instance 'home-application
                                                            :path-prefix "/"
                                                            :dojo-directory-name "dojo-20743/" ; keep track of the exact dojo version which is tested
                                                            :ajax-enabled #t))

(def layered-method make-frame-component-with-content ((application home-application) session frame content)
  (make-frame-component content))

;;;;;;
;;; Server

(def (special-variable e) *home-server* (make-instance 'broker-based-server
                                                       :host +any-host+
                                                       :port +default-http-server-port+
                                                       :brokers (list *home-application*)))

;;;;;;
;;; Production

(def function executable-toplevel ()
  "The toplevel function that is called when the dwim Server is started."
  (with-standard-toplevel-restarts
    (bind ((options (append (list +help-command-line-option+)
                            (list +http-server-port-command-line-option+)
                            (list +quiet-command-line-option+)
                            +database-command-line-options+
                            +generic-command-line-options+))
           (arguments (process-command-line-options options (get-command-line-arguments))))
          (process-help-command-line-argument options arguments)
          (process-http-server-port-command-line-argument arguments *home-server*)
          (process-quiet-command-line-argument arguments)
          (run-production-server arguments :hu.dwim.home *home-server* *home-application*))
    ;; process exit code
    0))
