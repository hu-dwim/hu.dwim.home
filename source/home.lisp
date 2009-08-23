;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Parameters

(def (constant :test 'equalp) +script-uris+ '("wui/js/wui.js"))

(def (constant :test 'string=) +page-icon+ "static/favicon.ico")

(def special-variable *stylesheet-uris* (append (flet ((entry (path)
                                                         (list (concatenate-string "static/" path)
                                                               (system-relative-pathname :hu.dwim.home (concatenate-string "www/" path))))
                                                       (dojo-relative-path (path)
                                                         (concatenate-string *dojo-directory-name* path)))
                                                  (list (entry "wui/css/wui.css")
                                                        (entry "wui/css/icon.css")
                                                        (entry "wui/css/widget.css")
                                                        (entry (dojo-relative-path "dijit/themes/tundra/tundra.css"))
                                                        (entry (dojo-relative-path "dojo/resources/dojo.css"))))))

;;;;;;
;;; Application

(def (class* e) home-application (application-with-home-package application-with-dojo-support)
  ()
  (:metaclass funcallable-standard-class))

(def (special-variable e) *home-application* (make-instance 'home-application
                                                            :path-prefix "/"
                                                            :ajax-enabled #t))

(def function make-frame-component (&optional content)
  (frame/widget (:title "dwim.hu"
                 :page-icon +page-icon+
                 :script-uris +script-uris+
                 :stylesheet-uris *stylesheet-uris*)
    (top/widget (:menu-bar (menu-bar/widget ()
                             (make-debug-menu-item)))
      (content/widget ()
        content))))

(def layered-method make-frame-component-with-content ((application home-application) session frame content)
  (make-frame-component content))

;;;;;;
;;; Entry points

(def file-serving-entry-point *home-application* "/static/" (system-relative-pathname :hu.dwim.wui "www/"))

(def js-file-serving-entry-point *home-application* "/wui/js/" (system-relative-pathname :hu.dwim.wui "source/js/"))

;; TODO: this repeated code is quite boring
(def entry-point (*home-application* :path-prefix "project" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (bind ((path (path-of (uri-of *request*)))
             (system-name (string-downcase (subseq path (1+ (position #\/ path :from-end #t))))))
        (setf (root-component-of *frame*)
              (make-frame-component (make-value-inspector (make-instance 'project :path (asdf::system-source-directory system-name))
                                                          :initial-alternative-type 'inspector/abstract)))
        (make-redirect-response-for-current-application (concatenate-string "project/" system-name)))))

(def entry-point (*home-application* :path "" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*) (make-frame-component "dwim.hu"))
        (make-redirect-response-for-current-application))))

(def file-serving-entry-point *home-application* "/static/darcsweb/" #P"/home/levy/workspace/darcsweb/")

;; TODO: why these two falses?
(def entry-point (*home-application* :path "cgi-bin/darcsweb.cgi" :with-session-logic #f :requires-valid-frame #f) ()
  (make-raw-functional-response ()
    (handle-cgi-request #P"/home/levy/workspace/darcsweb/darcsweb.cgi")))

;;;;;;
;;; Server

(def (special-variable e) *home-server* (make-instance 'broker-based-server
                                                       :host +any-host+
                                                       :port 8080
                                                       :brokers (list *home-application*)))
