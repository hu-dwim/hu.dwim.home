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

(def entry-point (*home-application* :path-prefix "function" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (bind ((path (path-of (uri-of *request*)))
             (function-name (string-downcase (subseq path (1+ (position #\/ path :from-end #t))))))
        (setf (root-component-of *frame*)
              (make-frame-component (make-value-inspector (fdefinition (bind ((*read-eval* #f))
                                                                         (read-from-string function-name)))
                                                          :initial-alternative-type 't/lisp-form/inspector)))
        (make-redirect-response-for-current-application (concatenate-string "function/" function-name)))))

(def entry-point (*home-application* :path-prefix "class" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (bind ((path (path-of (uri-of *request*)))
             (class-name (string-downcase (subseq path (1+ (position #\/ path :from-end #t))))))
        (setf (root-component-of *frame*)
              (make-frame-component (make-value-inspector (find-class (bind ((*read-eval* #f))
                                                                        (read-from-string class-name)))
                                                          :initial-alternative-type 't/lisp-form/inspector)))
        (make-redirect-response-for-current-application (concatenate-string "class/" class-name)))))

(def entry-point (*home-application* :path "" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*) (make-frame-component (make-value-inspector (find-book 'hu.dwim.home/install-guide)
                                                                                      :initial-alternative-type 'book/text/inspector)))
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

;;;;;;
;;; Production

(def logger log ())

(def function production-image-toplevel ()
  (hu.dwim.meta-model::production-image-toplevel :hu.dwim.home *home-server* *home-application*))

;;;;;;
;;; TODO: move

(def book hu.dwim.home/install-guide (:title "Install guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "The installation guide describes how to setup the same web service that is running at "
      (parse-uri "http://dwim.hu/")
      " on your local computer."))
  (chapter (:title "Installing Ubuntu")
    (paragraph ()
      (parse-uri "http://www.ubuntu.com/")))
  (chapter (:title "Creating the workspace")
    (shell-script ()
      "cd ~"
      "mkdir workspace"))
  (chapter (:title "Installing SBCL")
    (paragraph ()
      (parse-uri "http://www.sbcl.org/"))))

#|
Steps to reproduce the http://dwim.hu/ website on your local computer:
 - ubuntu linux - http://www.ubuntu.com/

 - terminal - click the menu item Applications/Accessories/Terminal

 - workspace
   $ cd ~
   $ mkdir workspace

 - sbcl - http://www.sbcl.org/
   $ sudo apt-get install cvs clisp
   $ cd ~/workspace
   $ cvs -z3 -d :pserver:anonymous:anonymous@sbcl.cvs.sourceforge.net:/cvsroot/sbcl co -P sbcl
   $ cd sbcl
   $ wget http://dwim.hu/static/sbcl/cutomize-target-features.lisp
   $ ./make.sh "clisp -ansi -on-error abort"
   $ sudo ./install.sh

 - PostgreSQL - http://www.postgresql.org/
   $ sudo apt-get install postgresql

 - PostgreSQL database and users
   $ sudo su - postgres
   $ createdb hu.dwim.home
   $ createuser -P hu.dwim.home --no-superuser --no-createdb --no-createrole
     engedjbe
     engedjbe
   $ exit

 - sqlite - http://www.sqlite.org/
   $ sudo apt-get install sqlite3

 - oracle

 - java
   $ sudo apt-get install sun-java6-jdk

 - repos
   $ darcs get http://dwim.hu/darcs/hu.dwim.asdf
   ...

 - build image
   $ ~/workspace/hu.dwim.environment/bin/build-image hu.dwim.home

 - startup
   $ ~/workspace/hu.dwim.home/server

 - shutdown
   press Control-C

 - hu.dwim.home - http://localhost.localdomain/

 - test suite - http://localhost.localdomain/test/

 - emacs - http://www.gnu.org/software/emacs/
   $ sudo apt-get install emacs-snapshot
   $ cd ~
   $ wget http://dwim.hu/install/.emacs

 - slime - http://common-lisp.net/project/slime/
   $ cd workspace
   $ cvs -z3 -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot co slime

 - connect server with slime
   $ emacs
     M-x slime
|#
