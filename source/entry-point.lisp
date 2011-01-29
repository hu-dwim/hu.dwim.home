;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Static file serving

(def file-serving-entry-points *home-application*
  ("static/home/"   (system-relative-pathname :hu.dwim.home "www/"))
  ("install/"       (system-relative-pathname :hu.dwim.home "www/install/"))
  ("test/coverage/" (system-relative-pathname :hu.dwim.home "www/test/coverage/"))
  ("darcs/"         #P"/opt/darcs/")
  ("git/"           #P"/opt/git/")
  ("live/"          *workspace-directory*)
  ("darcsweb/"      (merge-pathnames "darcsweb/" *workspace-directory*))
  ("gitweb/"        (merge-pathnames "gitweb/" *workspace-directory*)))

;;;;;;
;;; CGI

(def entry-points *home-application*
  (cgi-broker :path-prefix "darcsweb/darcsweb.cgi"
              :cgi-file (merge-pathnames "darcsweb/darcsweb.cgi" *workspace-directory*)
              :environment '(("PATH" . "/usr/bin"))
              :priority 1)
  (cgi-broker :path-prefix "gitweb/gitweb.cgi"
              :cgi-file (merge-pathnames "gitweb/gitweb.cgi" *workspace-directory*)
              :priority 1))

;;;;;;
;;; Main entry point

(def entry-point (*home-application* :path "")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (make-frame-root-component-rendering-response)))

;;;;;;
;;; Permanent entry points
;;;
;;; TODO: these entry points should not require session/frame if possible
;;; TODO: this repeated code is quite boring

(def entry-point (*home-application* :path-prefix "help/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (make-frame-root-component-rendering-response :content-component (make-instance 'usage-help/widget))))

(def macro with-symbol-finding-entry-point-logic ((var-name symbol-name &optional (packages ''(:hu.dwim.home
                                                                                               :hu.dwim.perec
                                                                                               :hu.dwim.presentation
                                                                                               :hu.dwim.web-server)))
                                                   &body body)
  (once-only (symbol-name)
    `(bind ((,var-name (find-symbol* ,symbol-name :packages ,packages :otherwise nil)))
       (or (when ,var-name
             ,@body)
           (make-not-found-response)))))

(def entry-point (*home-application* :path-prefix "project/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (with-symbol-finding-entry-point-logic (project-name (string-upcase *entry-point-relative-path*) :keyword)
      (awhen (find-project project-name :otherwise #f)
        (make-frame-root-component-rendering-response :content-component (make-value-inspector it))))))

(def entry-point (*home-application* :path-prefix "file/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (bind ((pathname (merge-pathnames *entry-point-relative-path* *workspace-directory*)))
      (if (and (starts-with-subseq (namestring (truename *workspace-directory*)) (namestring pathname))
               (iolib.os:file-exists-p pathname))
          (make-frame-root-component-rendering-response :content-component (make-value-inspector pathname))
          (make-not-found-response)))))

(def entry-point (*home-application* :path-prefix "definition/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (with-symbol-finding-entry-point-logic (name (string-upcase *entry-point-relative-path*))
      (awhen (hu.dwim.presentation::make-definitions name)
        (make-frame-root-component-rendering-response :content-component (make-value-inspector it))))))

(def entry-point (*home-application* :path-prefix "function/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (with-symbol-finding-entry-point-logic (name (string-upcase *entry-point-relative-path*))
      (awhen (and (typep name 'function-name)
                  (fdefinition name))
        (make-frame-root-component-rendering-response :content-component (make-value-inspector it))))))

(def entry-point (*home-application* :path-prefix "class/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (with-symbol-finding-entry-point-logic (name (string-upcase *entry-point-relative-path*))
      (awhen (find-class name #f)
        (make-frame-root-component-rendering-response :content-component (make-value-inspector it))))))

(def entry-point (*home-application* :path-prefix "status")
  (make-server-status-response))
