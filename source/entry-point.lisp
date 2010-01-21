;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Serving entry points

(macrolet ((entry-point ((class &rest initargs) &body body)
             `(def entry-point (*home-application* ,class ,@initargs)
                ,@body))
           (file-serving-entry-point (path-prefix root-directory)
             `(def file-serving-entry-point *home-application* ,path-prefix ,root-directory)))

  (file-serving-entry-point "static/"   (system-relative-pathname :hu.dwim.home "www/"))
  (file-serving-entry-point "install/"  (system-relative-pathname :hu.dwim.home "www/install/"))
  (file-serving-entry-point "darcs/"    #P"/opt/darcs/")
  (file-serving-entry-point "git/"      #P"/opt/git/")
  (file-serving-entry-point "live/"     *workspace-directory*)
  (file-serving-entry-point "darcsweb/" (merge-pathnames "darcsweb/" *workspace-directory*))
  (file-serving-entry-point "gitweb/"   (merge-pathnames "gitweb/" *workspace-directory*))

  (entry-point (cgi-broker :path-prefix "darcsweb/darcsweb.cgi"
                           :cgi-file (merge-pathnames "darcsweb/darcsweb.cgi" *workspace-directory*)
                           :environment '(("PATH" . "/usr/bin"))
                           :priority 1))

  (entry-point (cgi-broker :path-prefix "gitweb/gitweb.cgi"
                           :cgi-file (merge-pathnames "gitweb/gitweb.cgi" *workspace-directory*)
                           :priority 1)))

;;;;;;
;;; Login entry point

(def identifier-and-password-login-entry-point *home-application*
  :with-session-logic #t :ensure-session #t :ensure-frame #t
  :frame-component-factory 'make-frame-component
  :authentication-instrument-iterator 'iterate-possible-authentication-instruments-based-on-login-identifier)

;;;;;;
;;; Main entry point

(def entry-point (*home-application* :path "")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (if (root-component-of *frame*)
        (make-root-component-rendering-response *frame*)
        (progn
          (setf (root-component-of *frame*) (make-frame-component))
          (make-redirect-response-for-current-application)))))

;;;;;;
;;; Permanent entry points
;;;
;;; TODO: these entry points should not require session/frame if possible
;;; TODO: this repeated code is quite boring

(def entry-point (*home-application* :path-prefix "help/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (setf (root-component-of *frame*) (make-frame-component (make-instance 'usage-help/widget)))
    (make-root-component-rendering-response *frame*)))

(def macro with-symbol-finding-entry-point-logic ((var-name symbol-name &optional (packages ''(:hu.dwim.home
                                                                                               :hu.dwim.perec
                                                                                               :hu.dwim.wui)))
                                                   &body body)
  (once-only (symbol-name)
    `(bind ((,var-name (find-symbol* ,symbol-name :packages ,packages :otherwise nil)))
       (or (when ,var-name
             ,@body)
           (make-not-found-response)))))

(def entry-point (*home-application* :path-prefix "project/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (if (root-component-of *frame*)
        (make-root-component-rendering-response *frame*)
        (with-symbol-finding-entry-point-logic (project-name (string-upcase *entry-point-relative-path*) :keyword)
          (awhen (find-project project-name :otherwise nil)
            (setf (root-component-of *frame*) (make-frame-component (make-value-inspector it)))
            (make-redirect-response-for-current-application (string+ "project/" *entry-point-relative-path*)))))))

(def entry-point (*home-application* :path-prefix "file/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (if (root-component-of *frame*)
        (make-root-component-rendering-response *frame*)
        (bind ((pathname (merge-pathnames *entry-point-relative-path* *workspace-directory*)))
          (if (starts-with-subseq (namestring (truename *workspace-directory*)) (namestring pathname))
              (progn
                (setf (root-component-of *frame*) (make-frame-component (make-value-inspector pathname)))
                (make-redirect-response-for-current-application (string+ "file/" *entry-point-relative-path*)))
              (make-not-found-response))))))

(def function make-uri-to-workspace-location (workspace-relative-path)
  (make-uri :scheme "http" :host "dwim.hu" :path (string+ "/file/" workspace-relative-path)))

(def entry-point (*home-application* :path-prefix "definition/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (if (root-component-of *frame*)
        (make-root-component-rendering-response *frame*)
        (with-symbol-finding-entry-point-logic (name (string-upcase *entry-point-relative-path*))
          (awhen (hu.dwim.wui::make-definitions name)
            (setf (root-component-of *frame*) (make-frame-component (make-value-inspector it)))
            (make-redirect-response-for-current-application (string+ "definition/" *entry-point-relative-path*)))))))

(def entry-point (*home-application* :path-prefix "function/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (if (root-component-of *frame*)
        (make-root-component-rendering-response *frame*)
        (with-symbol-finding-entry-point-logic (name (string-upcase *entry-point-relative-path*))
          (awhen (and (typep name 'function-name)
                      (fdefinition name))
            (setf (root-component-of *frame*) (make-frame-component (make-value-inspector it)))
            (make-redirect-response-for-current-application (string+ "function/" *entry-point-relative-path*)))))))

(def entry-point (*home-application* :path-prefix "class/")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (if (root-component-of *frame*)
        (make-root-component-rendering-response *frame*)
        (with-symbol-finding-entry-point-logic (name (string-upcase *entry-point-relative-path*))
          (awhen (find-class name #f)
            (setf (root-component-of *frame*) (make-frame-component (make-value-inspector it)))
            (make-redirect-response-for-current-application (string+ "class/" *entry-point-relative-path*)))))))

(def entry-point (*home-application* :path-prefix "status")
  (make-server-status-response))
