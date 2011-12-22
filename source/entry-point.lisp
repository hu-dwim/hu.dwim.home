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
  ("gitweb/"        #P"/usr/share/gitweb/")
  ("mailman/images/" #P"/usr/share/images/mailman/" :priority 100))

(macrolet ((forward (name)
             `(def entry-point (*home-application* file-serving-broker
                                                   :path ,name
                                                   :file-to-serve ,(system-relative-pathname :hu.dwim.home (string+ "www/attila.lendvai/" name))
                                                   :priority most-positive-fixnum))))
  (forward "attila.lendvai.vcf")
  (forward "attila.lendvai.vcf.sig")
  (forward "attila.lendvai.key")
  (forward "attila.lendvai.cv.pdf")
  (forward "attila.lendvai.cv.pdf.sig")
  (forward "attila.lendvai.jpg"))

;;;;;;
;;; CGI

(def entry-points *home-application*
  (cgi-file-broker :path "darcsweb/darcsweb.cgi"
                   :cgi-file (iolib.pathnames:merge-file-paths "darcsweb/darcsweb.cgi" *workspace-directory*)
                   :environment '(("PATH" . "/usr/bin"))
                   :priority 1)
  (cgi-file-broker :path "gitweb/gitweb.cgi"
                   :cgi-file #P"/usr/lib/cgi-bin/gitweb.cgi"
                   :priority 1)
  (cgi-directory-broker :path "mailman"
                        :root-directory "/usr/lib/cgi-bin/mailman/"
                        :command-line-transformer (lambda (cgi-file)
                                                    `("sudo" "-n" "-E" "-u" "list" "-g" "www-data" ,(iolib.pathnames:file-path-namestring cgi-file)))
                        :render-directory-index #f
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

(def entry-point (*home-application* :path "help")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (make-frame-root-component-rendering-response :content-component (make-instance 'usage-help/widget))))

(def (with-macro* :macro-only-arguments var-name) with-symbol-finding-entry-point-logic
    (var-name &key (symbol-name nil symbol-name?)
              (packages ''(:hu.dwim.home
                           :hu.dwim.perec
                           :hu.dwim.presentation
                           :hu.dwim.web-server)))
  (or (when (length= 1 *entry-point-relative-path*)
        (bind ((symbol (find-symbol* (if symbol-name?
                                         symbol-name
                                         (string-upcase (first *entry-point-relative-path*)))
                                     :packages packages :otherwise nil)))
          (when symbol
            (-with-macro/body- (symbol var-name)))))
      (make-not-found-response)))

(def entry-point (*home-application* :path "project")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (with-symbol-finding-entry-point-logic (project-name :packages '(:keyword))
      (awhen (find-project project-name :otherwise #f)
        (make-frame-root-component-rendering-response :content-component (make-value-inspector it))))))

(def entry-point (*home-application* :path "file")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (bind ((pathname (iolib.pathnames:merge-file-paths (join-strings *entry-point-relative-path* "/")
                                                       *workspace-directory*)))
      (if (and (starts-with-subseq (iolib.pathnames:file-path-namestring (truename *workspace-directory*))
                                   (iolib.pathnames:file-path-namestring pathname))
               (iolib.os:file-exists-p pathname))
          (make-frame-root-component-rendering-response :content-component (make-value-inspector pathname))
          (make-not-found-response)))))

(def entry-point (*home-application* :path "definition")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (with-symbol-finding-entry-point-logic (name)
      (awhen (hu.dwim.presentation::make-definitions name)
        (make-frame-root-component-rendering-response :content-component (make-value-inspector it))))))

(def entry-point (*home-application* :path "function")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (with-symbol-finding-entry-point-logic (name)
      (awhen (and (typep name 'function-name)
                  (fdefinition name))
        (make-frame-root-component-rendering-response :content-component (make-value-inspector it))))))

(def entry-point (*home-application* :path "class")
  (with-entry-point-logic (:ensure-session #t :ensure-frame #t)
    (with-symbol-finding-entry-point-logic (name)
      (awhen (find-class name #f)
        (make-frame-root-component-rendering-response :content-component (make-value-inspector it))))))

(def entry-point (*home-application* :path "status")
  (make-server-status-response))
