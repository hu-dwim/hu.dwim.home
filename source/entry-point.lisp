;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Entry points


(def file-serving-entry-point *home-application* "/static/" (system-relative-pathname :hu.dwim.home "www/"))

(def file-serving-entry-point *home-application* "/install/" (system-relative-pathname :hu.dwim.home "www/install/"))

(def file-serving-entry-point *home-application* "/darcs/" #P"/opt/darcs/")

(def file-serving-entry-point *home-application* "/live/" common-lisp-user::*workspace-directory*)

(def file-serving-entry-point *home-application* "/darcsweb/" (merge-pathnames "darcsweb/" common-lisp-user::*workspace-directory*))

(def js-file-serving-entry-point *home-application* "/wui/js/" (system-relative-pathname :hu.dwim.wui "source/js/"))

;; TODO: these entry points should not require session/frame if possible
;; TODO: this repeated code is quite boring

(def entry-point (*home-application* :path-prefix "help/" :ensure-session #t :ensure-frame #t) ()
  (setf (root-component-of *frame*) (make-frame-component (make-instance 'usage-help/widget)))
  (make-root-component-rendering-response *frame*))

(def entry-point (*home-application* :path-prefix "project/" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*)
              ;; FIXME: leaking symbols
              (make-frame-component (make-value-inspector (find-project (intern (string-upcase *entry-point-relative-path*) :keyword))
                                                          :initial-alternative-type 'inspector/abstract)))
        (make-redirect-response-for-current-application (string+ "project/" *entry-point-relative-path*)))))

(def entry-point (*home-application* :path-prefix "file/" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (bind ((pathname (merge-pathnames *entry-point-relative-path* common-lisp-user::*workspace-directory*)))
        (when (starts-with-subseq (namestring (truename common-lisp-user::*workspace-directory*)) (namestring pathname))
          (setf (root-component-of *frame*)
                (make-frame-component (make-value-inspector pathname :initial-alternative-type 'pathname/lisp-file/inspector)))
          (make-redirect-response-for-current-application (string+ "file/" *entry-point-relative-path*))))))

(def entry-point (*home-application* :path-prefix "function/" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*)
              (make-frame-component (make-value-inspector (fdefinition (bind ((*read-eval* #f))
                                                                         (read-from-string *entry-point-relative-path*)))
                                                          :initial-alternative-type 't/lisp-form/inspector)))
        (make-redirect-response-for-current-application (string+ "function/" *entry-point-relative-path*)))))

(def entry-point (*home-application* :path-prefix "class/" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*)
              (make-frame-component (make-value-inspector (find-class (bind ((*read-eval* #f))
                                                                        (read-from-string *entry-point-relative-path*)))
                                                          :initial-alternative-type 't/lisp-form/inspector)))
        (make-redirect-response-for-current-application (string+ "class/" *entry-point-relative-path*)))))

(def entry-point (*home-application* :path "" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*) (make-frame-component))
        (make-redirect-response-for-current-application))))

(def entry-point (*home-application* :path "cgi-bin/darcsweb.cgi" :with-session-logic #f) ()
  (make-raw-functional-response ()
    (handle-cgi-request (merge-pathnames "darcsweb/darcsweb.cgi" common-lisp-user::*workspace-directory*))))
