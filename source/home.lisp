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

(def file-serving-entry-point *home-application* "/static/" (system-relative-pathname :hu.dwim.home "www/"))

(def file-serving-entry-point *home-application* "/install/" (system-relative-pathname :hu.dwim.home "www/install"))

(def file-serving-entry-point *home-application* "/darcs/" #P"/opt/darcs/")

(def file-serving-entry-point *home-application* "/darcsweb/" (truename (system-relative-pathname :hu.dwim.home "../darcsweb/")))

(def js-file-serving-entry-point *home-application* "/wui/js/" (system-relative-pathname :hu.dwim.wui "source/js/"))

;; TODO: this repeated code is quite boring
(def entry-point (*home-application* :path-prefix "project/" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*)
              (make-frame-component (make-value-inspector (make-instance 'project :path (asdf::system-source-directory *entry-point-relative-path*))
                                                          :initial-alternative-type 'inspector/abstract)))
        (make-redirect-response-for-current-application (concatenate-string "project/" *entry-point-relative-path*)))))

(def entry-point (*home-application* :path-prefix "file/" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (bind ((pathname (truename (system-relative-pathname :hu.dwim.home (concatenate-string "../" *entry-point-relative-path*)))))
        (when (ends-with-subseq ".lisp" (namestring pathname))
          (setf (root-component-of *frame*)
                (make-frame-component (make-value-inspector pathname :initial-alternative-type 'pathname/lisp-file/inspector)))
          (make-redirect-response-for-current-application (concatenate-string "file/" *entry-point-relative-path*))))))

(def entry-point (*home-application* :path-prefix "function/" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*)
              (make-frame-component (make-value-inspector (fdefinition (bind ((*read-eval* #f))
                                                                         (read-from-string *entry-point-relative-path*)))
                                                          :initial-alternative-type 't/lisp-form/inspector)))
        (make-redirect-response-for-current-application (concatenate-string "function/" *entry-point-relative-path*)))))

(def entry-point (*home-application* :path-prefix "class/" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*)
              (make-frame-component (make-value-inspector (find-class (bind ((*read-eval* #f))
                                                                        (read-from-string *entry-point-relative-path*)))
                                                          :initial-alternative-type 't/lisp-form/inspector)))
        (make-redirect-response-for-current-application (concatenate-string "class/" *entry-point-relative-path*)))))

(def entry-point (*home-application* :path "" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*) (make-frame-component (make-value-inspector (find-book 'hu.dwim.home/install-guide)
                                                                                      :initial-alternative-type 'book/text/inspector)))
        (make-redirect-response-for-current-application))))

;; TODO: why these two falses?
(def entry-point (*home-application* :path "cgi-bin/darcsweb.cgi" :with-session-logic #f :requires-valid-frame #f) ()
  (make-raw-functional-response ()
    (handle-cgi-request (truename (system-relative-pathname :hu.dwim.home "../darcsweb/darcsweb.cgi")))))

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

(def function collect-get-repository-shell-script-commands ()
  (sort (iter (with workspace-directory-prefix-length = (length (namestring cl-user::*workspace-directory*)))
              (for pathname :in (directory (merge-pathnames cl-user::*workspace-directory* "/*.*")))
              (unless (or (pathname-name pathname)
                          (char= #\. (first-elt (last-elt (pathname-directory pathname)))))
                (bind ((pathname-string (namestring pathname))
                       (name (subseq pathname-string workspace-directory-prefix-length (1- (length pathname-string)))))
                  (unless (search "sbcl" name)
                    (format *debug-io* "Getting repository information for ~A~%" pathname-string)
                    (collect (cond ((search "hu.dwim" name)
                                    (concatenate-string "darcs get http://dwim.hu/darcs/" name))
                                   ((probe-file (merge-pathnames "_darcs" pathname))
                                    (bind ((darcs-info (with-output-to-string (output)
                                                         (sb-ext:run-program "/usr/bin/darcs"
                                                                             `("show" "repo" "--repodir" ,pathname-string)
                                                                             :output output)))
                                           ((:values nil groups) (cl-ppcre:scan-to-strings ".*Default Remote: (.*?)/?\\n.*" darcs-info))
                                           (repository (first-elt groups)))
                                      (concatenate-string "darcs get " repository)))
                                   ((probe-file (merge-pathnames ".git" pathname))
                                    (bind ((git-info (with-output-to-string (output)
                                                       (sb-ext:run-program "/usr/bin/git"
                                                                           `("--git-dir" ,(concatenate-string pathname-string "/.git") "remote" "show" "origin" "-n")
                                                                           :output output)))
                                           ((:values nil groups) (cl-ppcre:scan-to-strings ".*URL: (.*?)/?\\n.*" git-info))
                                           (repository (first-elt groups)))
                                      (concatenate-string "git clone " repository)))
                                   ((probe-file (merge-pathnames ".svn" pathname))
                                    (bind ((svn-info (with-output-to-string (output)
                                                       (sb-ext:run-program "/usr/bin/svn"
                                                                           `("info" ,pathname-string)
                                                                           :output output)))
                                           ((:values nil groups) (cl-ppcre:scan-to-strings ".*URL: (.*?)/?\\n.*" svn-info))
                                           (repository (first-elt groups)))
                                      (concatenate-string "svn checkout " repository " " name)))
                                   ((probe-file (merge-pathnames "CVS" pathname))
                                    (bind ((repository (string-trim-whitespace (read-file-into-string (merge-pathnames "CVS/Root" pathname)))))
                                      (concatenate-string "cvs -z3 -d " repository " checkout")))
                                   (t
                                    (concatenate-string "TODO: " name))))))))
        #'string<))

(def book hu.dwim.home/install-guide (:title "Install Guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "The Install Guide describes how to install and configure the same web service that is running at "
      (parse-uri "http://dwim.hu/") " on your local computer."))
  (chapter (:title "Install Ubuntu")
    (paragraph ()
      (parse-uri "http://www.ubuntu.com/")))
  (chapter (:title "Creating the workspace")
    (shell-script ()
      "cd ~"
      "mkdir workspace"))
  (chapter (:title "Install SBCL")
    (paragraph ()
      (parse-uri "http://www.sbcl.org/"))
    (shell-script ()
      "sudo apt-get install cvs clisp"
      "cd ~/workspace"
      "cvs -z3 -d :pserver:anonymous@sbcl.cvs.sourceforge.net:/cvsroot/sbcl checkout -P sbcl"
      "cd sbcl"
      "wget http://dwim.hu/install/customize-target-features.lisp"
      "./make.sh \"clisp -ansi -on-error abort\""
      "sudo sh ./install.sh"))
  (chapter (:title "Install PostgreSQL")
    (paragraph ()
      (parse-uri "http://www.postgresql.org/"))
    (shell-script ()
      "sudo apt-get install postgresql"))
  (chapter (:title "Configure PostgreSQL")
    (paragraph ()
      (parse-uri "http://www.postgresql.org/"))
    (shell-script ()
      "sudo su - postgres"
      "createdb hu.dwim.home"
      "createuser -P hu.dwim.home --no-superuser --no-createdb --no-createrole"
      "exit")
    (paragraph ()
      "When prompted for the password type in 'engedjbe'"))
  (chapter (:title "Install Sqlite")
    (paragraph ()
      (parse-uri "http://www.sqlite.org/"))
    (shell-script ()
      "sudo apt-get install sqlite3"))
  (chapter (:title "Install Oracle")
    (paragraph ()
      (parse-uri "http://www.oracle.com/")))
  (chapter (:title "Install Java")
    (paragraph ()
      (parse-uri "http://java.sun.com/"))
    (shell-script ()
      "sudo apt-get install sun-java6-jdk"))
  (chapter (:title "Install Graphviz")
    (paragraph ()
      (parse-uri "http://www.graphviz.org/"))
    (shell-script ()
      "sudo apt-get install libgraphviz4"))
  (chapter (:title "Install Darcs Repositories")
    (make-instance 'shell-script
                   :contents (list* "sudo apt-get install cvs subversion git git-core darcs"
                                    (collect-get-repository-shell-script-commands))))
  (chapter (:title "Configure the Environment")
    (shell-script ()
      "cd ~"
      "wget http://dwim.hu/install/.sbclrc"))
  (chapter (:title "Configure the Server")
    (shell-script ()
      "cd ~/workspace/hu.dwim.home/www"
      "sh ../etc/createlinks.sh"))
  (chapter (:title "Build Production Server")
    (shell-script ()
      "~/workspace/hu.dwim.environment/bin/build-image hu.dwim.home"))
  (chapter (:title "Startup Server")
    (paragraph ()
      "The Server startup should not take more than a few seconds.")
    (shell-script ()
      "~/workspace/hu.dwim.home/server"))
  (chapter (:title "Shutdown Server")
    (paragraph ()
      "Press Control-C and wait until the Server shutdown is completed. The process should not take more than a few seconds."))
  (chapter (:title "Browse Server")
    (paragraph ()
      (parse-uri "http://localhost.localdomain:8080/")))
  (chapter (:title "Run Test Suite")
    (paragraph ()
      (parse-uri "http://localhost.localdomain/test/")))
  (chapter (:title "Install Emacs")
    (paragraph ()
      (parse-uri "http://www.gnu.org/software/emacs/"))
    (shell-script ()
      "sudo apt-get install emacs-snapshot"
      "cd ~"
      "wget http://dwim.hu/install/.emacs"))
  (chapter (:title "Install Slime")
    (paragraph ()
      (parse-uri "http://common-lisp.net/project/slime/"))
    (shell-script ()
      "cd workspace"
      "cvs -z3 -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot checkout slime"))
  (chapter (:title "Connect Server with Slime")
    (paragraph ()
      (parse-uri "http://common-lisp.net/project/slime/"))
    (shell-script ()
      "emacs")))
