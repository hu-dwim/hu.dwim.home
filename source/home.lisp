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
                                                         (list (string+ "static/" path)
                                                               (system-relative-pathname :hu.dwim.home (string+ "www/" path))))
                                                       (dojo-relative-path (path)
                                                         (string+ *dojo-directory-name* path)))
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
                             (make-documentation-menu-item)
                             (make-project-menu-item)
                             (make-source-menu-item)
                             (make-debug-menu-item)))
      (content/widget ()
        content))))

(def layered-method make-frame-component-with-content ((application home-application) session frame content)
  (make-frame-component content))

(def function make-project-menu-item ()
  (make-menu-item "Project"
                  (mapcar [bind ((project (make-instance 'project :path !1)))
                            (menu-item/widget ()
                                (replace-target-place/widget ()
                                    (hu.dwim.wui::name-of project)
                                  (make-value-inspector project :initial-alternative-type 't/detail/presentation)))]
                          (collect-live-project-pathnames))))

(def function make-documentation-menu-item ()
  (menu-item/widget ()
      "Documentation"
    (menu-item/widget ()
        (replace-target-place/widget ()
            "Install Guide"
          (make-value-inspector (find-book 'hu.dwim.home/install-guide) :initial-alternative-type 'book/text/inspector)))))

(def function make-source-menu-item ()
  (menu-item/widget ()
      "Source"
    (menu-item/widget ()
        (replace-target-place/widget ()
            "Class Browser"
          (make-value-inspector (find-class 'hu.dwim.perec::persistent-class) :initial-alternative-type 't/lisp-form/inspector)))
    (menu-item/widget ()
        (replace-target-place/widget ()
            "Function Browser"
          (make-value-inspector (fdefinition 'make-instance) :initial-alternative-type 't/lisp-form/inspector)))))

;;;;;;
;;; Entry points

(def file-serving-entry-point *home-application* "/static/" (system-relative-pathname :hu.dwim.home "www/"))

(def file-serving-entry-point *home-application* "/install/" (system-relative-pathname :hu.dwim.home "www/install/"))

(def file-serving-entry-point *home-application* "/darcs/" #P"/opt/darcs/")

(def file-serving-entry-point *home-application* "/live/" common-lisp-user::*workspace-directory*)

(def file-serving-entry-point *home-application* "/darcsweb/" (merge-pathnames "darcsweb/" common-lisp-user::*workspace-directory*))

(def js-file-serving-entry-point *home-application* "/wui/js/" (system-relative-pathname :hu.dwim.wui "source/js/"))

;; TODO: this repeated code is quite boring
(def entry-point (*home-application* :path-prefix "project/" :ensure-session #t :ensure-frame #t) ()
  (if (root-component-of *frame*)
      (make-root-component-rendering-response *frame*)
      (progn
        (setf (root-component-of *frame*)
              (make-frame-component (make-value-inspector (make-instance 'project :path (asdf::system-source-directory *entry-point-relative-path*))
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
        (setf (root-component-of *frame*) (make-frame-component (image/widget :id "dwim-logo" :location (make-uri-for-current-application "static/wui/image/about/dwim-logo.png"))))
        (make-redirect-response-for-current-application))))

(def entry-point (*home-application* :path "cgi-bin/darcsweb.cgi" :with-session-logic #f) ()
  (make-raw-functional-response ()
    (handle-cgi-request (merge-pathnames "darcsweb/darcsweb.cgi" common-lisp-user::*workspace-directory*))))

;;;;;;
;;; Server

(def constant +default-home-server-port+ 8080)

(def (special-variable e) *home-server* (make-instance 'broker-based-server
                                                       :host +any-host+
                                                       :port +default-home-server-port+
                                                       :brokers (list *home-application*)))

;;;;;;
;;; Production

(def special-variable *http-port-command-line-option*
  '("http-port"
    :type integer
    :initial-value #.+default-home-server-port+
    :documentation "The HTTP server port where it will listening"))

(def function process-http-port-command-line-argument (command-line-arguments)
  (when-bind http-port (getf command-line-arguments :http-port)
    (setf (hu.dwim.wui::port-of (find +default-home-server-port+ (hu.dwim.wui::listen-entries-of *home-server*) :key #'hu.dwim.wui::port-of))
          http-port)))

(def function production-image-toplevel ()
  (bind ((command-line-options (sort (append (list *help-command-line-option*)
                                             (list *http-port-command-line-option*)
                                             *database-command-line-options*
                                             *generic-command-line-options*)
                                     #'string< :key #'first))
         (command-line-arguments (command-line-arguments:process-command-line-options command-line-options
                                                                                      (command-line-arguments:get-command-line-arguments))))
    (process-help-command-line-argument command-line-options command-line-arguments)
    (process-http-port-command-line-argument command-line-arguments)
    (hu.dwim.meta-model::production-image-toplevel command-line-arguments :hu.dwim.home *home-server* *home-application*)))

;;;;;;
;;; TODO: move

(def function collect-live-project-pathnames ()
  (iter (for pathname :in (directory (merge-pathnames common-lisp-user::*workspace-directory* "/*.*")))
        (unless (or (pathname-name pathname)
                    (char= #\. (first-elt (last-elt (pathname-directory pathname))))
                    (string= "sbcl" (last-elt (pathname-directory pathname))))
          (collect pathname))))

(def function collect-install-project-shell-script-commands (live?)
  (sort (iter (for pathname :in (collect-live-project-pathnames))
              (for pathname-string = (namestring pathname))
              (for name = (last-elt (pathname-directory pathname)))
              (format *debug-io* "Getting project information for ~A (~A) ~%"
                      pathname-string (if live?
                                          "live"
                                          "head"))
              (collect (cond ((or (string= "ironclad" name)
                                  (string= "net-telent-date" name)
                                  (string= "parse-number" name)
                                  (string= "split-sequence" name))
                              (if live?
                                  (string+ "wget -np -r -nH --cut-dirs=1 http://dwim.hu/live/" name)
                                  (format nil "sbcl --eval \"(progn (require :asdf-install) (asdf-install:install :~A) (quit))\"" name)))
                             ((search "hu.dwim" name)
                              (string+ "darcs get http://dwim.hu/"
                                       (if live?
                                           "live/"
                                           "darcs/")
                                       name))
                             ((probe-file (merge-pathnames "_darcs" pathname))
                              (if live?
                                  (string+ "darcs get http://dwim.hu/live/" name)
                                  (bind ((darcs-info (with-output-to-string (output)
                                                       (sb-ext:run-program "/usr/bin/darcs"
                                                                           `("show" "repo" "--repodir" ,pathname-string)
                                                                           :output output)))
                                         ((:values nil groups) (cl-ppcre:scan-to-strings ".*Default Remote: (.*?)/?\\n.*" darcs-info))
                                         (project (first-elt groups)))
                                    (if (search "/var/opt/darcs" project)
                                        (string+ "darcs get http://dwim.hu/darcs/" name)
                                        (string+ "darcs get " project)))))
                             ((probe-file (merge-pathnames ".git" pathname))
                              (bind ((git-info (with-output-to-string (output)
                                                 (sb-ext:run-program "/usr/bin/git"
                                                                     `("--git-dir" ,(string+ pathname-string "/.git") "remote" "show" "origin" "-n")
                                                                     :output output)))
                                     ((:values nil groups) (cl-ppcre:scan-to-strings ".*URL: (.*?)/?\\n.*" git-info))
                                     (project (first-elt groups)))
                                (string+ "git clone " project
                                         (when live?
                                           (bind ((git-info (with-output-to-string (output)
                                                              (sb-ext:run-program "/usr/bin/git"
                                                                                  `("--git-dir" ,(string+ pathname-string "/.git") "rev-list" "--max-count=1" "HEAD")
                                                                                  :output output)))
                                                  (hash (string-trim-whitespace git-info)))
                                             (string+ " ; git --git-dir " ;
                                                      name
                                                      "/.git checkout -q " hash))))))
                             ((probe-file (merge-pathnames ".svn" pathname))
                              (bind ((svn-info (with-output-to-string (output)
                                                 (sb-ext:run-program "/usr/bin/svn"
                                                                     `("info" ,pathname-string)
                                                                     :output output)))
                                     ((:values nil groups) (cl-ppcre:scan-to-strings ".*URL: (.*?)/?\\n.*" svn-info))
                                     (project (first-elt groups))
                                     ((:values nil groups) (cl-ppcre:scan-to-strings ".*Revision: (.*?)\\n.*" svn-info))
                                     (revision (first-elt groups)))
                                (string+ "svn checkout " project
                                         (when live?
                                           (string+ " -r " revision))
                                         " " name)))
                             ((probe-file (merge-pathnames "CVS" pathname))
                              (bind ((root (string-trim-whitespace (read-file-into-string (merge-pathnames "CVS/Root" pathname))))
                                     (project (string-trim-whitespace (read-file-into-string (merge-pathnames "CVS/Repository" pathname)))))
                                (string+ "cvs -z3 -d " root " checkout -d " name " " project)))
                             (t
                              (warn "Don't know how to install project ~A" name)
                              (string+ "# TODO: Don't know how to install project " name)))))
        #'string<))

(def book hu.dwim.home/install-guide (:title "Install Guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "The Install Guide describes how to install and configure the same web service that is running at "
      (parse-uri "http://dwim.hu/") " on your local computer. The Install Guide is written to be as complete as possible in terms of listing all needed
software components. While this is helpful for newcomers, it also means that some components may be already present in your system. You can safely ignore installing
those parts that are you have already installed."))
  (chapter (:title "Prerequisits")
    (paragraph ()
      "The Install Guide is tested under Virtual Box to ensure all dependencies are properly listed. First the operating system is installed and then all 
shell scripts are executed from the following chapters."
      (parse-uri "http://www.virtualbox.org/")))
  (chapter (:title "Install Ubuntu")
    (paragraph ()
      "The Server runs under the Ubuntu Linux Server Edition (x86-64) operating system. Installing the Server under other versions of Ubuntu Linux (such as the
Desktop Edition) should not be a problem. Installing the Server under other Linux distributions may be different and may need more hand tuning than what is 
described here. Other operating systems such as Windows, Mac OS X, etc. are not tested and most likely will not work out of the box."
      (parse-uri "http://www.ubuntu.com/")))
  (chapter (:title "Create the Workspace")
    (paragraph ()
      "The Workspace directory will contain all version controlled source code. The directory is assumed to be created in your home directory.")
    (shell-script ()
      "mkdir ~/workspace"))
  (chapter (:title "Install SBCL")
    (paragraph ()
      "The Server runs under Steel Bank Common Lisp (x86-64) that is a free Common Lisp implementation for a number of platforms and hardware architectures."
      (parse-uri "http://www.sbcl.org/"))
    (shell-script ()
      "sudo apt-get install cvs clisp"
      "cd ~/workspace"
      "cvs -z3 -d :pserver:anonymous@sbcl.cvs.sourceforge.net:/cvsroot/sbcl checkout -P sbcl"
      "cd sbcl"
      "wget http://dwim.hu/install/customize-target-features.lisp"
      "~/workspace/sbcl/make.sh \"clisp -ansi -on-error abort\""
      "sudo sh ~/workspace/sbcl/install.sh"))
  (chapter (:title "Install PostgreSQL")
    (paragraph ()
      "The Server uses the " (find-project :hu.dwim.perec) " persistent Common Lisp Object System library to store persistent data in a relational database.
The default backend is the well known PostgreSQL open source relational database server."
      (parse-uri "http://www.postgresql.org/"))
    (shell-script ()
      "sudo apt-get install postgresql"))
  (chapter (:title "Configure PostgreSQL")
    (paragraph ()
      "The Server needs a database and a database user in PostgreSQL to store its persistent data."
      (parse-uri "http://www.postgresql.org/"))
    (shell-script ()
      "sudo su - postgres"
      "createdb hu.dwim.home"
      "createuser -P hu.dwim.home --no-superuser --no-createdb --no-createrole"
      "exit")
    "When prompted for the password type in 'engedjbe'")
  (chapter (:title "Install Sqlite")
    (paragraph ()
      "This installation step is optional. The Sqlite relational database should be installed only if you want to run the corresponding test suites."
      (parse-uri "http://www.sqlite.org/"))
    (shell-script ()
      "sudo apt-get install sqlite3"
      "sudo ln -s /usr/lib/libsqlite3.so.0 /usr/lib/libsqlite3.so"))
  (chapter (:title "Install Oracle")
    (paragraph ()
      "This installation step is optional. The Oracle relational database should be installed only if you want to run the corresponding test suites."
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
    (paragraph ()
      "The Server source code is located in multiple software components. Unfortunately they are using different version control systems. So to be able to install
these repositories you should first install the corresponding version control systems.")
    (shell-script ()
      "sudo apt-get install cvs darcs git git-core subversion")
    (paragraph ()
      "The install guide provides two very different ways to install the repositories. The suggested way is to install the exact same copy that is currently running
at " (parse-uri "http://dwim.hu/")". This allows you to have a stable version of all required repositories. The more advanced but also more fragile way is
to install the HEAD revisions of all required repositories. This allows you to have the newest fetaures and the newest bugs as well.")
    (chapter (:title "Live")
      "Install the Live revisions of the required repositories."
      (make-instance 'shell-script :contents (list* "cd ~/workspace"
                                                    (collect-install-project-shell-script-commands #t))))
    (chapter (:title "Head")
      "Install the HEAD revisions of the required repositories."
      (make-instance 'shell-script :contents (list* "cd ~/workspace"
                                                    (collect-install-project-shell-script-commands #f)))))
  (chapter (:title "Configure the Server")
    (shell-script ()
      "sudo mkdir /var/log/hu.dwim.home"
      ;; TODO: this is clearly not what we want
      "sudo chmod o+w /var/log/hu.dwim.home"
      ;; TODO: merge this into create-links
      "cd ~/workspace/hu.dwim.home/www"
      "sh ~/workspace/hu.dwim.home/etc/create-links.sh"))
  (chapter (:title "Build Server")
    (shell-script ()
      "sudo apt-get install libz-dev"
      "sh ~/workspace/cl-l10n/bin/update-cldr.sh"
      "sh ~/workspace/hu.dwim.wui/etc/build-dojo.sh --dojo ~/workspace/dojo/ --dojo-release-dir ~/workspace/hu.dwim.wui/www/ --profile ~/workspace/hu.dwim.wui/etc/wui.profile.js --locales \"en-us,hu\""
      "sh ~/workspace/hu.dwim.environment/bin/build-image hu.dwim.home"))
  (chapter (:title "Startup Server")
    (paragraph ()
      "The Server startup should not take more than a few seconds.")
    (shell-script ()
      "~/workspace/hu.dwim.home/hu.dwim.home"))
  (chapter (:title "Shutdown Server")
    (paragraph ()
      "Press 'Control-C' when the server window is active and wait until the Server shutdown is completed. The process should not take more than a few seconds."))
  (chapter (:title "Browse Server")
    (paragraph ()
      (parse-uri "http://localhost.localdomain:8080/")))
  (chapter (:title "Run Test Suite")
    (paragraph ()
      (parse-uri "http://localhost.localdomain:8080/test/")))
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
      "cd ~/workspace"
      "cvs -z3 -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot checkout slime"))
  (chapter (:title "Configure the Development Environment")
    (paragraph ()
      "TODO")
    (shell-script ()
      "cd ~"
      "wget http://dwim.hu/install/.sbclrc"))
  (chapter (:title "Connect Server with Slime")
    (paragraph ()
      (parse-uri "http://common-lisp.net/project/slime/"))
    (shell-script ()
      "emacs")))
