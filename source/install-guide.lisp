;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

(def function collect-project-installing-shell-commands (live?)
  (with-simple-restart (skip-all "Return NIL from COLLECT-PROJECT-INSTALLING-SHELL-COMMANDS")
    (sort (iter (with darcs-get = "darcs get ") ;; TODO: add --lazy after it is proved to be worth
                (with git-clone = "git clone ") ;; TODO: add --depth 1 after it is proved to be worth
                (for pathname :in (collect-live-project-pathnames))
                (for pathname-string = (namestring pathname))
                (for name = (last-elt (pathname-directory pathname)))
                (format *debug-io* "Getting project information for ~A (~A) ~%"
                        pathname-string (if live?
                                            "live"
                                            "head"))
                (with-simple-restart (skip "Skip project ~A" pathname)
                  (collect (cond ((search "hu.dwim" name)
                                  (string+ darcs-get "http://dwim.hu/"
                                           (if live?
                                               "live/"
                                               "darcs/")
                                           name))
                                 ((probe-file (merge-pathnames "_darcs" pathname))
                                  (if live?
                                      (string+ darcs-get "http://dwim.hu/live/" name)
                                      (bind ((darcs-info (with-output-to-string (output)
                                                           (sb-ext:run-program "/usr/bin/darcs"
                                                                               `("show" "repo" "--repodir" ,pathname-string)
                                                                               :output output)))
                                             ((:values nil groups) (cl-ppcre:scan-to-strings ".*Default Remote: (.*?)/?\\n.*" darcs-info))
                                             (project (unless (zerop (length groups))
                                                        (first-elt groups))))
                                        (if (search "/var/opt/darcs" project)
                                            (string+ darcs-get "http://dwim.hu/darcs/" name)
                                            (string+ darcs-get project)))))
                                 ((probe-file (merge-pathnames ".git" pathname))
                                  (bind ((git-info (with-output-to-string (output)
                                                     (sb-ext:run-program "/usr/bin/git"
                                                                         `("--git-dir" ,(string+ pathname-string "/.git") "remote" "show" "origin" "-n")
                                                                         :output output)))
                                         ((:values nil groups) (cl-ppcre:scan-to-strings ".*URL: (.*?)/?\\n.*" git-info))
                                         (project (first-elt groups)))
                                    (string+ git-clone project
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
                                  (string+ "# TODO: Don't know how to install project " name))))))
          #'string<)))

(def book install-guide (:title "Install Guide" :authors '("Levente Mészáros"))
  (chapter (:title "Introduction")
    (paragraph ()
      "This guide describes how to install and configure the web service running at " (parse-uri "http://dwim.hu/") ". It is meant to be as complete and independent as possible, including the installation of all the required libraries into a standalone directory. The shell script below will use version control tools (for the projects using one) to replicate the exact same revision used by the live site. Therefore it's a good idea to use the version of the dependencies this script installs, and later to update using the default remote repository locations. This way the updates of the dependencies arrive to your local checkout in chunks that integrate well, or at least well enough to run our website.")
    (paragraph ()
      "You can see the source of this install guide at " (parse-uri "http://dwim.hu/file/hu.dwim.home/source/install-guide.lisp")))
  (chapter (:title "Platform")
    (paragraph ()
      "This guide is tested using VirtualBox to ensure that it sets up everything properly. First the operating system is installed, then the shell scripts in the upcoming chapters are run."
      ;; TODO install this url on the VirtualBox above. same applies for many other occasions below...
      (parse-uri "http://www.virtualbox.org/")))
  (chapter (:title "Install Ubuntu")
    (paragraph ()
      "Our server runs on Ubuntu Linux (Server Edition, x86-64). Setting up the server on other Debian based Linux distributions is likely to work without any issues. Other operating systems, such as Windows or Mac OS X, are not tested and will most probably not work out of the box. Patches are welcome!"
      (parse-uri "http://www.ubuntu.com/")))
  (chapter (:title "Set up a directory for the installed components")
    (paragraph ()
      "All the required source code will be in the workspace directory. TODO: set up an input field to let the reader configure the install path of the scripts, defaulting to /opt/hu.dwim.home/workspace and suggesting ~/workspace as an alternative.")
    (shell-script ()
      "mkdir ~/workspace"))
  (chapter (:title "Install SBCL")
    (paragraph ()
      "The Server runs under Steel Bank Common Lisp (x86-64) that is a free Common Lisp implementation for a number of platforms and hardware architectures."
      (parse-uri "http://www.sbcl.org/"))
    (paragraph ()
      "One way to install SBCL is to download the exact same version that dwim.hu is using. Our git repository usually has a few extra patches, but they are usually not crucial to have.")
    (shell-script ()
      "cd ~/workspace"
      "sudo apt-get install git git-core"
      "git clone http://dwim.hu/git/sbcl"
      "cd ~/workspace/sbcl"
      "git checkout hu.dwim")
    (paragraph ()
      "Another way is to download the current SBCL head source tree to get the latest features and bugs.")
    (shell-script ()
      "sudo apt-get install cvs"
      "cd ~/workspace"
      "cvs -z3 -d :pserver:anonymous@sbcl.cvs.sourceforge.net:/cvsroot/sbcl checkout -P sbcl")
    (paragraph ()
      "Bootstrap SBCL using CLISP.")
    (shell-script ()
      "sudo apt-get install clisp"
      "cd ~/workspace/sbcl"
      "wget http://dwim.hu/install/customize-target-features.lisp"
      "sh ~/workspace/sbcl/make.sh \"clisp -ansi -on-error abort\""
      "sudo sh ~/workspace/sbcl/install.sh")
    (paragraph ()
      "SBCL can also be installed from a binary distribution, see its website for details."))
  (chapter (:title "Install PostgreSQL")
    (paragraph ()
      "The Server uses the " #+nil(find-project :hu.dwim.perec) " persistent Common Lisp Object System library to store persistent data in a relational database. The default backend is the well known PostgreSQL open source relational database server."
      (parse-uri "http://www.postgresql.org/"))
    (shell-script ()
      "sudo apt-get install postgresql"))
  (chapter (:title "Configure PostgreSQL")
    (paragraph ()
      "The Server needs a database and a database user in PostgreSQL to store its persistent data."
      (parse-uri "http://www.postgresql.org/"))
    (shell-script ()
      "sudo -u postgres createdb hu.dwim.home"
      "sudo -u postgres createuser --pwprompt --no-superuser --no-createdb --no-createrole hu.dwim.home")
    (paragraph ()
      "When prompted for the password type in 'engedjbe', which is the default password for hu.dwim.home."))
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
  (chapter (:title "Install gitweb")
    (paragraph ()
      "This is needed for gitweb")
    (shell-script ()
      "sudo apt-get install curl libcurl4-gnutls-dev"
      "make GIT_BINDIR=\"/usr/bin\" GITWEB_PROJECTROOT=\"~/workspace\" GITWEB_PROJECT_MAXDEPTH=2 gitweb/gitweb.cgi"))
  (chapter (:title "Install darcsweb")
    (paragraph ()
      "This is needed for gitweb")
    (shell-script ()
      "sudo apt-get install curl libcurl4-gnutls-dev"))
  (chapter (:title "Install Graphviz")
    (paragraph ()
      (parse-uri "http://www.graphviz.org/"))
    (shell-script ()
      "sudo apt-get install libgraphviz4"))
  (chapter (:title "Install Darcs Repositories")
    (paragraph ()
      "The Server source code is located in multiple software components. Unfortunately they are using different version control systems. So to be able to install these repositories you should first install the corresponding version control systems.")
    (shell-script ()
      "sudo apt-get install cvs darcs git git-core subversion")
    (paragraph ()
      "The install guide provides two very different ways to install the repositories. The suggested way is to install the exact same copy that is currently running at " (parse-uri "http://dwim.hu/")". This allows you to have a stable version of all required repositories. The more advanced but also more fragile way is to install the HEAD revisions of all required repositories. This allows you to have the newest fetaures and the newest bugs as well.")
    (chapter (:title "Live")
      "Install the live revisions of the required dependencies. Usually the live system lags behind the latest revisions by a week or two, but in return it integrates well enough to run this site."
      (make-instance 'shell-script :contents (list* "cd ~/workspace"
                                                    (collect-project-installing-shell-commands #t))))
    (chapter (:title "Head")
      "Install the HEAD revisions of the required dependencies. WARNING: This is only advised if you are prepared for random incompatibilities between the head revisions of the ninty-some libraries that are used for this project! Otherwise check out the live repositories (see above)."
      (make-instance 'shell-script :contents (list* "cd ~/workspace"
                                                    (collect-project-installing-shell-commands #f)))))
  (chapter (:title "Configure www/ directory served at the url static/")
    (shell-script ()
      ;; TODO: this has to come after building dojo, but I think this build has to be rethought
      ;; TODO this should not be needed...
      "sh ~/workspace/hu.dwim.home/etc/create-www-links.sh"))
  (chapter (:title "Configure the Server as a unix service to run from /opt/hu.dwim.home/ (optional)")
    ;; TODO workspace path should be coming from a variable
    (chapter (:title "Add a user that will be used to run the server process")
      (shell-script ()
        "sudo adduser --disabled-login --disabled-password --no-create-home dwim"))
    (chapter (:title "Set up logging")
      (shell-script ()
        "sudo mkdir --parents /var/log/hu.dwim.home/archive /var/run/hu.dwim.home"
        "sudo chown -R dwim:admin /var/log/hu.dwim.home /var/run/hu.dwim.home"
        "sudo chmod ug=rwxs,o= /var/log/hu.dwim.home /var/log/hu.dwim.home/archive /var/run/hu.dwim.home"
        "sudo ln -s /opt/hu.dwim.home/workspace/hu.dwim.home/etc/logrotate.conf /etc/logrotate.d/hu.dwim.home.conf"))
    (chapter (:title "Set up rc.d scripts to automatically start the server")
      (shell-script ()
        "sudo ln -sf /opt/hu.dwim.home/workspace/hu.dwim.home/etc/rc.d-script /etc/init.d/hu.dwim.home"
        "sudo chgrp root /opt/hu.dwim.home/workspace/hu.dwim.home/etc/rc.d-script"
        "sudo chmod u=rwx,g=rwx,o=r /opt/hu.dwim.home/workspace/hu.dwim.home/etc/rc.d-script /opt/hu.dwim.home/workspace/hu.dwim.home/bin/"
        "sudo chmod u+x,g+x,o-x /opt/hu.dwim.home/workspace/hu.dwim.home/bin/*.sh"
        "sudo update-rc.d hu.dwim.home defaults")))
  (chapter (:title "Build the server executable")
    (shell-script ()
      "sudo apt-get install libz-dev"
      "sh ~/workspace/cl-l10n/bin/update-cldr.sh"
      "sh ~/workspace/hu.dwim.wui/etc/build-dojo.sh --dojo ~/workspace/dojo/ --dojo-release-dir ~/workspace/hu.dwim.home/www/ --profile ~/workspace/hu.dwim.wui/etc/wui.profile.js --locales \"en-us,hu\""
      "sh ~/workspace/hu.dwim.build/bin/build.sh --load-swank --production-build --overwrite-output-file --executable-output --toplevel-function hu.dwim.home::executable-toplevel --output-filename ~/hu.dwim.home hu.dwim.home.all"))
  (chapter (:title "Startup Server")
    (paragraph ()
      "The Server startup should not take more than a few seconds.")
    (shell-script ()
      "~/hu.dwim.home"))
  (chapter (:title "Shutdown Server")
    (paragraph ()
      "Press 'Control-C' when the server window is active and wait until the Server shutdown is completed. The process should not take more than a few seconds."))
  (chapter (:title "Browse Server")
    (paragraph ()
      (parse-uri "http://localhost.localdomain:8080/")))
  (chapter (:title "Run Test Suite")
    (paragraph ()
      "TODO"))
  (chapter (:title "Browse Test Suite")
    (paragraph ()
      "TODO"))
  (chapter (:title "Install Emacs")
    (paragraph ()
      (parse-uri "http://www.gnu.org/software/emacs/"))
    (shell-script ()
      "sudo apt-get install emacs-snapshot"
      "cd ~"
      "wget http://dwim.hu/install/.emacs"))
  (chapter (:title "Install Slime")
    (paragraph ()
      "The recommended way is to install the SLIME branch hu.dwim.slime.")
    (paragraph ()
      "The official SLIME can be found under" (parse-uri "http://common-lisp.net/project/slime/"))
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
