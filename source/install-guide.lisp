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
                                             (project (first-elt groups)))
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
      "The Install Guide describes how to install and configure the same web service that is running at "
      (parse-uri "http://dwim.hu/") " on your local computer. The Install Guide is written to be as complete as possible in terms of listing all needed
software components. While this is helpful for newcomers, it also means that some components may be already present in your system. You can safely ignore installing
those parts that you have already installed.")
    (paragraph ()
      "You can see the source of this install guide at " (parse-uri "http://dwim.hu/file/hu.dwim.home/source/install-guide.lisp")))
  (chapter (:title "Platform")
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
    (paragraph ()
      "One way to install SBCL is to download the exact same version that dwim.hu is using. Our git repository usually has a few extra patches, but they are usually not crucial to have.")
    (shell-script ()
      "cd ~/workspace"
      "sudo apt-get install git git-core"
      "git clone http://dwim.hu/sbcl"
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
      "The Server uses the " #+nil(find-project :hu.dwim.perec) " persistent Common Lisp Object System library to store persistent data in a relational database.
The default backend is the well known PostgreSQL open source relational database server."
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
                                                    (collect-project-installing-shell-commands #t))))
    (chapter (:title "Head")
      "Install the HEAD revisions of the required repositories."
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
      "sh ~/workspace/hu.dwim.wui/etc/build-dojo.sh --dojo ~/workspace/dojo/ --dojo-release-dir ~/workspace/hu.dwim.home/www/ --profile ~/workspace/hu.dwim.home/etc/hu.dwim.home.profile.js --locales \"en-us,hu\""
      "sh ~/workspace/hu.dwim.build/bin/build.sh --load-swank --production-build --overwrite-output-file --executable-output --output-filename ~/hu.dwim.home hu.dwim.home"))
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
