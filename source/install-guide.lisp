;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

(def function make-uri-to-workspace-location (workspace-relative-path)
  (make-uri :scheme "http" :host "dwim.hu" :path (string+ "/file/" workspace-relative-path)))

(def function collect-project-installing-shell-commands (live?)
  (with-simple-restart (skip-all "Return NIL from COLLECT-PROJECT-INSTALLING-SHELL-COMMANDS")
    (sort (iter (with darcs-get = "darcs get ") ;; TODO: add --lazy after it is proved to be worth
                (with git-clone = "git clone ") ;; TODO: add --depth 1 after it is proved to be worth
                (for pathname :in (collect-live-project-pathnames))
                (for pathname-string = (namestring pathname))
                (for name = (last-elt (pathname-directory pathname)))
                (format *debug-io* "Getting project information for ~A (~A)~%"
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
                                                 (string+ " ; git --git-dir " name "/.git --work-tree " name "/ checkout -q " hash))))))
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

;; TODO make head and live italic
(def book install-guide (:title "Install Guide")
  (chapter (:title "Introduction")
    (paragraph ()
      "This guide describes how to install and configure the web service running at " (hyperlink "http://dwim.hu/") ". It is meant to be as complete and standalone as possible, including the installation of all the required dependencies under a single local directory.")
    (paragraph ()
      "The shell script below will use version control tools (where available) to install the required components. It can replicate the exact same revisions used by the 'live' site (most probably the one you are reading this guide on), or it can clone the bleeding edge versions (called 'head'). You are advised to clone the live repositories though, and also to update only from them (unless you are planning to work on the framework itself). This way the updates to the numerous integrated libraries arrive to your local checkouts in packages that properly work together -- or at least good enough to run this site.")
    (paragraph ()
      "NOTE: the shell script fragments on this page are ready for copy&pasting! Even if the selection is displayed wrong by some browsers, the copied text will be what you expect it to be.")
    (paragraph ()
      "For the curious, " (hyperlink (print-uri-to-string (make-uri-to-workspace-location "hu.dwim.home/source/install-guide.lisp")) "the source code of this guide") " is inspectable online."))
  (chapter (:title "Required operating system")
    (paragraph ()
      "Our site runs on " (hyperlink "http://www.debian.org/" "Debian Linux") ", but setting it up on other Debian based Linux distributions is likely to work without any issues. Other operating systems, such as Windows or Mac OS X, are not tested and will most probably not work out of the box (but it's all " (hyperlink/wikipedia "open source") ", patches are welcome!)."))
  (chapter (:title "Install some tools first")
    (shell-script ()
      "sudo apt-get install cvs darcs git git-core subversion"))
  (chapter (:title "Set up a directory for the installed Lisp libraries")
    (shell-script ()
      "export DWIM_INSTALL_PATH=/opt/hu.dwim.home"
      "export DWIM_WORKSPACE=${DWIM_INSTALL_PATH}/workspace"
      "export DWIM_PROJECT_NAME=\"hu.dwim.home\""
      "export DWIM_DAEMON_USER=\"home-service\""
      "mkdir --parents ${DWIM_WORKSPACE}"))
  (chapter (:title "Install PostgreSQL")
    (paragraph ()
      ;; TODO
      "Our site uses hu.dwim.perec " #+nil(find-project :hu.dwim.perec) " to store persistent data in a relational database. The default and most thoroughly tested backend of perec is the one that uses " (hyperlink "http://postgresql.org/" "PostgreSQL") ", an " (hyperlink/wikipedia "open source") " relational database server.")
    (shell-script ()
      "sudo apt-get install postgresql"))
  (chapter (:title "Configure PostgreSQL")
    (paragraph ()
      "A database and a database user in PostgreSQL is needed to store persistent data."
      (hyperlink "http://www.postgresql.org/"))
    (shell-script ()
      "sudo -u postgres createdb ${DWIM_PROJECT_NAME}"
      "sudo -u postgres createuser --pwprompt --no-superuser --no-createdb --no-createrole ${DWIM_PROJECT_NAME}")
    (paragraph ()
      "When prompted for the password type in 'engedjbe', which is the default password for hu.dwim.home."))
  (chapter (:title "Install Sqlite (optional)")
    (paragraph ()
      "You only need to install " (hyperlink "http://www.oracle.com/" "SQLite") " if you plan to use the correspondig backend of hu.dwim.rdbms (e.g. running its test suite).")
    (shell-script ()
      "sudo apt-get install sqlite3"))
  (chapter (:title "Install Oracle (optional)")
    (paragraph ()
      "You only need to install " (hyperlink "http://www.oracle.com/" "Oracle") " if you plan to use the correspondig backend of hu.dwim.rdbms (e.g. running its test suite)."))
  (chapter (:title "Add a user that will be used to run the server process (optional)")
    (shell-script ()
      "sudo adduser --disabled-login --disabled-password --no-create-home ${DWIM_DAEMON_USER}"
      "sudo adduser ${DWIM_DAEMON_USER} darcs"
      "sudo adduser ${DWIM_DAEMON_USER} git"))
  (chapter (:title "Install dependencies from source code repositories")
    (chapter (:title "Live repositories (recommended)")
      "The following script installs the live revisions of the dependencies (the ones that were used to compile and run dwim.hu). Usually the live repositories are lagging behind head by a week or two, but in return they are more stable."
      (make-instance 'shell-script :contents (list* "cd ${DWIM_WORKSPACE}" (collect-project-installing-shell-commands #t))))
    (chapter (:title "Head repositories (not recommended)")
      ;; TODO bold
      "Alternatively you can install the HEAD revisions of the dependencies, but this is only advised if you are prepared for random incompatibilities between the head revisions of the ninty-some libraries that are used for this project! Otherwise check out the LIVE repositories (see above). "
      (make-instance 'shell-script :contents (list* "cd ${DWIM_WORKSPACE}" (collect-project-installing-shell-commands #f)))))
  (chapter (:title "Compile and install libfixposix")
    (paragraph ()
      (hyperlink "http://gitorious.org/libfixposix" "libfixposix") " is a dependency of " (hyperlink "http://common-lisp.net/project/iolib/" "IOLib") ".")
    (shell-script ()
      "sudo apt-get install build-essential automake autoconf libtool"
      "cd ${DWIM_WORKSPACE}/libfixposix"
      "autoreconf -i"
      "mkdir build"
      "cd build"
      "../configure"
      "make"
      "sudo make install"
      "sudo ldconfig"))
  (chapter (:title "Install Graphviz")
    (paragraph ()
      (hyperlink "http://www.graphviz.org/"))
    (shell-script ()
      "sudo apt-get install libgraphviz-dev"))
  (chapter (:title "Build SBCL")
    (paragraph ()
      "This server runs on " (hyperlink "http://sbcl.org" "Steel Bank Common Lisp (SBCL)") ", an " (hyperlink/wikipedia "open source") " " (hyperlink/wikipedia "Common Lisp") " implementation.")
    (paragraph ()
      ;; TODO bold
      "NOTE: At the time of writing, our git repository contains a few extra but not essential patches. If you followed this guide, then you have those patches in a git branch called 'hu.dwim', so you will not be running a vanilla SBCL.")
    (paragraph ()
      "To build SBCL using CLISP:")
    (shell-script ()
      "sudo apt-get install clisp"
      "cd ${DWIM_WORKSPACE}/sbcl"
      "sh ${DWIM_WORKSPACE}/sbcl/make.sh \"clisp -ansi -on-error abort\"")
    (paragraph ()
      "Building with CLISP may not always work, but you can also build SBCL with itself. To do so you need to download a precompiled SBCL binary from " (hyperlink "http://www.sbcl.org/platform-table.html") ".")
    (shell-script ()
      "cd ${DWIM_WORKSPACE}/sbcl"
      "sh ${DWIM_WORKSPACE}/sbcl/make.sh /path/to/the-extracted-sbcl/run-sbcl.sh"))
  (chapter (:title "Set up www/ directory which is served at the 'static/' URL")
    (chapter (:title "Build a Dojo Toolkit checkout")
      (paragraph ()
        "Java is required to build the Dojo Toolkit.")
      (shell-script ()
        ;; TODO alternatively: "sudo apt-get install sun-java6-jdk"
        "sudo apt-get install default-jre-headless")
      (paragraph ()
        "The sources of Dojo Toolkit should already be checked out into the workspace directory by one of the VCS commands above.")
      (shell-script ()
        "cd ${DWIM_WORKSPACE}"
        ;; it's not needed because it's above in the automatically generated checkout commands "svn co http://svn.dojotoolkit.org/src/tags/release-1.5/ dojotoolkit-v1.5/"
        ;; "for i in . dojo dojox dijit demos util ; do pushd $i; svn up --ignore-externals --revision {desired dojo svn revision}; popd; done"
        "sh ${DWIM_WORKSPACE}/hu.dwim.web-server/etc/build-dojo.sh --dojo ${DWIM_WORKSPACE}/dojotoolkit-v1.5/ --dojo-release-dir ${DWIM_WORKSPACE}/hu.dwim.web-server/www/libraries/ --profile ${DWIM_WORKSPACE}/hu.dwim.web-server/etc/dojo-build-profile.js --locales \"en-us,hu\""))
    (chapter (:title "Build a Sencha (Ext JS) checkout (optional)")
      (shell-script ()
        "cd ${DWIM_WORKSPACE}"
        "sh ${DWIM_WORKSPACE}/hu.dwim.web-server/etc/build-sencha.sh ${DWIM_WORKSPACE}/ext-core-3.3.x/ --output-dir ${DWIM_WORKSPACE}/hu.dwim.web-server/www/libraries/")))
  (chapter (:title "Build the server executable")
    (paragraph ()
      "If you changed the installation path, then make sure you update hu.dwim.home/bin/env.sh accordingly!")
    (shell-script ()
      "sudo apt-get install libz-dev"
      "sh ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/build.sh"))
  (chapter (:title "Running the server")
    (paragraph ()
      "Starting up the server shouldn't take more than a couple of seconds, most of which is spent with the synchronization of the SQL schema.")
    (shell-script ()
      "/opt/hu.dwim.home/hu.dwim.home --verbose")
    (paragraph ()
      "If everything started normally then the service should be listening on " (hyperlink "http://127.0.0.1:8080/") ". Pressing Control-c in the terminal should initiate a graceful server shutdown which normally takes a few seconds."))
  (chapter (:title "Set up backups (optional)")
    (shell-script ()
      "sudo mkdir --parents /var/backups/${DWIM_PROJECT_NAME}/database"
      "sudo chown -R ${DWIM_DAEMON_USER}:admin /var/backups/${DWIM_PROJECT_NAME}"
      "sudo chmod -R u=rwx,g=rwx,o-rwx /var/backups/${DWIM_PROJECT_NAME}"))
  (chapter (:title "Set up darcsweb (optional)")
    (shell-script ()
      "sudo chmod ug+rx ${DWIM_WORKSPACE}/darcsweb/darcsweb.cgi"))
  (chapter (:title "Set up gitweb (optional)")
    (paragraph ()
      "The following dependencies are needed for gitweb:")
    (shell-script ()
      "sudo apt-get install curl libcurl4-gnutls-dev"
      "make GIT_BINDIR=\"/usr/bin\" GITWEB_PROJECTROOT=${DWIM_WORKSPACE} GITWEB_PROJECT_MAXDEPTH=2 gitweb/gitweb.cgi"
      "sudo chmod ug+rx ${DWIM_WORKSPACE}/gitweb/gitweb.cgi"))
  (chapter (:title "Configure the server as a unix service (optional)")
    (chapter (:title "Set up logging")
      (shell-script ()
        "sudo mkdir --parents /var/log/${DWIM_PROJECT_NAME}/archive /var/run/${DWIM_PROJECT_NAME}"
        "sudo chown -R ${DWIM_DAEMON_USER}:${DWIM_DAEMON_USER} ${DWIM_INSTALL_PATH}"
        "sudo chown -R ${DWIM_DAEMON_USER}:admin /var/log/${DWIM_PROJECT_NAME} /var/run/${DWIM_PROJECT_NAME}"
        "sudo chmod ug=rwxs,o-rwx ${DWIM_INSTALL_PATH} /var/log/${DWIM_PROJECT_NAME} /var/log/${DWIM_PROJECT_NAME}/archive /var/run/${DWIM_PROJECT_NAME}"
        "sudo ln -s ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/logrotate.conf /etc/logrotate.d/${DWIM_PROJECT_NAME}.conf"))
    (chapter (:title "Set up rc.d scripts to automatically start the server")
      (shell-script ()
        "sudo ln -sf ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/rc.d-script /etc/init.d/${DWIM_PROJECT_NAME}"
        "sudo chgrp root ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/rc.d-script"
        "sudo chmod u=rwx,g=rwx,o=r ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/rc.d-script ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/"
        "sudo chmod u+x,g+x,o-x ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/*.sh"
        "sudo update-rc.d ${DWIM_PROJECT_NAME} defaults"))
    (chapter (:title "Set up a daily cron job")
      (shell-script ()
        "chmod +x ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/cron.daily"
        "sudo ln -s ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/cron.daily /etc/cron.daily/00${DWIM_PROJECT_NAME}"))
    (chapter (:title "Increase the maximum amount of separate memory mappings on linux")
      (shell-script ()
        "sudo echo \"vm.max_map_count = 262144\" >/etc/sysctl.d/30-sbcl.conf")))
  #+nil
  (chapter (:title "Run Test Suite")
    (paragraph ()
      "TODO"))
  #+nil
  (chapter (:title "Browse Test Suite")
    (paragraph ()
      "TODO"))
  #+()
  (chapter (:title "Install Slime (optional)")
    (paragraph ()
      "It's recommended to install our branch of SLIME, but the official SLIME should be fine, too.")
    (paragraph ()
      "The official SLIME can be found at " (hyperlink "http://common-lisp.net/project/slime/"))
    (shell-script ()
      "cd ${DWIM_WORKSPACE}"
      "cvs -z3 -d :pserver:anonymous:anonymous@common-lisp.net:/project/slime/cvsroot checkout slime"))
  (chapter (:title "Configure the Development Environment (optional)")
    (paragraph ()
      "Install " (hyperlink "http://www.gnu.org/software/emacs/" "Emacs") ".")
    (shell-script ()
      "sudo apt-get install emacs-snapshot hyperspec"
      "wget http://dwim.hu/install/init.el --no-clobber --output-document=\"~/.emacs.d/init.el\""
      "wget http://dwim.hu/install/.sbclrc --no-clobber --output-document=\"~/.sbclrc\""))
  #+nil
  (chapter (:title "Connect Server with Slime")
    (paragraph ()
      (hyperlink "http://common-lisp.net/project/slime/"))
    (shell-script ()
      "emacs")))
