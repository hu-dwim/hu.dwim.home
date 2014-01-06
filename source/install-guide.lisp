;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

(def function make-uri-to-workspace-location (workspace-relative-path)
  (hu.dwim.uri:make-uri :scheme "http" :host "dwim.hu" :path (string+ "/file/" workspace-relative-path)))

(def function build-repo-clone-command/git (project-pathname &key (preserve-exact-revision #t))
  (check-type project-pathname pathname)
  (assert (fad:directory-pathname-p project-pathname))
  (bind ((project-pathname-as-string (namestring project-pathname)))
    (flet ((run-git (&rest args)
             (with-output-to-string (output)
               (uiop:run-program `("/usr/bin/git" "--git-dir" ,(string+ project-pathname-as-string "/.git") ,@args)
                                 :output output))))
      (bind ((directory-name (last-elt (pathname-directory project-pathname)))
             (branch-name (string-trim-whitespace (run-git "symbolic-ref" "-q" "HEAD")))
             (repo-url (bind ((git-info (run-git "remote" "show" "origin" "-n"))
                              ((:values nil groups) (cl-ppcre:scan-to-strings ".*URL: (.*?)/?\\n.*" git-info)))
                         (first-elt groups)))
             (revision-hash (string-trim-whitespace (run-git "log" "-1" "--format=%H"))))
        (if (string= branch-name "(unnamed branch)")
            (error "The git repo ~S is in a detached HEAD state, cannot build a clone command for it." project-pathname)
            (setf branch-name (subseq branch-name (length "refs/heads/"))))
        ;; TODO: maybe add --depth 1 after it is proven to be worth it
        (values (string+ "git clone --branch " branch-name " " repo-url " " directory-name
                         (when preserve-exact-revision
                           (string+ "; git --work-tree " directory-name " --git-dir " directory-name "/.git reset --hard " revision-hash)))
                (list directory-name repo-url branch-name revision-hash))))))

(def function build-repo-clone-command/svn (project-pathname &key (preserve-exact-revision #t))
  (check-type project-pathname pathname)
  (assert (fad:directory-pathname-p project-pathname))
  (bind ((project-pathname-as-string (namestring project-pathname))
         (directory-name (last-elt (pathname-directory project-pathname)))
         (svn-info (with-output-to-string (output)
                     (uiop:run-program `("/usr/bin/svn" "info" ,project-pathname-as-string)
                                       :output output)))
         ((:values nil groups) (cl-ppcre:scan-to-strings ".*URL: (.*?)/?\\n.*" svn-info))
         (repo-url (first-elt groups))
         ((:values nil groups) (cl-ppcre:scan-to-strings ".*Revision: (.*?)\\n.*" svn-info))
         (revision (first-elt groups)))
    (values (string+ "svn checkout " repo-url " "
                     (when preserve-exact-revision
                       (string+ "-r " revision " "))
                     directory-name)
            (list directory-name repo-url revision))))

(def function collect-project-installing-shell-commands (pathnames live?)
  (with-simple-restart (skip-all "Return NIL from COLLECT-PROJECT-INSTALLING-SHELL-COMMANDS")
    (sort (iter (with darcs-get = "darcs get ") ;; TODO: add --lazy after it is proved to be worth
                (for pathname :in pathnames)
                (for pathname-string = (namestring pathname))
                (for directory-name = (last-elt (pathname-directory pathname)))
                (format *debug-io* "Getting project information for ~A (~A)~%"
                        pathname-string (if live?
                                            "live"
                                            "head"))
                (with-simple-restart (skip "Skip project ~A" pathname)
                  (collect (cond ((search "hu.dwim" directory-name)
                                  (string+ darcs-get "http://dwim.hu/"
                                           (if live?
                                               "live/"
                                               "darcs/")
                                           directory-name))
                                 ((probe-file (merge-pathnames "_darcs" pathname))
                                  ;; TODO maybe use darcs log --context >contextfile and then darcs get --context="contextfile"?
                                  (if live?
                                      (string+ darcs-get "http://dwim.hu/live/" directory-name)
                                      (bind ((darcs-info (with-output-to-string (output)
                                                           (uiop:run-program `("/usr/bin/darcs" "show" "repo" "--repodir" ,pathname-string)
                                                                               :output output)))
                                             ((:values nil groups) (cl-ppcre:scan-to-strings ".*Default Remote: (.*?)/?\\n.*" darcs-info))
                                             (project (unless (zerop (length groups))
                                                        (first-elt groups))))
                                        (if (search "/var/opt/darcs" project)
                                            (string+ darcs-get "http://dwim.hu/darcs/" directory-name)
                                            (string+ darcs-get project)))))
                                 ((probe-file (merge-pathnames ".git" pathname))
                                  (build-repo-clone-command/git pathname :preserve-exact-revision live?))
                                 ((probe-file (merge-pathnames ".svn" pathname))
                                  (build-repo-clone-command/svn pathname :preserve-exact-revision live?))
                                 ((probe-file (merge-pathnames "CVS" pathname))
                                  (bind ((root (string-trim-whitespace (read-file-into-string (merge-pathnames "CVS/Root" pathname))))
                                         (project-cvs-url (string-trim-whitespace (read-file-into-string (merge-pathnames "CVS/Repository" pathname)))))
                                    (string+ "cvs -z3 -d " root " checkout -d " directory-name " " project-cvs-url)))
                                 (t
                                  (warn "Don't know how to install project ~A" directory-name)
                                  (string+ "# TODO: Don't know how to install project " directory-name))))))
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
      "For the curious, " (hyperlink (hu.dwim.uri:print-uri-to-string (make-uri-to-workspace-location "hu.dwim.home/source/install-guide.lisp")) "the source code of this guide") " is inspectable online."))
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
      "sudo adduser ${DWIM_DAEMON_USER} www-data"
      "sudo adduser ${DWIM_DAEMON_USER} darcs"
      "sudo adduser ${DWIM_DAEMON_USER} git"))
  (chapter (:title "Install dependencies from source code repositories")
    (chapter (:title "Live repositories (recommended)")
      "The following script installs the live revisions of the dependencies (the ones that were used to compile and run dwim.hu). Usually the live repositories are lagging behind head by a week or two, but in return they are more stable."
      (make-instance 'shell-script :contents (list* "cd ${DWIM_WORKSPACE}" (collect-project-installing-shell-commands (collect-live-project-pathnames) #t))))
    (chapter (:title "Head repositories (not recommended)")
      ;; TODO bold
      "Alternatively you can install the HEAD revisions of the dependencies, but this is only advised if you are prepared for random incompatibilities between the head revisions of the ninty-some libraries that are used for this project! Otherwise check out the LIVE repositories (see above). "
      (make-instance 'shell-script :contents (list* "cd ${DWIM_WORKSPACE}" (collect-project-installing-shell-commands (collect-live-project-pathnames) #f)))))
  (chapter (:title "Install libfixposix")
    (paragraph ()
      (hyperlink "http://gitorious.org/libfixposix" "libfixposix") " is a dependency of " (hyperlink "http://common-lisp.net/project/iolib/" "IOLib") ". The preferred way to install it on Linux is to add the APT repository maintained by the author as described " (hyperlink "http://comments.gmane.org/gmane.lisp.iolib.devel/422" "in this mail") ".")
    (chapter (:title "Compiling on Linux")
      (paragraph ()
        "Alternatively, you can compile it on Linux, and possibly other unix-like operating systems, as described below.")
      (shell-script ()
        "sudo apt-get install build-essential automake autoconf libtool check"
        "cd ${DWIM_WORKSPACE}/libfixposix"
        "autoreconf -i"
        "mkdir build"
        "cd build"
        "../configure"
        "make"
        "sudo make install"
        "sudo ldconfig")))
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
        "sh ${DWIM_WORKSPACE}/hu.dwim.web-server/etc/build-dojo.sh --dojo ${DWIM_WORKSPACE}/dojotoolkit-v1.6/ --dojo-release-dir ${DWIM_WORKSPACE}/hu.dwim.web-server/www/libraries/ --profile ${DWIM_WORKSPACE}/hu.dwim.web-server/etc/dojo-build-profile.js --locales \"en-us,hu\""))
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
      "sudo apt-get install bzip2 tar"
      "sudo mkdir --parents /var/backups/${DWIM_PROJECT_NAME}/database"
      "sudo chown ${DWIM_DAEMON_USER}:adm /var/backups/${DWIM_PROJECT_NAME}"
      "sudo chmod ug=rwx,o=rx /var/backups/${DWIM_PROJECT_NAME}"
      "sudo chown ${DWIM_DAEMON_USER}:postgres /var/backups/${DWIM_PROJECT_NAME}/database"
      "sudo ln -s /var/backups/${DWIM_PROJECT_NAME} ${DWIM_INSTALL_PATH}/backup"))
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
    (chapter (:title "Ensure proper file permissions")
      (shell-script ()
        "sudo chown -R ${DWIM_DAEMON_USER}:${DWIM_DAEMON_USER} ${DWIM_INSTALL_PATH}"))
    (chapter (:title "Set up logging")
      (shell-script ()
        "sudo mkdir --parents /var/log/${DWIM_PROJECT_NAME}/archive"
        "sudo chown -R ${DWIM_DAEMON_USER}:adm /var/log/${DWIM_PROJECT_NAME}"
        "sudo chmod u=rwx,g=rwxs,o=rx ${DWIM_INSTALL_PATH} /var/log/${DWIM_PROJECT_NAME} /var/log/${DWIM_PROJECT_NAME}/archive"
        "sudo touch /var/log/${DWIM_PROJECT_NAME}/backup.log"
        "sudo chown ${DWIM_DAEMON_USER}:postgres /var/log/${DWIM_PROJECT_NAME}/backup.log"
        ;; NOTE: chown/mod the dir also, because darcs seems to update using: rename, create new file, delete old one
        "sudo chown root:adm ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/ ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/logrotate.conf"
        "sudo chmod u=rw,g=rx,o=rx ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/"
        "sudo chmod u=rw,g=r,o=r ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/logrotate.conf"
        "sudo ln -s ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/logrotate.conf /etc/logrotate.d/${DWIM_PROJECT_NAME}.conf"
        "ln -s /var/log/${DWIM_PROJECT_NAME} ${DWIM_INSTALL_PATH}/log"))
    (chapter (:title "Set up rc.d scripts to automatically start the server")
      (shell-script ()
        "sudo mkdir --parents /var/run/${DWIM_PROJECT_NAME}"
        ;; NOTE security risk, so chown a few things
        "sudo chown root:adm /var/run/${DWIM_PROJECT_NAME} ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/ ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/rc.d-script ${DWIM_WORKSPACE}/hu.dwim.environment/etc/service-scripts/ ${DWIM_WORKSPACE}/hu.dwim.environment/etc/service-scripts/rc.d-script ${DWIM_WORKSPACE}/hu.dwim.environment/etc/service-scripts/server-loop.sh ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/environment.sh ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/login-environment.sh ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/server-loop.sh"
        "sudo chown root:postgres ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/postgresql-wal-archive-command.sh ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/create-database-backup-snapshot.sh"
        "sudo chmod ug=rwxs,o-rwx /var/run/${DWIM_PROJECT_NAME}"
        "sudo chmod u=rwx,g=rx,o=rx ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/ ${DWIM_WORKSPACE}/hu.dwim.environment/etc/service-scripts/ ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/"
        "sudo chmod u=rwx,g=rx,o=rx ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/rc.d-script ${DWIM_WORKSPACE}/hu.dwim.environment/etc/service-scripts/rc.d-script ${DWIM_WORKSPACE}/hu.dwim.environment/etc/service-scripts/server-loop.sh ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/environment.sh ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/login-environment.sh ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/postgresql-wal-archive-command.sh ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/create-database-backup-snapshot.sh ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/server-loop.sh"
        "sudo chmod u+x,g+x,o+x ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/bin/*.sh"
        "sudo ln -s ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/rc.d-script /etc/init.d/${DWIM_PROJECT_NAME}"
        "sudo update-rc.d ${DWIM_PROJECT_NAME} defaults"))
    (chapter (:title "Set up a daily cron job")
      (paragraph ()
        "WARNING: the linux run-parts script (usually used to run cron.daily) ignores files that have a dot in their names!")
      (shell-script ()
        ;; NOTE security risk, so chown a few things
        "sudo chmod u=rwx,g=rx,o=rx ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/ ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/cron.*"
        "sudo chown root:adm ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/cron.*"
        "sudo ln -s ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/cron.daily /etc/cron.daily/00-${DWIM_PROJECT_NAME}"
        "sudo ln -s ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/cron.hourly /etc/cron.hourly/00-${DWIM_PROJECT_NAME}"
        "sudo ln -s ${DWIM_WORKSPACE}/${DWIM_PROJECT_NAME}/etc/cron.weekly /etc/cron.weekly/00-${DWIM_PROJECT_NAME}"))
    (chapter (:title "Increase the maximum amount of separate memory mappings on linux")
      (shell-script ()
        "sudo bash -c 'echo \"vm.max_map_count = 262144\" >/etc/sysctl.d/30-sbcl.conf'")))
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
