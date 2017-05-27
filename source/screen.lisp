;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Frame

(def method make-frame-root-component-using-application ((application home-application) session frame content)
  (bind (((:values home-menu initial-content) (make-home-menu))
         (menu-bar (menu-bar/widget ()
                     home-menu
                     (make-documentation-menu)
                     (make-demo-menu)
                     (make-project-menu)
                     (make-repository-menu)
                     (make-source-menu)
                     (make-test-menu)
                     ;; we don't want random visitors to turn on debugging... (make-debug-menu)
                     )))
    (frame/widget (:title "dwim.hu"
                   :stylesheet-uris (make-stylesheet-uris :hu.dwim.home "static/home/" "css/home.css")
                   :page-icon-uri (make-page-icon-uri :hu.dwim.home "static/home/" "favicon.ico"))
      (contents/widget ()
        (top/widget (:menu-bar menu-bar)
          (or content initial-content))
        (google-analytics/widget :analytics-account "UA-15042869-1")))))

;;;;;;
;;; Home menu

(def function make-home-menu ()
  (bind (((:values home-menu-item home-content) (make-home-menu-item)))
    (values (menu-item/widget ()
                "Home"
              home-menu-item
              #+nil
              (make-news-menu-item)
              #+nil
              (make-blog-menu-item)
              #+nil
              (make-forum-menu-item)
              #+nil
              (make-contact-menu-item)
              (make-licence-menu-item)
              (make-help-menu-item))
            home-content)))

(def function make-home-menu-item ()
  (bind ((content (vertical-list/layout ()
                    (image/widget :id "dwim-logo" :location (make-uri-for-current-application "static/hdp/image/about/dwim-logo.png"))
                    (make-value-inspector
                     (book (:title "dwim.hu")
                       #+nil
                       (chapter (:title "About us")
                         (paragraph ()
                           "We are a company specialized on software development. Our main focus is on Web based business application development, including the expertise needed to evaluate and model complex business processes.")
                         (paragraph ()
                           "We are dedicated to open source software development. Everything you can see on this site is based on open source software components, many of them written by us."))
                       (chapter (:title "About this site")
                         (paragraph ()
                           "This site is a live demonstration and a reflexive documentation for our software libraries written in Common Lisp. It's all open source, and it has been used as the framework for a few paying projects."))
                       #+nil
                       (chapter (:title "Platform")
                         (paragraph ()
                           "This dwim Server installation runs under Ubuntu Linux x86-64 Server Edition and Steel Banks Common Lisp (SBCL) x86-64. In principle, other operating systems and other Common Lisp implementations may also be used, but we are not testing the dwim Server and the related software components with them. Some of the dwim libraries use non standard Common Lisp code, so porting to another implementation might require deep understanding of the related issues. If you are interested in doing or having this, please contact us."))
                       #+nil
                       (chapter (:title "Hardware")
                         (paragraph ()
                           "The dwim Server runs on a multicore - " (machine-version) " - computer with a couple of gigabytes memory. To decrease costs the operating system runs using virtualization. Unfortunately the dwim Server is connected to the Internet with a quite slow network at the moment. For these reasons perfromance may suffer, but there is no inherent problem there."))
                       #+nil
                       (chapter (:title "Architecture overview")
                         (paragraph ()
                           "The dwim Server runs on the scalable iolib based pure lisp-from-the-socket web server called hu.dwim.web-server. It stores persistent data in the Common Lisp Object System (CLOS) based Object Relational Mapping (ORM) called hu.dwim.perec. The persistence layer is backed up with PostgreSQL, but other relational databases such as Sqlite, and Oracle are also partially supported. The Common Lisp code fragments that are presented throughout this site are shown by introspection. In other words they are part of the live system, and this serves well our goal of providing reflexive documentation."))
                       #+nil
                       (chapter (:title "Licence")
                         (paragraph ()
                           "The dwim Server is built using only Open Source software. The Ubuntu Linux operating system, the SBCL Common Lisp implementation, the PostgreSQL relational database, and the dwim Common Lisp libraries are all Open Source Software. We try to avoid using software components that have restrictive licences. For commercial purposes GPL is restrictive, so we try to avoid that. The only notable exception at the moment is the Common Lisp code syntax highlighter. For more details on the individual licences (BSD, MIT, LGPL, GPL, etc.) please check the Licence menu item."))
                       #+nil
                       (chapter (:title "Installation")
                         (paragraph ()
                           "The dwim Server and the dwim Common Lisp projects can be installed for free for any puprpose including commercial purposes. The Install Guide is found under the Documentation menu item. It provides separate installation instructions for all dwim Common Lisp projects including all dependencies, and assuming as little as possible of your system."))
                       (chapter (:title "Contact us")
                         (paragraph ()
                           "The company behind dwim.hu is called M. Wallen Software Ltd.")
                         (paragraph ()
                           "You can email us at attila.lendvai and/or levente.meszaros on gmail.com.")
                         (paragraph ()
                           ;; TODO #dwim.hu goes as it is into the html... check whether that's legal or not.
                           "Alternatively you can catch us on IRC at European daytime, on the " (hyperlink "http://freenode.net/") " server, in the #dwim.hu channel.")))))))
    (values (menu-item/widget ()
                (replace-target-place/widget ()
                    "Home"
                  content))
            content)))

(def function make-news-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "News"
        (inline-render-xhtml/widget ()
          "Soon"))))

(def function make-blog-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Blog"
        "Soon")))

(def function make-forum-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Forum"
        (inline-render-xhtml/widget ()
          "Soon"))))

(def function make-contact-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Contact"
        (inline-render-xhtml/widget ()
          "Soon"))))

(def function make-licence-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Licence"
        (make-live-project-licences-inspector))))

(def function make-help-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Help"
        (make-instance 'usage-help/widget))))

(def function make-live-project-licences-inspector ()
  (make-instance 'sequence/alternator/inspector
                 :component-value (mapcar (lambda (pathname)
                                            (hu.dwim.presentation::project-licence-pathname
                                             (bind ((project-name (project-name pathname)))
                                               (or (find-project project-name :otherwise nil)
                                                   (make-instance 'project
                                                                  :name project-name
                                                                  :path pathname)))))
                                          (collect-live-project-pathnames))))

;;;;;;
;;; Demo menu

(def function make-demo-menu ()
  (menu-item/widget ()
      "Demo"
    (make-server-status-menu-item)
    (make-echo-server-demo-menu-item)
    (make-hello-world-server-demo-menu-item)
    (make-home-server-demo-menu-item)
    (make-wui-demo-menu-item)
    (make-perec-demo-menu-item)))

(def function make-server-status-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Server Status"
        (bind ((uri (hu.dwim.uri:delete-all-query-parameters (make-uri-for-current-application "status"))))
          (make-value-inspector
           (book (:title "Server Status" :authors '("Levente Mészáros"))
             (chapter (:title "Introduction")
               "This example demonstrates how to define a simple entry point that shows some server status information. The live entry point is at: "
               uri)
             (chapter (:title "Source")
               (paragraph ()
                 "The status is rendered by the following code:")
               (fdefinition 'write-server-status)
               (paragraph ()
                 "The entry point is defined in the following code, look at the entry-point 'status':")
               ;; TODO: get the entry-point's code only
               (system-relative-pathname :hu.dwim.home "source/entry-point.lisp")
               (paragraph ()
                 "The demo menu item is created by the following code:")
               (fdefinition 'make-server-status-menu-item))
             (chapter (:title "Live")
               ;; TODO FIXME this and all the rest are broken: the result will end up xhtml quoted...
               (inline-render-xhtml/widget ()
                 <iframe (:width "100%" :height "400px" :style "border: none;" :src ,(hu.dwim.uri:print-uri-to-string uri))>))))))))

(def function make-echo-server-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Echo Server"
        (bind ((uri (hu.dwim.uri:delete-all-query-parameters (make-uri-for-current-application "echo"))))
          (make-value-inspector
           (book (:title "Echo Server" :authors '("Levente Mészáros"))
             (chapter (:title "Introduction")
               "This example demonstrates how to define a simple request echo server. The live entry point is at: "
               uri)
             (chapter (:title "Source")
               (paragraph ()
                 "The echo server source is the following:")
               (system-relative-pathname :hu.dwim.home "source/echo-server.lisp")
               (paragraph ()
                 "The demo menu item is created by the following code:")
               (fdefinition 'make-echo-server-demo-menu-item))
             (chapter (:title "Live")
               (inline-render-xhtml/widget ()
                 <iframe (:width "100%" :height "400px" :style "border: none;" :src ,(hu.dwim.uri:print-uri-to-string uri))>))))))))

(def function make-hello-world-server-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Hello World Server"
        (bind ((uri (hu.dwim.uri:delete-all-query-parameters (make-uri-for-current-application "hello-world"))))
          (make-value-inspector
           (book (:title "Hello World Server" :authors '("Levente Mészáros"))
             (chapter (:title "Introduction")
               "This example demonstrates how to define a simple hello world server. The live entry point is at: "
               uri)
             (chapter (:title "Source")
               (paragraph ()
                 "The hello world server source is the following:")
               (system-relative-pathname :hu.dwim.home "source/hello-world-server.lisp")
               (paragraph ()
                 "The demo menu item is created by the following code:")
               (fdefinition 'make-hello-world-server-demo-menu-item))
             (chapter (:title "Live")
               (inline-render-xhtml/widget ()
                 <iframe (:width "100%" :height "100px" :style "border: none;" :src ,(hu.dwim.uri:print-uri-to-string uri))>))))))))

(def function make-home-server-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Home Server"
        (make-value-inspector
         (book (:title "Home Server" :authors '("Levente Mészáros"))
           (chapter (:title "Introduction")
             "This example demonstrates how the dwim Home Server is defined that is running at:" (net.uri:parse-uri "http://dwim.hu/"))
           (chapter (:title "Source")
             (paragraph ()
               "The following source file defines the server, the application and the toplevel function used to execute the server:")
             (system-relative-pathname :hu.dwim.home "source/server.lisp")
             (paragraph ()
               "The demo menu item is created by the following code:")
             (fdefinition 'make-home-server-demo-menu-item)))))))

(def function make-wui-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "User Interface"
        ;; NOTE: due to loading issues related to hu.dwim.home not being dependent on hu.dwim.presentation.test
        (funcall (find-symbol "MAKE-COMPONENT-DEMO-CONTENT" :hu.dwim.presentation.test)))))

(def function make-perec-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Persistent Data"
        (inline-render-xhtml/widget ()
          "Soon"))))

;;;;;;
;;; Project menu

(def function make-project-menu ()
  (bind (((dwim-projects other-projects) (partition (collect-live-project-pathnames)
                                                    (lambda (pathname)
                                                      (search "hu.dwim" (last-elt (pathname-directory pathname))))
                                                    (constantly #t))))
    (menu-item/widget ()
        "Project"
      (make-menu-item "Dwim" (mapcar 'make-project-menu-item dwim-projects))
      (make-menu-item "Other" (mapcar 'make-project-menu-item other-projects)))))

(def function make-project-menu-item (pathname)
  (bind ((project-name (project-name pathname))
         (project (or (find-project project-name :otherwise nil)
                      (make-instance 'project
                                     :name project-name
                                     :path pathname))))
    (menu-item/widget ()
        (replace-target-place/widget ()
            (string-downcase (hu.dwim.presentation::name-of project))
          (make-value-inspector project)))))

;; KLUDGE this should dispatch on PROJECT, or should be done in a different way
(def method make-project-tab-pages :around ((component project/detail/inspector) project)
  (append (call-next-method)
          (bind ((project-package-name (string (hu.dwim.presentation::name-of project)))
                 (doc-package (awhen (starts-with-subseq (symbol-name '#:hu.dwim.) project-package-name)
                                (find-package (string+ project-package-name (string '#:.documentation))))))
            (awhen (and doc-package
                        (find-user-guide doc-package))
              (list (tab-page/widget (:selector (icon/widget switch-to-tab-page :label "User guide"))
                      (make-value-inspector it)))))))

;;;;;;
;;; Documentation menu

(def function make-documentation-menu ()
  (menu-item/widget ()
      "Documentation"
    (make-install-guide-menu-item)
    (make-wui-documentation-item)
    (make-perec-documentation-item)))

(def function make-install-guide-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Install Guide"
        (make-value-inspector (find-book 'install-guide)))))

(def function make-tutorial-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Tutorial"
        (make-value-inspector (find-book 'tutorial)))))

(def function make-wui-documentation-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "User Interface"
        (make-value-inspector (find-user-guide :hu.dwim.presentation)))))

(def function make-perec-documentation-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Persistent Data"
        (make-value-inspector (find-user-guide :hu.dwim.perec)))))

(def function collect-live-project-pathnames ()
  (iter (for pathname :in (directory (merge-pathnames *workspace-directory* "/*.*")))
        (unless (or (pathname-name pathname)
                    (char= #\. (first-elt (last-elt (pathname-directory pathname)))))
          (collect pathname))))

;;;;;;
;;; Repository menu

(def function make-repository-menu ()
  (menu-item/widget ()
      "Repository"
    (make-darcs-repository-menu-item)
    (make-git-repository-menu-item)))

(def function make-darcs-repository-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Darcs"
        (inline-render-xhtml/widget ()
          <iframe (:width "100%" :height "5000px" :style "border: none; overflow: hidden" :src "/darcsweb/darcsweb.cgi")>))))

(def function make-git-repository-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Git"
        (inline-render-xhtml/widget ()
          <iframe (:width "100%" :height "1000px" :style "border: none; overflow: hidden" :src "/gitweb/gitweb.cgi")>))))

;;;;;;
;;; Source menu

(def function make-source-menu ()
  (menu-item/widget ()
      "Source"
    (make-definition-browser-menu-item)
    (make-class-browser-menu-item)
    (make-function-browser-menu-item)
    (make-file-browser-menu-item)))

(def function make-definition-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Definition Browser"
        (make-filter 'definition))))

(def function make-class-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Class Browser"
        (make-filter 'class))))

(def function make-function-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Function Browser"
        (make-filter 'function))))

(def function make-file-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "File Browser"
        (make-filter 'asdf:source-file))))

;;;;;;
;;; Test menu

(def function make-test-menu ()
  (menu-item/widget ()
      "Test"
    (make-test-browser-menu-item)
    (make-system-test-result-browser-menu-item)
    (make-individual-test-result-browser-menu-item)
    (make-latest-system-test-results-menu-item)))

(def function make-test-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Test Code Browser"
        (make-filter 'hu.dwim.stefil::test))))

(def function make-individual-test-result-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Individual Test Result Browser"
        (make-filter 'individual-test-result))))

(def function make-system-test-result-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "System Test Result Browser"
        (make-filter 'system-test-result))))

(def function make-latest-system-test-results-menu-item ()
  (flet ((make (title compared-system-version &optional (base-system-version compared-system-version))
           (replace-target-place/widget ()
               title
             (make-periodic-standalone-test-report title compared-system-version base-system-version))))
    (menu-item/widget ()
        "Latest System Test Results"
      (menu-item/widget ()
          (make "Live Repositories (Comparing Last 2 Results)" :live))
      (menu-item/widget ()
          (make "Head Repositories (Comparing Last 2 Results)" :head))
      (menu-item/widget ()
          (make "Head Compared To Live Repositories" :head :live)))))
