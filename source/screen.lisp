;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Frame

(def function make-frame-component (&optional content)
  (bind (((:values home-menu initial-content) (make-home-menu))
         (menu-bar (menu-bar/widget ()
                     home-menu
                     (make-documentation-menu)
                     (make-demo-menu)
                     (make-project-menu)
                     (make-repository-menu)
                     (make-source-menu)
                     (make-test-menu)
                     (make-debug-menu))))
    (frame/widget (:title "dwim.hu"
                   :stylesheet-uris (append
                                     (make-default-stylesheet-uris)
                                     (mapcar (make-stylesheet-uri-entry-factory :hu.dwim.home)
                                             '("home/css/home.css"))))
      (top/widget (:menu-bar menu-bar)
        (or content initial-content)))))

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
                    (image/widget :id "dwim-logo" :location (make-uri-for-current-application "static/wui/image/about/dwim-logo.png"))
                    (make-value-inspector
                     (book (:title "dwim.hu" :authors '("Levente Mészáros"))
                       (paragraph ()
                         "The acronym dwim stands for the well known phrase: do what I mean. It is a humorous way of describing a user's feeling that a computer function should work the way the user wants, instead of the actual result that the user did not want. This term has been in use for decades. The acronym hu refers to Hungary, our home country.")
                       (chapter (:title "Introduction")
                         (paragraph ()
                           "This website is a live demostration and a reflexive documentation for the dwim Common Lisp projects. The dwim Server allows browsing the project test suites and the latest test results. To aid development the site also provides Darcs based distributed version control repositories for all hu.dwim projects."))
                       (chapter (:title "Platform")
                         (paragraph ()
                           "This dwim Server installation runs under Ubuntu Linux x86-64 Server Edition and Steel Banks Common Lisp (SBCL) x86-64. In principle, other operating systems and other Common Lisp implementations may also be used, but we are not testing the dwim Server and the related software components with them. Some of the dwim libraries use non standard Common Lisp code, so porting to another implementation might require deep understanding of the related issues. If you are interested in doing or having this, please contact us."))
                       (chapter (:title "Hardware")
                         (paragraph ()
                           "The dwim Server runs on a multicore - " (machine-version) " - computer with a couple of gigabytes memory. To decrease costs the operating system runs using virtualization. Unfortunately the dwim Server is connected to the Internet with a quite slow network at the moment. For these reasons perfromance may suffer, but there is no inherent problem there."))
                       (chapter (:title "Software")
                         (paragraph ()
                           "The dwim Server runs on the scalable iolib based pure lisp-from-the-socket web server called hu.dwim.wui. It stores persistent data in the Common Lisp Object System (CLOS) based Object Relational Mapping (ORM) called hu.dwim.perec. The persistence layer is backed up with PostgreSQL, but other relational databases such as Sqlite, and Oracle are also partially supported. The Common Lisp code fragments that are presented throughout this site are shown by introspection. In other words they are part of the live system, and this serves well our goal of providing reflexive documentation."))
                       (chapter (:title "Licence")
                         (paragraph ()
                           "The dwim Server is built using only Open Source software. The Ubuntu Linux operating system, the SBCL Common Lisp implementation, the PostgreSQL relational database, and the dwim Common Lisp libraries are all Open Source Software. We try to avoid using software components that have restrictive licences. For commercial purposes GPL is restrictive, so we try to avoid that. The only notable exception at the moment is the Common Lisp code syntax highlighter. For more details on the individual licences (BSD, MIT, LGPL, GPL, etc.) please check the Licence menu item."))
                       (chapter (:title "Installation")
                         (paragraph ()
                           "The dwim Server and the dwim Common Lisp projects can be installed for free for any puprpose including commercial purposes. The Install Guide is found under the Documentation menu item. It provides separate installation instructions for all dwim Common Lisp projects including all dependencies, and assuming as little as possible of your system."))
                       (chapter (:title "Contact")
                         (paragraph ()
                           "The company behind dwim.hu is called M. Wallen Software Ltd.")
                         (paragraph ()
                           "The people involved are Levente Mészáros, Attila Lendvai, Tamás Borbély and Bálint Mészáros. Send emails to either Attila or Levente or both to gmail.com.")))))))
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
  (make-instance 'sequence/inspector
                 :component-value (mapcar (lambda (pathname)
                                            (hu.dwim.wui::project-licence-pathname (or (find-project-by-path pathname)
                                                                                       (make-instance 'project :path pathname))))
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
        (bind ((uri "http://dwim.hu/status"))
          (make-value-inspector
           (book (:title "Server Status" :authors '("Levente Mészáros"))
             (chapter (:title "Introduction")
               "This example demonstrates how to define a simple entry point that shows some server status information. The live entry point is at: "
               (parse-uri uri))
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
               (inline-render-xhtml/widget ()
                 <iframe (:width "100%" :height "400px" :style "border: none;" :src ,uri)>))))))))

(def function make-echo-server-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Echo Server"
        (bind ((uri "http://dwim.hu/echo"))
          (make-value-inspector
           (book (:title "Echo Server" :authors '("Levente Mészáros"))
             (chapter (:title "Introduction")
               "This example demonstrates how to define a simple request echo server. The live entry point is at: "
               (parse-uri uri))
             (chapter (:title "Source")
               (paragraph ()
                 "The echo server source is the following:")
               (system-relative-pathname :hu.dwim.home "source/echo-server.lisp")
               (paragraph ()
                 "The demo menu item is created by the following code:")
               (fdefinition 'make-echo-server-demo-menu-item))
             (chapter (:title "Live")
               (inline-render-xhtml/widget ()
                 <iframe (:width "100%" :height "400px" :style "border: none;" :src ,uri)>))))))))

(def function make-hello-world-server-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Hello World Server"
        (bind ((uri "http://dwim.hu/hello-world"))
          (make-value-inspector
           (book (:title "Hello World Server" :authors '("Levente Mészáros"))
             (chapter (:title "Introduction")
               "This example demonstrates how to define a simple hello world server. The live entry point is at: "
               (parse-uri uri))
             (chapter (:title "Source")
               (paragraph ()
                 "The hello world server source is the following:")
               (system-relative-pathname :hu.dwim.home "source/hello-world-server.lisp")
               (paragraph ()
                 "The demo menu item is created by the following code:")
               (fdefinition 'make-hello-world-server-demo-menu-item))
             (chapter (:title "Live")
               (inline-render-xhtml/widget ()
                 <iframe (:width "100%" :height "100px" :style "border: none;" :src ,uri)>))))))))

(def function make-home-server-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Home Server"
        (make-value-inspector
         (book (:title "Home Server" :authors '("Levente Mészáros"))
           (chapter (:title "Introduction")
             "This example demonstrates how the dwim Home Server is defined that is running at:" (parse-uri "http://dwim.hu/"))
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
        ;; NOTE: due to loading issues related to hu.dwim.home not being dependent on hu.dwim.wui.test
        (funcall (find-symbol "MAKE-COMPONENT-DEMO-CONTENT" :hu.dwim.wui.test)))))

(def function make-perec-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Persistent Data"
        (inline-render-xhtml/widget ()
          "Soon"))))

;;;;;;
;;; Project menu

(def function make-project-menu ()
  (bind (((dwim-projects other-projects) (hu.dwim.wui::partition (collect-live-project-pathnames)
                                                                 (lambda (pathname)
                                                                   (search "hu.dwim" (last-elt (pathname-directory pathname))))
                                                                 (constantly #t))))
    (menu-item/widget ()
        "Project"
      (make-menu-item "Dwim" (mapcar #'make-project-menu-item dwim-projects))
      (make-menu-item "Other" (mapcar #'make-project-menu-item other-projects)))))

(def function make-project-menu-item (pathname)
  (bind ((project (or (find-project-by-path pathname)
                      (make-instance 'project :path pathname))))
    (menu-item/widget ()
        (replace-target-place/widget ()
            (hu.dwim.wui::name-of project)
          (make-value-inspector project)))))

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
        (make-value-inspector (find-user-guide :hu.dwim.wui)))))

(def function make-perec-documentation-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Persistent Data"
        (make-value-inspector (find-user-guide :hu.dwim.perec)))))

(def function collect-live-project-pathnames ()
  (iter (for pathname :in (directory (merge-pathnames *workspace-directory* "/*.*")))
        (unless (or (pathname-name pathname)
                    (char= #\. (first-elt (last-elt (pathname-directory pathname))))
                    (string= "sbcl" (last-elt (pathname-directory pathname))))
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
    (make-test-result-browser-menu-item)
    (make-system-test-result-browser-menu-item)
    (make-last-system-test-results-menu-item)))

(def function make-test-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Code browser"
        (make-filter 'hu.dwim.stefil::test))))

(def function make-test-result-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Result browser"
        (make-filter 'test-result))))

(def function make-system-test-result-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "System result browser"
        (make-filter 'system-test-result))))

(def function make-last-system-test-results-menu-item ()
  (menu-item/widget ()
      "Last system results"
    (menu-item/widget ()
        (replace-target-place/widget ()
            "Live"
          (make-periodic-standalone-test-report :live)))
    (menu-item/widget ()
        (replace-target-place/widget ()
            "Head "
          (make-periodic-standalone-test-report :head)))))
