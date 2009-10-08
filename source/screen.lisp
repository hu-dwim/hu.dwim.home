;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; constants

(def constant +page-icon+ "static/favicon.ico")

(def constant +script-uris+ '("wui/js/wui.js" "wui/js/component-hierarchy.js"))

(def constant +stylesheet-uris+ (append (flet ((entry (path)
                                                 (list (string+ "static/" path)
                                                       (system-relative-pathname :hu.dwim.home (string+ "www/" path))))
                                               (dojo-relative-path (path)
                                                 (string+ *dojo-directory-name* path)))
                                          (list (entry "home/css/home.css")
                                                (entry "wui/css/wui.css")
                                                (entry "wui/css/icon.css")
                                                (entry "wui/css/widget.css")
                                                (entry (dojo-relative-path "dijit/themes/tundra/tundra.css"))
                                                (entry (dojo-relative-path "dojo/resources/dojo.css"))))))


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
                     (make-debug-menu))))
    (frame/widget (:title "dwim.hu" :page-icon +page-icon+ :script-uris +script-uris+ :stylesheet-uris +stylesheet-uris+)
      (top/widget (:menu-bar menu-bar)
        (content/widget ()
          (or content
              initial-content))))))

;;;;;;
;;; Home

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
                     (book (:title "dwim.hu")
                       (chapter (:title "Introduction")
                         (paragraph ()
                           "This website is a live demostration and a reflexive documentation for our Common Lisp projects."))
                       (chapter (:title "Platform")
                         "The dwim Server runs under Ubuntu Linux x86-64 Server Edition and  Steel Banks Common Lisp (SBCL) x86-64.")
                       (chapter (:title "Software")
                         (paragraph ()
                           "The dwim Server runs on our scalable iolib based pure lisp-from-the-socket web server called hu.dwim.wui.
It stores persistent data in our Common Lisp Object System (CLOS) Object Relational Mapping (ORM) called hu.dwim.perec, backed up with PostgreSQL.
The Common Lisp code fragments that are presented throughout the site are shown by introspection. In other words they are part of the live system."))
                       (chapter (:title "Licence")
                         (paragraph ()
                           "The dwim Server is built using only Open Source software. The operating system, the Common Lisp implementation,
the database, and our software components are all public domain software. For more details on the individual licences (BSD, MIT, LGPL) please check
the Licence menu item."))
                       (chapter (:title "Installation")
                         (paragraph ()
                           "The dwim Server can be installed for free for any purpose. The Install Guide is found under the Documentation menu item.")))
                     :initial-alternative-type 't/text/inspector))))
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
        (inline-render-xhtml/widget ()
          "Soon"))))

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
  (make-instance 'vertical-list/layout
                 :contents (mapcar (lambda (pathname)
                                     (make-value-inspector (hu.dwim.wui::project-licence-pathname (or (find-project-by-path pathname)
                                                                                                      (make-instance 'project :path pathname)))
                                                           :default-alternative-type 'pathname/text-file/inspector))
                                   (collect-live-project-pathnames))))

;;;;;;
;;; Demo

(def function make-demo-menu ()
  (menu-item/widget ()
      "Demo"
    (make-server-status-menu-item)
    (make-echo-server-demo-menu-item)
    (make-hello-world-server-demo-menu-item)
    (make-wui-demo-menu-item)
    (make-perec-demo-menu-item)))

(def function make-server-status-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Server Status"
        (bind ((uri "http://dwim.hu/status"))
          (make-value-inspector
           (book (:title "Server Status")
             (chapter (:title "Introduction")
               "This example demonstrates how to define a simple entry point that shows some server status information. The live entry point is at: "
               (parse-uri uri))
             (chapter (:title "Source")
               (paragraph ()
                 "The status is rendered by the following code:")
               (make-value-inspector (fdefinition 'write-server-status) :initial-alternative-type 'function/lisp-form/inspector)
               (paragraph ()
                 "The entry point is defined in the following code, look at the entry-point 'status':")
               ;; TODO: get the entry-point's code only
               (make-value-inspector (system-relative-pathname :hu.dwim.home "source/entry-point.lisp") :initial-alternative-type 'pathname/lisp-file/inspector)
               (paragraph ()
                 "The demo menu item is created by the following code:")
               (make-value-inspector (fdefinition 'make-server-status-menu-item) :initial-alternative-type 'function/lisp-form/inspector))
             (chapter (:title "Live")
               (inline-render-xhtml/widget ()
                 <iframe (:width "100%" :height "400px" :style "border: none;" :src ,uri)>)))
           :initial-alternative-type 't/text/inspector)))))

(def function make-echo-server-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Echo Server"
        (bind ((uri "http://dwim.hu/echo"))
          (make-value-inspector
           (book (:title "Echo Server")
             (chapter (:title "Introduction")
               "This example demonstrates how to define a simple request echo server. The live entry point is at: "
               (parse-uri uri))
             (chapter (:title "Source")
               (paragraph ()
                 "The echo server source is the following:")
               (make-value-inspector (system-relative-pathname :hu.dwim.home "source/echo-server.lisp") :initial-alternative-type 'pathname/lisp-file/inspector)
               (paragraph ()
                 "The demo menu item is created by the following code:")
               (make-value-inspector (fdefinition 'make-echo-server-demo-menu-item) :initial-alternative-type 'function/lisp-form/inspector))
             (chapter (:title "Live")
               (inline-render-xhtml/widget ()
                 <iframe (:width "100%" :height "400px" :style "border: none;" :src ,uri)>)))
           :initial-alternative-type 't/text/inspector)))))

(def function make-hello-world-server-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Hello World Server"
        (bind ((uri "http://dwim.hu/hello-world"))
          (make-value-inspector
           (book (:title "Hello World Server")
             (chapter (:title "Introduction")
               "This example demonstrates how to define a simple hello world server. The live entry point is at: "
               (parse-uri uri))
             (chapter (:title "Source")
               (paragraph ()
                 "The hello world server source is the following:")
               (make-value-inspector (system-relative-pathname :hu.dwim.home "source/hello-world-server.lisp") :initial-alternative-type 'pathname/lisp-file/inspector)
               (paragraph ()
                 "The demo menu item is created by the following code:")
               (make-value-inspector (fdefinition 'make-hello-world-server-demo-menu-item) :initial-alternative-type 'function/lisp-form/inspector))
             (chapter (:title "Live")
               (inline-render-xhtml/widget ()
                 <iframe (:width "100%" :height "100px" :style "border: none;" :src ,uri)>)))
           :initial-alternative-type 't/text/inspector)))))

(def function make-wui-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "User Interface"
        (hu.dwim.wui.test::make-demo-content))))

(def function make-perec-demo-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Persistent Data"
        (inline-render-xhtml/widget ()
          "Soon"))))

;;;;;;
;;; Project

(def function make-project-menu ()
  (make-menu-item "Project" (mapcar #'make-project-menu-item (collect-live-project-pathnames))))

(def function make-project-menu-item (pathname)
  (bind ((project (or (find-project-by-path pathname)
                      (make-instance 'project :path pathname))))
    (menu-item/widget ()
        (replace-target-place/widget ()
            (hu.dwim.wui::name-of project)
          (make-value-inspector project :initial-alternative-type 't/detail/presentation)))))

;;;;;;
;;; Documentation

(def function make-documentation-menu ()
  (menu-item/widget ()
      "Documentation"
    (make-install-guide-menu-item)
    (make-tutorial-menu-item)
    (make-wui-documentation-item)
    (make-perec-documentation-item)))

(def function make-install-guide-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Install Guide"
        (make-value-inspector (find-book 'install-guide) :initial-alternative-type 'book/text/inspector))))

(def function make-tutorial-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Tutorial"
        (make-value-inspector (find-book 'tutorial) :initial-alternative-type 'book/text/inspector))))

(def function make-wui-documentation-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "User Interface"
        (make-value-inspector (find-book 'hu.dwim.wui.documentation::user-guide) :initial-alternative-type 'book/text/inspector))))

(def function make-perec-documentation-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Persistent Data"
        (make-value-inspector (find-book 'hu.dwim.perec.documentation::user-guide) :initial-alternative-type 'book/text/inspector))))

(def function collect-live-project-pathnames ()
  (iter (for pathname :in (directory (merge-pathnames *workspace-directory* "/*.*")))
        (unless (or (pathname-name pathname)
                    (char= #\. (first-elt (last-elt (pathname-directory pathname))))
                    (string= "sbcl" (last-elt (pathname-directory pathname))))
          (collect pathname))))

;;;;;;
;;; Repository

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
;;; Source

(def function make-source-menu ()
  (menu-item/widget ()
      "Source"
    (make-class-browser-menu-item)
    (make-function-browser-menu-item)
    (make-file-browser-menu-item)))

(def function make-class-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Class Browser"
        (make-value-inspector (find-class 'hu.dwim.perec::persistent-class) :initial-alternative-type 't/lisp-form/inspector))))

(def function make-function-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "Function Browser"
        (make-value-inspector (fdefinition 'make-instance) :initial-alternative-type 't/lisp-form/inspector))))

(def function make-file-browser-menu-item ()
  (menu-item/widget ()
      (replace-target-place/widget ()
          "File Browser"
        (make-value-inspector (system-relative-pathname :hu.dwim.home "source/server.lisp") :initial-alternative-type 'pathname/lisp-file/inspector))))
