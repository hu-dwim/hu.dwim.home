;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; constants

(def (constant :test #'equal) +page-icon+ "static/favicon.ico")

(def (constant :test #'equal) +script-uris+ '("wui/js/wui.js" "wui/js/component-hierarchy.js"))

(def (constant :test #'equal) +stylesheet-uris+ (append (flet ((entry (path)
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
;;; Frame

(def function make-frame-component (&optional content)
  (bind (((:values home-menu initial-content) (make-home-menu))
         (menu-bar (menu-bar/widget ()
                     home-menu
                     (make-documentation-menu)
                     (make-demo-menu)
                     (make-project-menu)
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
                    (make-value-inspector (paragraph () "This website is a live demostration and reflexive documentation for our Common Lisp projects.")
                                          :initial-alternative-type 't/text/inspector)
                    (image/widget :id "dwim-logo" :location (make-uri-for-current-application "static/wui/image/about/dwim-logo.png")))))
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
    (make-wui-demo-menu-item)
    (make-perec-demo-menu-item)))

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
    (make-tutorial-menu-item)))

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

(def function collect-live-project-pathnames ()
  (iter (for pathname :in (directory (merge-pathnames *workspace-directory* "/*.*")))
        (unless (or (pathname-name pathname)
                    (char= #\. (first-elt (last-elt (pathname-directory pathname))))
                    (string= "sbcl" (last-elt (pathname-directory pathname))))
          (collect pathname))))

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
