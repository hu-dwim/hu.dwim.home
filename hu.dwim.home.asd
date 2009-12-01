;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.home
  :class hu.dwim.system
  :description "A web application for all hu.dwim systems including their test suite and documentation."
  :depends-on (:hu.dwim.logger
               :hu.dwim.perec.documentation
               :hu.dwim.perec+iolib
               :hu.dwim.wui.documentation
               :hu.dwim.wui+hu.dwim.perec
               :hu.dwim.wui+hu.dwim.reader
               :hu.dwim.wui+stefil
               :hu.dwim.meta-model
               :hu.dwim.model

               ;; TODO this should be (an optional) part of the build process
               ;; keep it separate for now, so that it's easy to comment them out when not building the final product
               :hu.dwim.asdf.documentation
               :hu.dwim.blog.documentation
               :hu.dwim.build.documentation
               :hu.dwim.common.documentation
               :hu.dwim.common-lisp.documentation
               :hu.dwim.computed-class.documentation
               :hu.dwim.debug.documentation
               :hu.dwim.def.documentation
               :hu.dwim.defclass-star.documentation
               :hu.dwim.delico.documentation
               :hu.dwim.dises.documentation
               :hu.dwim.graphviz.documentation
               :hu.dwim.lazy-eval.documentation
               :hu.dwim.logger.documentation
               :hu.dwim.meta-model.documentation
               :hu.dwim.model.documentation
               :hu.dwim.new-project.documentation
               :hu.dwim.partial-eval.documentation
               :hu.dwim.quasi-quote.documentation
               :hu.dwim.rdbms.documentation
               :hu.dwim.reader.documentation
               :hu.dwim.remote-eval.documentation
               :hu.dwim.serializer.documentation
               :hu.dwim.stefil.documentation
               :hu.dwim.syntax-sugar.documentation
               :hu.dwim.util.documentation
               :hu.dwim.walker.documentation
               :hu.dwim.wiki.documentation
               :hu.dwim.wui.documentation
               )
  :components ((:module "source"
                :components ((:file "echo-server" :depends-on ("server"))
                             (:file "entry-point" :depends-on ("server"))
                             (:file "hello-world-server" :depends-on ("server"))
                             (:file "install-guide" :depends-on ("screen"))
                             (:file "logger" :depends-on ("package"))
                             (:file "package")
                             (:file "screen" :depends-on ("logger"))
                             (:file "server" :depends-on ("screen"))
                             (:file "tutorial" :depends-on ("logger"))))))

(defmethod perform :after ((o develop-op) (c (eql (find-system :hu.dwim.home))))
  (eval (let ((*package* (find-package :hu.dwim.home)))
          (read-from-string
           "(progn
              (unless (connection-specification-of *model*)
                (setf (connection-specification-of *model*)
                      `(:host \"localhost\" :port 5432 :database \"hu.dwim.home\" :user-name \"hu.dwim.home\" :password \"engedjbe\")))
              (setf *database* (database-of *model*))
              (setf hu.dwim.perec::*compiled-query-cache* (make-compiled-query-cache))
              (setf *debug-on-error* t)
              (setf (current-locale) (list \"en\"))
              (setf (running-in-test-mode? *home-application*) t)
              (unless (dojo-directory-name-of *home-application*)
                (setf (dojo-directory-name-of *home-application*) (find-latest-dojo-directory-name (asdf:system-relative-pathname :hu.dwim.home \"www/\")))
                (warn \"dojo-directory-name of *home-application* was nil, it was set it to the latest available in the www/ directory: ~S\" (dojo-directory-name-of *home-application*)))
              (startup-server *home-server*))")))
  (warn "Made sideffects on the following global variables: *database*, *compiled-query-cache*, *debug-on-error*, *locale*."))
