;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(load-system :hu.dwim.asdf)

(in-package :hu.dwim.asdf)

(defsystem :hu.dwim.home
  :class hu.dwim.system
  :author ("Attila Lendvai <attila.lendvai@gmail.com>"
           "Levente Mészáros <levente.meszaros@gmail.com>"
           "Tamás Borbély <tomi.borbely@gmail.com>")
  :description "hu.dwim.home"
  :licence "BSD / Public domain"
  :depends-on (:hu.dwim.asdf.documentation
               :hu.dwim.blog.documentation
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
               :hu.dwim.perec.documentation
               :hu.dwim.quasi-quote.documentation
               :hu.dwim.rdbms.documentation
               :hu.dwim.reader.documentation
               :hu.dwim.remote-eval.documentation
               :hu.dwim.serializer.documentation
               :hu.dwim.stefil.documentation
               :hu.dwim.syntax-sugar.documentation
               :hu.dwim.util.documentation
               :hu.dwim.walker.documentation
               :hu.dwim.wui.documentation
               :hu.dwim.wui+hu.dwim.reader+hu.dwim.syntax-sugar
               :hu.dwim.wui+stefil)
  :components ((:module "source"
                :components ((:file "configuration" :depends-on ("package"))
                             (:file "entry-point" :depends-on ("server"))
                             (:file "install-guide" :depends-on ("screen"))
                             (:file "tutorial" :depends-on ("configuration"))
                             (:file "package")
                             (:file "screen" :depends-on ("configuration"))
                             (:file "server" :depends-on ("screen"))))))
