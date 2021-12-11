;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(defsystem :hu.dwim.home.all
  :defsystem-depends-on (:hu.dwim.asdf)
  :class "hu.dwim.asdf:hu.dwim.system"
  :description "All hu.dwim systems including their test suite and documentation."
  :depends-on (:hu.dwim.asdf.documentation
               :hu.dwim.blog.documentation
               :hu.dwim.common-lisp.documentation
               :hu.dwim.common.documentation
               :hu.dwim.computed-class.documentation
               :hu.dwim.debug.documentation
               :hu.dwim.def/documentation
               :hu.dwim.defclass-star/documentation
               :hu.dwim.delico/documentation
               :hu.dwim.dises.documentation
               :hu.dwim.genetic-programming.documentation
               :hu.dwim.graphviz.documentation
               :hu.dwim.home.documentation
               :hu.dwim.lazy-eval/documentation
               :hu.dwim.logger.documentation
               :hu.dwim.meta-model.documentation
               :hu.dwim.model.documentation
               :hu.dwim.new-project.documentation
               :hu.dwim.partial-eval.documentation
               :hu.dwim.perec.documentation
               :hu.dwim.presentation.documentation
               :hu.dwim.quasi-quote.documentation
               :hu.dwim.rdbms.documentation
               :hu.dwim.reader.documentation
               :hu.dwim.reiterate.documentation
               :hu.dwim.remote-eval.documentation
               :hu.dwim.serializer.documentation
               ;; :hu.dwim.stefil.documentation
               :hu.dwim.syntax-sugar.documentation
               :hu.dwim.util.documentation
               :hu.dwim.vm.documentation
               :hu.dwim.walker/documentation
               :hu.dwim.web-server.documentation
               :hu.dwim.wiki.documentation))

(defmethod perform :after ((o hu.dwim.asdf:develop-op) (c (eql (find-system :hu.dwim.home.all))))
  (hu.dwim.asdf:develop-system :hu.dwim.home))
