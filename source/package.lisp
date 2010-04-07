;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.home
  (:use :cl-l10n
        :command-line-arguments
        :contextl
        :hu.dwim.asdf
        :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.logger
        :hu.dwim.meta-model
        :hu.dwim.model
        :hu.dwim.perec
        :hu.dwim.rdbms
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.wui
        :iolib
        :local-time)
  (:shadowing-import-from :hu.dwim.perec
                          #:ip-address)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.wui)))
