;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.def)

(def package :hu.dwim.home.test
  (:use :hu.dwim.common
        :hu.dwim.def
        :hu.dwim.defclass-star
        :hu.dwim.home
        :hu.dwim.perec
        :hu.dwim.rdbms
        :hu.dwim.stefil
        :hu.dwim.syntax-sugar
        :hu.dwim.util
        :hu.dwim.wui)
  (:readtable-setup (setup-readtable/same-as-package :hu.dwim.home)))
