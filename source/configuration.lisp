;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; These definitions need to be available by the time we are reading other files, therefore they are in a standalone file.

(def function setup-readtable ()
  (hu.dwim.wui::setup-readtable))

#+#.(cl:when (cl:find-package "SWANK") '(:and))
(register-readtable-for-swank
 '(:hu.dwim.home :hu.dwim.home.test) 'setup-readtable)
