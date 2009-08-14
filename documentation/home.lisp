;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home.documentation)

#|
asdf system definitions should be idiomatic if possible, that is:

(in-package :asdf)

(load-system :hu.dwim.asdf)

(defsystem :foo
  ;; ...
  )

optionally some defmethod specializing asdf's protocols

find-system    -> load the system definition into the running lisp vm and return it
compile-system -> compile the system but do not load it into the running lisp vm
load-system    -> compile and load the system into the running lisp vm
test-system    -> compile and load the test system of system and run the test suite
develop-system -> compile and load the test system of system, and change into the test package, and prepare for development

test system starts with the function (test)
|#
