;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icon

(def localization en
  (class-name.test-environment "test environment")
  (class-name.system-test-result "system test result")
  (class-name.test-result "test result")

  (slot-name.machine-instance "machine instance")
  (slot-name.machine-type "machine type")
  (slot-name.machine-version "machine version")
  (slot-name.lisp-implementation-type "lisp implementation type")
  (slot-name.lisp-implementation-version "lisp implementation version")
  (slot-name.test-name "test name")
  (slot-name.test-duration "test duration")
  (slot-name.test-result "test result")
  (slot-name.assertion-count "assertion count")
  (slot-name.success-count "success count")
  (slot-name.expected-failure-count "expected failure count")
  (slot-name.failure-count "failure count")
  (slot-name.error-count "error count")
  (slot-name.system-name "system name")
  (slot-name.system-version "system version")
  (slot-name.run-at "run at")
  (slot-name.compile-output "compile output")
  (slot-name.load-output "load output")
  (slot-name.test-output "test output")

  (test-result.pass "pass")
  (test-result.fail "fail"))
