;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.wui)

;;;;;;
;;; Icon

(def localization en
  (class-name.system-test-result "system test result")
  (class-name.test-environment "test environment")
  (class-name.test-result "test result")

  (comparison-result.better "better")
  (comparison-result.new "new")
  (comparison-result.same "same")
  (comparison-result.unknown "unknown")
  (comparison-result.worse "worse")

  (slot-name.assertion-count "assertion count")
  (slot-name.comparison-result "comparison result")
  (slot-name.compile-output "compile output")
  (slot-name.error-count "error count")
  (slot-name.expected-failure-count "expected failure count")
  (slot-name.failure-count "failure count")
  (slot-name.lisp-implementation-type "lisp implementation type")
  (slot-name.lisp-implementation-version "lisp implementation version")
  (slot-name.load-output "load output")
  (slot-name.machine-instance "machine instance")
  (slot-name.machine-type "machine type")
  (slot-name.machine-version "machine version")
  (slot-name.run-at "run at")
  (slot-name.success-count "success count")
  (slot-name.system-name "system name")
  (slot-name.system-version "system version")
  (slot-name.test-duration "test duration")
  (slot-name.test-name "test name")
  (slot-name.test-output "test output")
  (slot-name.test-result "test result")

  (test-result.fail "fail")
  (test-result.pass "pass"))
