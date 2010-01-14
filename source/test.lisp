;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Model

(def (entity e) test-environment ()
  ((machine-instance
    (machine-instance)
    :type (text 32))
   (machine-type
    (machine-type)
    :type (text 32))
   (machine-version
    (machine-version)
    :type (text 128))
   (lisp-implementation-type
    (lisp-implementation-type)
    :type (text 32)
    :primary #t)
   (lisp-implementation-version
    (lisp-implementation-version)
    :type (text 32)
    :primary #t)))

(def (entity e) abstract-test-result ()
  ((test-name
    :type (symbol* 128))
   (test-duration
    :type (or null duration)
    :primary #t)
   (test-result
    :type (member :pass :fail)
    :reference #t
    :primary #t)
   (assertion-count
    :type (or null integer-64)
    :primary #t)
   (success-count
    :type (or null integer-64))
   (expected-failure-count
    :type (or null integer-64))
   (failure-count
    :type (or null integer-64)
    :primary #t)
   (error-count
    :type (or null integer-64)
    :primary #t))
  (:abstract #t))

(def (entity e) test-result (abstract-test-result)
  ((test-name
    :reference #t
    :primary #t)))

(def (entity e) system-test-result (abstract-test-result test-environment)
  ((system-name
    :type (text 128)
    :reference #t
    :primary #t)
   (system-version
    :type (text 128)
    :primary #t)
   (run-at
    :type timestamp
    :primary #t)
   (compile-output
    :type (or null text))
   (load-output
    :type (or null text))
   (test-output
    :type (or null text))))

(def association
  ((:slot system-test-result :type (or null system-test-result))
   (:slot test-results :type (set test-result))))

(def icon test-result-pass)

(def icon test-result-fail)

;;;;;;
;;; Standalone testing

(def (function e) store-system-test-result (system-name system-version run-at)
  (with-model-database
    (with-new-compiled-query-cache
      (with-transaction
        (%store-system-test-result system-name system-version run-at)))))

(def function %store-system-test-result (system-name system-version run-at)
  (bind ((system (find-system system-name))
         (test-system (find-system (system-test-system-name system)))
         (root-test-name (find-symbol "TEST" (system-package-name test-system)))
         (result (system-test-result test-system))
         (failures (hu.dwim.stefil::failure-descriptions-of result))
         (system-test-result (make-instance 'system-test-result
                                            :system-name (string-downcase system-name)
                                            :system-version (string-downcase system-version)
                                            :run-at run-at
                                            :compile-output (system-compile-output system)
                                            :load-output (system-load-output system)
                                            :test-output (system-test-output test-system)
                                            :test-name root-test-name
                                            :test-duration (hu.dwim.stefil::internal-realtime-spent-with-test-of (gethash (hu.dwim.stefil:find-test root-test-name) (hu.dwim.stefil::run-tests-of result)))
                                            :test-result (if (zerop (length failures))
                                                             :pass
                                                             :fail)
                                            ;; TODO: we don't have that information in the test results, do we?
                                            :assertion-count (hu.dwim.stefil::assertion-count-of result)
                                            :success-count nil
                                            :expected-failure-count nil
                                            :failure-count (length failures)
                                            :error-count nil)))
    (iter (for (test test-run) :in-hashtable (hu.dwim.stefil::run-tests-of result))
          (for test-result = (make-instance 'test-result
                                            :system-test-result system-test-result
                                            :test-name (hu.dwim.stefil::name-of test)
                                            :test-duration (hu.dwim.stefil::internal-realtime-spent-with-test-of test-run)
                                            :test-result (if (zerop (hu.dwim.stefil::number-of-added-failure-descriptions-of test-run))
                                                             :pass
                                                             :fail)
                                            ;; TODO: we don't have that information in the test-run, do we?
                                            :assertion-count nil
                                            :success-count nil
                                            :expected-failure-count nil
                                            :failure-count (hu.dwim.stefil::number-of-added-failure-descriptions-of test-run)
                                            :error-count nil)))
    system-test-result))

(def (function e) standalone-test-system (system-name system-version &key force)
  ;; TODO: support :head
  (assert (eq system-version :live))
  (home.info "Standalone test for ~A started" system-name)
  (bind ((last-test (with-transaction
                      (awhen (select-last-system-test-result system-name system-version)
                        (run-at-of it))))
         (last-write (system-write-timestamp system-name system-version)))
    (if (or force
            (not last-test)
            (local-time:timestamp< last-test last-write))
        ;; TODO: system-version should be either Live or Head, set up asdf central registry accordingly
        (bind ((run-at (local-time:now))
               (output-path (ensure-directories-exist (pathname (format nil "/tmp/test/~A/~A/" (string-downcase system-name) run-at))))
               (test-program `(progn ; NOTE: forms will be read and evaluated one after the other
                                (sb-ext::disable-debugger)
                                (load ,(truename (system-relative-pathname :hu.dwim.home "../hu.dwim.environment/source/environment.lisp")))
                                (setf asdf:*default-toplevel-directory* ,output-path)
                                (test-system ,system-name)
                                (load-system :hu.dwim.home)
                                (in-package :hu.dwim.home)
                                (setf (connection-specification-of *model*) ',(connection-specification-of *model*))
                                (store-system-test-result ,system-name ,system-version (parse-timestring ,(format-timestring nil run-at)))
                                (quit 0)))
               (sbcl-home (sb-posix:getenv "SBCL_HOME"))
               (shell-arguments `(,(namestring (truename (merge-pathnames "run-sbcl.sh" (pathname sbcl-home))))
                                   "--no-sysinit" "--no-userinit"
                                   "--eval" ,(let ((*package* (find-package :common-lisp)))
                                                  (format nil "~S" `(progn
                                                                      ,@(iter (for form :in (cdr test-program))
                                                                              (collect `(eval (read-from-string ,(format nil "~S" form)))))))))))
          (format *debug-io* "; Running standalone test for ~A with the following arguments (copy to shell):~%/bin/sh ~{~S ~}~%" system-name shell-arguments)
          (bind ((process (sb-ext:run-program "/bin/sh" shell-arguments
                                              :environment (remove nil (list* (when sbcl-home
                                                                                (concatenate 'string "SBCL_HOME=" sbcl-home))
                                                                              (sb-ext:posix-environ)))
                                              :wait #t)))
            (if (zerop (sb-ext::process-exit-code process))
                (home.info "Standalone test for ~A finished" system-name)
                (with-transaction
                  (make-instance 'system-test-result
                                 :system-name (string-downcase system-name)
                                 :system-version (string-downcase system-version)
                                 :run-at run-at
                                 :test-name nil
                                 :test-result :fail
                                 :compile-output "Failed"
                                 :load-output "Failed"
                                 :test-output "Failed")
                  (home.warn "Standalone test for ~A failed" system-name)))
            (iolib.os:delete-files output-path :recursive t)))
        (home.info "Standalone test result for ~A is up to date" system-name))))

(def (function e) collect-hu.dwim-system-names ()
  (iter (for (name specification) :in-hashtable asdf::*defined-systems*)
        (for system = (cdr specification))
        (for system-name = (asdf::component-name system))
        (when (and (typep system 'hu.dwim.asdf:hu.dwim.system)
                   (find-system (system-test-system-name system) nil))
          (collect system-name))))

(def (function e) standalone-test-hu.dwim-systems (system-version &key force)
  (dolist (system-name (collect-hu.dwim-system-names))
    (standalone-test-system system-name system-version :force force)))

(def (function e) select-last-system-test-result (system-name system-version)
  (select-first-matching-instance (instance system-test-result)
    (where (and (equal (system-name-of instance) (string-downcase system-name))
                (equal (system-version-of instance) (string-downcase system-version))))
    (order-by :descending (run-at-of instance))))

(def function map-pathnames-recursively (pathname function)
  (labels ((recurse (pathname)
             (when (funcall function pathname)
               (unless (pathname-name pathname)
                 (foreach #'recurse (directory (merge-pathnames pathname "*.*")))))))
    (recurse pathname)))

(def function system-write-timestamp (system-name system-version)
  (declare (ignore system-version))
  (bind ((universal nil))
    (map-pathnames-recursively (system-pathname system-name)
                               (lambda (pathname)
                                 (if (pathname-name pathname)
                                     (setf universal (max (or universal 0) (file-write-date pathname)))
                                     (not (member (last-elt (pathname-directory pathname)) '("_darcs" ".git") :test #'equal)))))
    (local-time:universal-to-timestamp universal)))

(def function register-timer-entry/periodic-standalone-test (timer)
  (bind ((name "Standalone test")
         (first-time (local-time:adjust-timestamp (local-time:now)
                       (offset :day 1)
                       (set :hour 0)
                       (set :minute 0)
                       (set :sec 0)
                       (set :nsec 0))))
    (register-timer-entry timer
                          (named-lambda standalone-test ()
                            (bordeaux-threads::make-thread (lambda ()
                                                             (with-model-database
                                                               (standalone-test-hu.dwim-systems :live)
                                                               #+nil
                                                               (standalone-test-hu.dwim-systems :head)))
                                                           :name name))
                          :run-at first-time
                          :interval +seconds-per-day+
                          :name name)))
