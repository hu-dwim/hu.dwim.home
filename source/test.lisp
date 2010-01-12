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
  (bind ((system-name (asdf::coerce-name system-name))
         (system (find-system system-name))
         (test-system (find-system (system-test-system-name system)))
         (root-test-name (find-symbol "TEST" (system-package-name test-system)))
         (result (system-test-result test-system))
         (failures (hu.dwim.stefil::failure-descriptions-of result))
         (system-test-result (make-instance 'system-test-result
                                            :system-name system-name
                                            :system-version system-version
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

(def (function e) standalone-test-system (system-name system-version)
  (home.info "Standalone test for system ~A started" system-name)
  ;; TODO: system-version should be either Live or Head, set up asdf central registry accordingly
  (bind ((run-at (local-time:now))
         (output-path (ensure-directories-exist (pathname (format nil "/tmp/test/~A/~A/" (string-downcase system-name) run-at))))
         (test-program `(progn ; NOTE: forms will be read and evaluated one after the other
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
    (format *debug-io* "; Running standalone test for system ~A with the following arguments (copy to try):~%~{~S ~}~%" system-name shell-arguments)
    (bind ((process (sb-ext:run-program "/bin/sh" shell-arguments
                                        :environment (remove nil (list* (when sbcl-home
                                                                          (concatenate 'string "SBCL_HOME=" sbcl-home))
                                                                        (sb-ext:posix-environ)))
                                        :wait #t)))
      (if (zerop (sb-ext::process-exit-code process))
          (home.info "Standalone test for system ~A finished" system-name)
          (progn
            (with-transaction
              (make-instance 'system-test-result
                             :system-name system-name
                             :system-version system-version
                             :run-at run-at
                             :test-name nil
                             :test-result :fail
                             :compile-output "Failed"
                             :load-output "Failed"
                             :test-output "Failed"))
            (home.warn "Standalone test for system ~A failed" system-name))))))

(def (function e) standalone-test-hu.dwim-systems ()
  (iter (for (name specification) :in-hashtable asdf::*defined-systems*)
        (for system = (cdr specification))
        (for system-name = (asdf::component-name system))
        (when (and (typep system 'hu.dwim.asdf:hu.dwim.system)
                   (find-system (system-test-system-name system) nil))
          (iter (for system-version :in '("Live" "Head"))
                (standalone-test-system system-name system-version)))))

(def (function e) select-latest-system-test-result (system-name system-version)
  (select-instance (instance system-test-result)
    (where (and (equal (system-name-of instance) system-name)
                (equal (system-version-of instance) system-version)))
    (order-by :descending (run-at-of instance))))
