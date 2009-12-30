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
    :type (text 32))
   (machine-type
    :type (text 32))
   (machine-version
    :type (text 128))
   (lisp-implementation-type
    :type (text 32)
    :primary #t)
   (lisp-implementation-version
    :type (text 32)
    :primary #t)))

(def (entity e) abstract-test-result ()
  ((test-name
    :type (symbol* 128))
   (test-duration
    :type duration
    :primary #t)
   (test-result
    :type (member :pass :fail)
    :reference #t
    :primary #t)
   (assertion-count
    :type integer-64
    :primary #t)
   (success-count
    :type integer-64)
   (expected-failure-count
    :type integer-64)
   (failure-count
    :type integer-64
    :primary #t)
   (error-count
    :type integer-64
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
   (run-at
    :type timestamp
    :primary #t)
   (output
    :type text)))

(def association
  ((:type (or null system-test-result))
   (:type (set test-result))))

(def icon test-result/pass)

(def icon test-result/fail)

(def (function e) store-system-test-result (system-name &optional (connection-specification (connection-specification-of *model*)))
  (setf (connection-specification-of *model*) connection-specification)
  (with-model-database
    (with-new-compiled-query-cache
      (with-transaction
        (%store-system-test-result system-name))))
  (quit 0))

(def function %store-system-test-result (system-name)
  (bind ((system-name (asdf::coerce-name system-name))
         (system (find-system system-name))
         (test-system (find-system (system-test-system-name system)))
         (root-test-name (find-symbol "TEST" (system-package-name test-system)))
         (result (system-test-result test-system))
         (failures (hu.dwim.stefil::failure-descriptions-of result))
         (system-test-result (make-instance 'system-test-result
                                            :system-name system-name
                                            :run-at (now)
                                            :machine-instance (machine-instance)
                                            :machine-type (machine-type)
                                            :machine-version (machine-version)
                                            :lisp-implementation-type (lisp-implementation-type)
                                            :lisp-implementation-version (lisp-implementation-version)
                                            :test-name root-test-name
                                            :test-duration (hu.dwim.stefil::internal-realtime-spent-with-test-of (gethash (hu.dwim.stefil:find-test root-test-name) (hu.dwim.stefil::run-tests-of result)))
                                            :test-result (if (zerop (length failures))
                                                             :pass
                                                             :fail)
                                            :assertion-count (hu.dwim.stefil::assertion-count-of result)
                                            :success-count -1
                                            :expected-failure-count -1
                                            :failure-count (length failures)
                                            :error-count -1
                                            :output (system-test-output test-system))))
    (iter (for (test test-run) :in-hashtable (hu.dwim.stefil::run-tests-of result))
          (for test-result = (make-instance 'test-result
                                            :test-name (hu.dwim.stefil::name-of test)
                                            :test-duration (hu.dwim.stefil::internal-realtime-spent-with-test-of test-run)
                                            :test-result (if (zerop (hu.dwim.stefil::number-of-added-failure-descriptions-of test-run))
                                                             :pass
                                                             :fail)
                                            ;; TODO: we don't have that information in the test-run do we?
                                            :assertion-count -1
                                            :success-count -1
                                            :expected-failure-count -1
                                            :failure-count (hu.dwim.stefil::number-of-added-failure-descriptions-of test-run)
                                            :error-count -1
                                            :system-test-result system-test-result)))
    system-test-result))

(def (function e) standalone-test-system (system-name)
  (home.info "Standalone test for system ~A started" system-name)
  (bind ((setup-environment-program `(load ,(truename (system-relative-pathname :hu.dwim.home "../hu.dwim.environment/source/environment.lisp"))))
         ;; TODO: a fresh build with rmfasl is needed
         (test-system-program `(test-system ,system-name))
         (load-home-program '(load-system :hu.dwim.home))
         (store-system-test-result-program `(hu.dwim.home::store-system-test-result ,system-name ',(hu.dwim.meta-model::connection-specification-of *model*)))
         (sbcl-home (sb-posix:getenv "SBCL_HOME"))
         (shell-arguments (bind ((*package* (find-package :keyword)))
                            `(,(namestring (truename (merge-pathnames "run-sbcl.sh" (pathname sbcl-home))))
                               "--no-sysinit" "--no-userinit"
                               "--eval" ,(format nil "~S" setup-environment-program)
                               "--eval" ,(format nil "~S" test-system-program)
                               "--eval" ,(format nil "~S" load-home-program)
                               "--eval" ,(format nil "~S" store-system-test-result-program)))))
    ;; NOTE: ~S allows copying the forms into a shell
    (format *debug-io* "; Running test with: ~{~S ~}~%" shell-arguments)
    (sb-ext:run-program "/bin/sh" shell-arguments
                        :environment (remove nil (list* (when sbcl-home
                                                          (concatenate 'string "SBCL_HOME=" sbcl-home))
                                                        (sb-ext:posix-environ)))
                        :wait #t))
  (home.info "Standalone test for system ~A finished" system-name))

(def (function e) standalone-test-hu.dwim-systems ()
  (iter (for (name specification) :in-hashtable asdf::*defined-systems*)
        (for system = (cdr specification))
        (when (and (typep system 'hu.dwim.asdf:hu.dwim.system)
                   (find-system (system-test-system-name system) nil))
          (standalone-test-system (asdf::component-name system)))))

(def (function e) select-latest-system-test-result (system-name)
  (select-instance (instance system-test-result)
    (where (eq (system-name-of instance) system-name))
    (order-by :descending (run-at-of instance))))
