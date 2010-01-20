;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;;;
;;;; TODO: use dimensional data when it becomes usable

;;;;;;
;;; Persistent test results

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
;;; Standalone test system

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
            (timestamp< last-test last-write))
        ;; TODO: system-version should be either :live or :head, set up asdf central registry accordingly
        (bind ((run-at (now))
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

(def (function e) select-last-system-test-result (system-name system-version &key (run-at-before +end-of-time+))
  ;; TODO: take machine-* and implementation-* into account
  (select-first-matching-instance (instance system-test-result)
    (where (and (equal (system-name-of instance) (string-downcase system-name))
                (equal (system-version-of instance) (string-downcase system-version))
                (timestamp< (run-at-of instance) run-at-before)))
    (order-by :descending (run-at-of instance))))

(def (function e) collect-hu.dwim-system-names ()
  (iter (for (name specification) :in-hashtable asdf::*defined-systems*)
        (for system = (cdr specification))
        (for system-name = (asdf::component-name system))
        (when (and (typep system 'hu.dwim.asdf:hu.dwim.system)
                   (find-system (system-test-system-name system) nil))
          (collect system-name))))

;;;;;;
;;; System test result comparison

(def class* system-test-result-comparison ()
  ((comparison-result :type (member :new :same :better :worse :unknown))
   (base-test-result :type system-test-result)
   (compared-test-result :type system-test-result)))

(def icon comparison-result-new)

(def icon comparison-result-same)

(def icon comparison-result-better)

(def icon comparison-result-worse)

(def icon comparison-result-unknown)

(def function %compare-system-test-results (base-test-result compared-test-result)
  (bind ((base-result (test-result-of base-test-result))
         (compared-result (test-result-of compared-test-result))
         (base-failure-count (failure-count-of base-test-result))
         (compared-failure-count (failure-count-of compared-test-result))
         (base-error-count (error-count-of base-test-result))
         (compared-error-count (error-count-of compared-test-result)))
    (if base-test-result
        (cond ((or (and (eq base-result :pass)
                        (eq compared-result :pass))
                   (and (eq base-result :fail)
                        (eq compared-result :fail)
                        (eql base-failure-count compared-failure-count)
                        (eql base-error-count compared-error-count)))
               :same)
              ((or (and (eq base-result :pass)
                        (eq compared-result :fail))
                   (and (eq base-result compared-result)
                        (or (< base-failure-count compared-failure-count)
                            (< base-error-count compared-error-count))))
               :worse)
              ((or (and (eq base-result :fail)
                        (eq compared-result :pass))
                   (and (eq base-result compared-result)
                        (or (> base-failure-count compared-failure-count)
                            (> base-error-count compared-error-count))))
               :better)
              (t
               :unknown))
        :new)))

(def function compare-system-test-results (base-test-result compared-test-result)
  (make-instance 'system-test-result-comparison
                 :comparison-result (%compare-system-test-results base-test-result compared-test-result)
                 :base-test-result base-test-result
                 :compared-test-result compared-test-result))

(def function compare-to-previous-system-test-result (system-test-result)
  (compare-system-test-results (select-last-system-test-result (system-name-of system-test-result)
                                                               (system-version-of system-test-result)
                                                               :run-at-before (run-at-of system-test-result))
                               system-test-result))



;;;;;;
;;; Periodic standalone test

(def function collect-periodic-standalone-test-system-names ()
  (collect-hu.dwim-system-names))

(def function periodic-standalone-test ()
  (with-model-database
    ;; TODO: test :head systems too
    (dolist (system-name (collect-periodic-standalone-test-system-names))
      (standalone-test-system system-name :live))
    (with-readonly-transaction
      (send-standalone-test-email-report (make-periodic-standalone-test-report :live)))))

(def function register-timer-entry/periodic-standalone-test (timer)
  (bind ((name "Standalone test")
         (first-time (adjust-timestamp (now)
                       (offset :day 1)
                       (set :hour 0)
                       (set :minute 0)
                       (set :sec 0)
                       (set :nsec 0))))
    (register-timer-entry timer
                          (named-lambda standalone-test ()
                            (bordeaux-threads::make-thread 'periodic-standalone-test :name name))
                          :first-time first-time
                          :time-interval +seconds-per-day+
                          :name name)))

(def function make-periodic-standalone-test-report (system-version)
  (make-value-inspector (iter (for system-name :in (collect-periodic-standalone-test-system-names))
                              (for system-test-result = (select-last-system-test-result system-name system-version))
                              (when system-test-result
                                (collect (compare-to-previous-system-test-result system-test-result))))))

;;;;;;;
;;; Send email report

(def function send-standalone-test-email-report (component)
  ;; TODO: put the mail addresses into the database
  (cl-smtp:send-email "smtp.gmail.com" #+nil (hu.dwim.model::mail-relay-host-name-of (cluster-of (cluster-node-of *cluster-node-session*)))
                      "dwim.hu@gmail.com" '("levente.meszaros@gmail.com" "attila.lendvai@gmail.com" "tomi.borbely@gmail.com" "darabi@web.de")
                      "[dwim.hu] Standalone test results" ""
                      :authentication '("dwim.hu@gmail.com" "engedjbe") :ssl :tls
                      :html-message (with-active-layers (passive-xhtml-layer)
                                      (render-to-xhtml-string component))))

;;;;;;
;;; Utility

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
                          :first-time first-time
                          :time-interval +seconds-per-day+
                          :name name)))
