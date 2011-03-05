;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;;;
;;;; TODO: use dimensional data when it becomes usable in the user interface

;;;;;;
;;; Persistent test results

(def persistent-member-type test-result :passed :failed :aborted)

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
    ;; FIXME? symbol type makes this slot unreadable in a vm that doesn't have the test package loaded...
    :type (symbol* 128)
    :prefetch #f)
   (test-duration
    :type (or null duration)
    :primary #t)
   (test-result
    :type test-result
    :reference #t
    :primary #t)
   (assertion-count
    :type (or null integer-64)
    :primary #t)
   (failed-assertion-count
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

(def (entity e) individual-test-result (abstract-test-result)
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
   (standard-output
    :type (or null text))
   (standard-error
    :type (or null text))
   (compile-output
    :type (or null text))
   (load-output
    :type (or null text))
   (test-output
    :type (or null text))))

(def association
  ((:slot system-test-result :type (or null system-test-result))
   (:slot individual-test-results :type (set individual-test-result))))

(def icon test-result-passed)

(def icon test-result-failed)

(def icon test-result-aborted)

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
         (root-test-name (find-symbol (system-test-name test-system) (system-package-name test-system)))
         (result (system-test-result test-system))
         (failures (hu.dwim.stefil::failure-descriptions-of result))
         ((&key number-of-assertions number-of-failures number-of-expected-failures number-of-failed-assertions number-of-unexpected-errors &allow-other-keys)
          (hu.dwim.stefil::extract-test-run-statistics result))
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
                                                             :passed
                                                             :failed)
                                            ;; TODO: we don't have that information in the test results, do we?
                                            :assertion-count number-of-assertions
                                            :failed-assertion-count number-of-failed-assertions
                                            :expected-failure-count number-of-expected-failures
                                            :failure-count number-of-failures
                                            :error-count number-of-unexpected-errors)))
    (iter (for (test test-run) :in-hashtable (hu.dwim.stefil::run-tests-of result))
          (make-instance 'individual-test-result
                         :system-test-result system-test-result
                         :test-name (hu.dwim.stefil::name-of test)
                         :test-duration (hu.dwim.stefil::internal-realtime-spent-with-test-of test-run)
                         :test-result (if (zerop (hu.dwim.stefil::number-of-added-failure-descriptions-of test-run))
                                          :passed
                                          :failed)
                         ;; TODO: we don't have that information in the test-run, do we?
                         :assertion-count nil
                         :failed-assertion-count nil
                         :expected-failure-count nil
                         :failure-count (hu.dwim.stefil::number-of-added-failure-descriptions-of test-run)
                         :error-count nil))
    system-test-result))

(def function standalone-test-system/build-lisp-form (system-name system-version output-path &key (disable-debugger #t) (run-at (now)))
  (bind ((forms `(,@(when disable-debugger
                      `((sb-ext::disable-debugger)))
                  (load ,(merge-pathnames "hu.dwim.environment/source/environment.lisp" *workspace-directory*))
                  (format *trace-output* "ASDF source registry after loading environment.lisp is ~S" (asdf::source-registry))
                  ;; (trace asdf:initialize-source-registry)
                  ,@(when (eq system-version :head)
                      ;; alternatively. but it doesn't work as expected, because :inherit-configuration will inherit the default logic, not the current config
                      ;; (initialize-asdf-source-registry #P"/opt/darcs/" :inherit-configuration? #t :insert-at :head)
                      `((initialize-asdf-source-registry '(#P"/opt/darcs/" ,*workspace-directory*))))
                  (format *trace-output* "Final ASDF source registry is ~S" (asdf::source-registry))
                  (asdf:initialize-output-translations '(:output-translations
                                                         (,*workspace-directory* ,output-path)
                                                         (#P"/opt/darcs/" ,output-path)
                                                         :ignore-inherited-configuration))
                  (format *trace-output* "Loading dependencies for system ~S" ',system-name)
                  (map nil 'load-system (collect-system-dependencies ,system-name :transitive #t))
                  (load-system :sb-cover)
                  (declaim (optimize sb-cover:store-coverage-data))
                  (format *trace-output* "Loading system ~S now" ',system-name)
                  (load-system ,system-name)
                  (declaim (optimize (sb-cover:store-coverage-data 0)))
                  (format *trace-output* "Starting the test of system ~S" ',system-name)
                  (test-system ,system-name)
                  (sb-cover:report ,(system-relative-pathname :hu.dwim.home (format nil "www/test/coverage/~A/" (string-downcase system-name))))
                  (format *trace-output* "Loading system ~S" :hu.dwim.home)
                  (load-system :hu.dwim.home)
                  (in-package :hu.dwim.home)
                  (setf (connection-specification-of *model*) ',(connection-specification-of *model*))
                  (store-system-test-result ,system-name ,system-version (parse-timestring ,(with-output-to-string (str) ; so that it's not a simple-base-string, which is not print-readable
                                                                                              (format-timestring str run-at))))
                  (quit 0))))
    (with-standard-io-syntax
      (bind ((*package* (find-package :common-lisp)))
        (write-to-string `(progn
                            ,@(iter (for form :in forms)
                                    ;; because of read-time dependencies we must delay reading every form
                                    (collect `(eval (read-from-string ,(write-to-string form)))))))))))

(def (function e) standalone-test-system (system-name system-version &key force)
  (test.info "Standalone test for ~A ~A started" system-name system-version)
  (if (or force
          (with-transaction
            (bind ((last-test (select-last-system-test-result system-name system-version)))
              (or (not last-test)
                  (eq :aborted (test-result-of last-test))
                  (bind ((last-test-run-at (run-at-of last-test))
                         (last-write-at (system-write-timestamp system-name system-version)))
                    (test.debug "System last modified at ~A, last test run was at ~A" last-write-at last-test-run-at)
                    (timestamp< last-test-run-at last-write-at))))))
      (bind ((run-at (now))
             (output-path (ensure-directories-exist (pathname (format nil "/tmp/test/~A/~A/" (string-downcase system-name) run-at))))
             (sbcl-home (merge-pathnames "sbcl/" *workspace-directory*))
             (shell-arguments `(,(namestring (truename (merge-pathnames "run-sbcl.sh" sbcl-home)))
                                 "--no-sysinit" "--no-userinit"
                                 "--eval" ,(standalone-test-system/build-lisp-form system-name system-version output-path :run-at run-at))))
        (test.debug "Running standalone test for ~A ~A with the following arguments (copy to shell):~%/bin/sh ~{~S ~}~%" system-name system-version shell-arguments)
        (bind ((standard-output-file (merge-pathnames "standard-output.log" output-path))
               (standard-error-file (merge-pathnames "standard-error.log" output-path))
               (process (with-output-to-file (standard-output standard-output-file :if-exists :supersede)
                          (with-output-to-file (standard-error standard-error-file :if-exists :supersede)
                            (sb-ext:run-program "/bin/sh" shell-arguments
                                                :environment (remove nil (list* (concatenate 'string "SBCL_HOME=" (namestring sbcl-home))
                                                                                (sb-ext:posix-environ)))
                                                :output standard-output
                                                :error standard-error
                                                :wait #t))))
               (process-exit-code (sb-ext::process-exit-code process))
               (standard-output (read-file-into-string standard-output-file))
               (standard-error (read-file-into-string standard-error-file)))
          (test.info "Standalone test execution for ~A ~A finished with exit code ~A~%Captured standard error~%~A" system-name system-version process-exit-code standard-error)
          (if (zerop process-exit-code)
              (with-transaction
                (bind ((system-test-result (select-system-test-result system-name system-version run-at)))
                  (setf (standard-output-of system-test-result) standard-output)
                  (setf (standard-error-of system-test-result) standard-error)
                  (test.info "Standalone test for ~A ~A finished" system-name system-version)))
              (with-transaction
                (make-instance 'system-test-result
                               :system-name (string-downcase system-name)
                               :system-version (string-downcase system-version)
                               :run-at run-at
                               :test-name nil
                               :test-result :aborted
                               :standard-output (read-file-into-string standard-output-file)
                               :standard-error (read-file-into-string standard-error-file)
                               :compile-output "Aborted"
                               :load-output "Aborted"
                               :test-output "Aborted")
                (test.warn "Standalone test for ~A ~A aborted" system-name system-version)))
          (iolib.os:delete-files output-path :recursive t)))
      (test.info "Standalone test result for ~A ~A is up to date" system-name system-version)))

(def (function e) select-system-test-result (system-name system-version run-at)
  ;; TODO: take machine-* and implementation-* into account
  (select-instance (instance system-test-result)
    (where (and (equal (system-name-of instance) (string-downcase system-name))
                (equal (system-version-of instance) (string-downcase system-version))
                (timestamp= (run-at-of instance) run-at)))))

(def (function e) select-last-system-test-result (system-name system-version &key (run-at-before +end-of-time+))
  ;; TODO: take machine-* and implementation-* into account
  (select-first-matching-instance (instance system-test-result)
    (where (and (equal (system-name-of instance) (string-downcase system-name))
                (equal (system-version-of instance) (string-downcase system-version))
                (timestamp< (run-at-of instance) run-at-before)))
    (order-by :descending (run-at-of instance))))

(def (function e) collect-hu.dwim-system-names ()
  (sort (iter (for (name specification) :in-hashtable asdf::*defined-systems*)
              (for system = (cdr specification))
              (for system-name = (asdf:component-name system))
              (when (typep system 'hu.dwim.system)
                (collect system-name)))
        #'string<))

;;;;;;
;;; System test result comparison

(def member-type comparison-result :new :same :better :worse :unknown)

(def class* system-test-result-comparison ()
  ((system :type asdf:system)
   (comparison-result :type comparison-result)
   (compared-test-result :type system-test-result)
   (base-test-result :type system-test-result)))

(def icon comparison-result-new)

(def icon comparison-result-same)

(def icon comparison-result-better)

(def icon comparison-result-worse)

(def icon comparison-result-unknown)

(def function %compare-system-test-results (compared-test-result base-test-result)
  ;; TODO: make this correct by looking at each individual test
  (if base-test-result
      (bind ((base-result (test-result-of base-test-result))
             (compared-result (test-result-of compared-test-result))
             (base-failure-count (failure-count-of base-test-result))
             (compared-failure-count (failure-count-of compared-test-result))
             (base-error-count (error-count-of base-test-result))
             (compared-error-count (error-count-of compared-test-result)))
        (flet ((<* (a b)
                 (and a b (< a b)))
               (>* (a b)
                 (and a b (> a b))))
          (cond ((or (and (eq base-result :passed)
                          (eq compared-result :passed))
                     (and (eq base-result :failed)
                          (eq compared-result :failed)
                          (eql base-failure-count compared-failure-count)
                          (eql base-error-count compared-error-count)))
                 :same)
                ((or (and (eq base-result :passed)
                          (eq compared-result :failed))
                     (and (eq base-result compared-result)
                          (or (<* base-failure-count compared-failure-count)
                              (<* base-error-count compared-error-count))))
                 :worse)
                ((or (and (eq base-result :failed)
                          (eq compared-result :passed))
                     (and (eq base-result compared-result)
                          (or (>* base-failure-count compared-failure-count)
                              (>* base-error-count compared-error-count))))
                 :better)
                (t
                 :unknown))))
      :new))

(def function compare-system-test-results (compared-test-result base-test-result)
  (make-instance 'system-test-result-comparison
                 :system (find-system (system-name-of compared-test-result))
                 :comparison-result (%compare-system-test-results compared-test-result base-test-result)
                 :compared-test-result compared-test-result
                 :base-test-result base-test-result))

(def function compare-to-previous-system-test-result (system-test-result)
  (compare-system-test-results system-test-result
                               (select-last-system-test-result (system-name-of system-test-result)
                                                               (system-version-of system-test-result)
                                                               :run-at-before (run-at-of system-test-result))))

;;;;;;
;;; Periodic standalone test

(def function collect-periodic-standalone-test-system-names ()
  (collect-if (lambda (system)
                (awhen (find-system (system-test-system-name (find-system system)) nil)
                  (typep it 'hu.dwim.test-system)))
              (collect-hu.dwim-system-names)))

(def function periodic-standalone-test ()
  (with-layered-error-handlers ((lambda (error)
                                  (bind ((error-message (build-error-log-message :error-condition error
                                                                                 :message "Error reached toplevel in PERIODIC-STANDALONE-TEST"
                                                                                 :timestamp (local-time:now))))
                                    (test.error error-message)
                                    (send-standalone-test-email-message error-message)))
                                (lambda (&key &allow-other-keys)
                                  (return-from periodic-standalone-test)))
    (with-simple-restart (abort "Abort testing")
      (with-model-database
        (dolist (system-name (collect-periodic-standalone-test-system-names))
          (standalone-test-system system-name :head)
          (standalone-test-system system-name :live))
        (with-readonly-transaction
          (send-standalone-test-email-report))))))

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
                          :run-at first-time
                          :interval +seconds-per-day+
                          :name name)))

(def function make-periodic-standalone-test-report (title compared-system-version &optional (base-system-version compared-system-version))
  (make-value-inspector (iter (for system-name :in (collect-periodic-standalone-test-system-names))
                              (for system-test-result = (select-last-system-test-result system-name compared-system-version))
                              (when system-test-result
                                (collect (if (eq compared-system-version base-system-version)
                                             (compare-to-previous-system-test-result system-test-result)
                                             (compare-system-test-results system-test-result (select-last-system-test-result system-name base-system-version))))))
                        :deep-arguments `(:alternatives (sequence/table/inspector (:page-navigation-bar (:page-size ,most-positive-fixnum))))
                        :title (title/widget () title)))

(def layered-method make-reference-content ((component t/reference/inspector) (class standard-class) (prototype asdf:system) (value asdf:system))
  (asdf:component-name value))

(def layered-method make-reference-content ((component t/reference/inspector) (class standard-class) (prototype system-test-result) (value system-test-result))
  (bind (((:read-only-slots test-result run-at assertion-count failed-assertion-count expected-failure-count failure-count error-count) value)
         (localized-run-at (localized-timestamp run-at))
         (localized-test-result (hu.dwim.presentation::localized-member-component-value class (find-slot class 'test-result) test-result)))
    (make-icon/widget (ecase test-result
                        (:passed 'test-result-passed)
                        (:failed 'test-result-failed)
                        (:aborted 'test-result-aborted))
                      :label (ecase test-result
                               (:passed (format nil "~A (~A, ~A) @ ~A" localized-test-result assertion-count expected-failure-count localized-run-at))
                               (:failed (format nil "~A (~A, ~A, ~A, ~A, ~A) @ ~A" localized-test-result assertion-count failed-assertion-count expected-failure-count failure-count error-count localized-run-at))
                               (:aborted (format nil "~A @ ~A" localized-test-result localized-run-at))))))

(def layered-method make-command-bar-commands ((component t/inspector) (class standard-class) (prototype hu.dwim.asdf:hu.dwim.system) (value hu.dwim.asdf:hu.dwim.system))
  (list* (make-test-system-command value) (call-next-layered-method)))

(def icon test-system)

(def function make-test-system-command (system)
  (command/widget ()
    (icon/widget test-system)
    (make-action
      (standalone-test-system (asdf:component-name system) :live :force t)
      (standalone-test-system (asdf:component-name system) :head :force t))))

;;;;;;;
;;; Send email report

(def function send-standalone-test-email-report ()
  (send-standalone-test-email-message (with-active-layers (passive-layer)
                                        (vertical-list/layout ()
                                          (make-periodic-standalone-test-report "Head Repositories (Comparing Last 2 Results)" :head)
                                          (make-periodic-standalone-test-report "Live Repositories (Comparing Last 2 Results)" :live)
                                          (make-periodic-standalone-test-report "Head Compared To Live Repositories" :head :live)))))

(def function send-standalone-test-email-message (component)
  (test.info "Sending standalone test report email")
  ;; TODO: put the mail addresses into the database
  (cl-smtp:send-email "localhost" ;; (hu.dwim.model::mail-relay-host-name-of (cluster-of (cluster-node-of *cluster-node-session*)))
                      "root@dwim.hu" '("admin@dwim.hu")
                      "[dwim.hu] Periodic standalone test results" ""
                      :html-message (with-active-layers (passive-layer)
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
  (bind ((universal nil))
    (map-pathnames-recursively (ecase system-version
                                 (:live
                                  (system-directory system-name))
                                 (:head
                                  (bind ((directory-name (last-elt (pathname-directory (system-directory system-name)))))
                                    (merge-pathnames (pathname (string+ directory-name "/")) #P"/opt/darcs/"))))
                               (lambda (pathname)
                                 (if (pathname-name pathname)
                                     (setf universal (max (or universal 0) (file-write-date pathname)))
                                     (not (member (last-elt (pathname-directory pathname)) '("_darcs" ".git") :test #'equal)))))
    (assert universal)
    (universal-to-timestamp universal)))
