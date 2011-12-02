;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

;;;;;;
;;; Constants

(def (constant e) +default-database-name+ "hu.dwim.home")
(def (constant e) +default-database-user-name+ "hu.dwim.home")
(def (constant e) +default-database-password+ "engedjbe")

;;;;;;
;;; Database

(def global-variable *home-database* (make-instance 'hu.dwim.meta-model::postgresql/dwim
                                                    :generated-transaction-class-name 'transaction
                                                    :connection-specification '()))

(def with-macro with-transaction/home ()
  (with-database *home-database*
    (with-new-compiled-query-cache
      (with-transaction
        (-body-)))))
