;;; -*- mode: Lisp; Syntax: Common-Lisp; -*-
;;;
;;; Copyright (c) 2009 by the authors.
;;;
;;; See LICENCE for details.

(in-package :hu.dwim.home)

(def layered-method iterate-possible-authentication-instruments ((application home-application) (identifier string) visitor)
  (bind ((identifier-as-string (string-trim " " identifier)))
    (flet ((try (thing)
             (iter (for instrument :in-sequence (etypecase thing
                                                  (subject (authentication-instruments-of thing))
                                                  (list thing)))
                   (authentication.debug "Trying subject ~A" (subject-of instrument))
                   (funcall visitor instrument))))
      (try *import-technical-subject*) ; TODO delme, only some test code...
      (values #f nil))))
