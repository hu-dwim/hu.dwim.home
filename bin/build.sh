#!/bin/sh
#| -*- mode: lisp; coding: utf-8-unix -*-

# this script assumes SBCL at multiple places. ideally it should use
# cl-launch, be see Fare's thoughts below (from around 2014):
#
# Quick answer: cl-launch could help indeed, though it might need some love.
#
# A few things cl-launch 4 will do for you:
# * abstract over implementation. More useful if you're on Windows or
# ARM, where SBCL support is not as good and you'll want CCL instead.
# * manage the painful loading of asdf and uiop and quicklisp — note
# that any version of ASDF good enough for cl-launch 4 will have UIOP.
# * let you easily specify --system hu.dwim.logger --system
# maru+hu.dwim.logger --system swank --file setup.lisp, etc.
# * (via UIOP) setup the debugger hooks, portably.
#
# Things it won't do (yet):
# * support a non-standard quicklisp installation rather than the
# builtin ~/quicklisp or ~/.quicklisp. It's a SMOP to add (see e.g. how
# the source-registry is handled), but still has to be done. I believe
# ${x#*=} is a standard enough shell construct that one could give an
# argument to --quicklisp with --quicklisp=foo.
# * same for output translation — though you can already export the
# environment variable.

. `dirname "$0"`/environment.sh

SCRIPT_DIR=`dirname "$0"`
SCRIPT_DIR=`readlink -f ${SCRIPT_DIR}`

LISP=${SCRIPT_DIR}/../../sbcl/run-sbcl.sh
LISP=`readlink -f ${LISP}`

cd "${SCRIPT_DIR}"

echo "*** "`date`" Building '${DWIM_PROJECT_NAME}' from workspace '${DWIM_WORKSPACE}'"

BUILD_LOG_FILE="${DWIM_EXECUTABLE_CORE_FILE}.build-log"

export CL_SOURCE_REGISTRY="(:source-registry (:also-exclude \"sbcl\") (:tree \"${DWIM_WORKSPACE}\") :ignore-inherited-configuration)"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${DWIM_WORKSPACE}\" (\"${DWIM_INSTALL_PATH}/.cache/common-lisp/\" :implementation)) :ignore-inherited-configuration)"

# "call" the lisp part below
exec ${LISP} --dynamic-space-size "${DWIM_MAXIMUM_MEMORY_SIZE}" --end-runtime-options --no-sysinit --no-userinit --script "$0" --end-toplevel-options 2>&1 | tee ${BUILD_LOG_FILE}

chown ${DWIM_DAEMON_USER}:${DWIM_DAEMON_USER} "${DWIM_EXECUTABLE_CORE_FILE}.new"
chmod o-rwx "${DWIM_EXECUTABLE_CORE_FILE}.new"

echo "*** "`date`" Finished building ${DWIM_PROJECT_NAME}, executable should be at ${DWIM_EXECUTABLE_CORE_FILE}.new"

# let's quit the shell part before the shell interpreter runs on the lisp stuff below
kill -INT $$

# and from here follows the lisp part that gets "called" above |#

(in-package :cl-user)

(require :asdf)

(let ((output-translations (uiop:getenv "ASDF_OUTPUT_TRANSLATIONS")))
  (assert output-translations)
  (defun hu.dwim.home/set-output-translations-hook ()
    (asdf:initialize-output-translations output-translations)))

;; WARNING: this is fragile as is, because the UIOP API doesn't state it clearly that hooks registered later will be called later, but
;; we need that behavior for shadowing the default UIOP behavior of setting the output-translations to the user's home.
(uiop:register-image-restore-hook 'hu.dwim.home/set-output-translations-hook)

(defmethod asdf:output-files ((o asdf:program-op) (s (eql (asdf:find-system :hu.dwim.home))))
  (let ((exe-path (uiop:getenv "DWIM_EXECUTABLE_CORE_FILE")))
    (if exe-path
        (values (list (concatenate 'string exe-path ".new")) t)
        (call-next-method))))

(asdf:operate 'asdf:program-op :hu.dwim.home)

;; this is dead man's land here on implementations like SBCL that can only SAVE-LISP-AND-DIE where the die part is not optional.
