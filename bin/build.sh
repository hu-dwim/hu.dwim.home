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

export CL_SOURCE_REGISTRY="(:source-registry (:tree \"${DWIM_WORKSPACE}\") (:also-exclude \"sbcl\") :ignore-inherited-configuration)"
export ASDF_OUTPUT_TRANSLATIONS="(:output-translations (\"${DWIM_WORKSPACE}\" (\"${DWIM_INSTALL_PATH}/.cache/common-lisp/\" :implementation)) :ignore-inherited-configuration)"

# "call" the lisp part below
exec ${LISP} --dynamic-space-size "${DWIM_MAXIMUM_MEMORY_SIZE}" --end-runtime-options --no-sysinit --no-userinit --script "$0" --end-toplevel-options 2>&1 | tee ${BUILD_LOG_FILE}

# TODO maybe this should be done by hand anyways...? if so then check the other uses of DWIM_EXECUTABLE_CORE_FILE
#mv ???where-from??? "${DWIM_EXECUTABLE_CORE_FILE}"
#chown ${DWIM_DAEMON_USER}:${DWIM_DAEMON_USER} "${DWIM_EXECUTABLE_CORE_FILE}"
#chmod o-rwx "${DWIM_EXECUTABLE_CORE_FILE}"

echo "*** "`date`" Finished building ${DWIM_PROJECT_NAME}, executable should be at ${DWIM_EXECUTABLE_CORE_FILE}"

# let's quit the shell part before it runs on the lisp stuff below
kill -INT $$

# and from here follows the lisp part that gets launched above |#

(in-package :cl-user)

(require :asdf)

(asdf:operate 'asdf:program-op :hu.dwim.home)
