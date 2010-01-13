#!/bin/sh

. "`dirname $0`/env.sh"

cd "$PROJECT_HOME"

echo "*** " `date` " Building ${PROJECT_NAME}"

OUTPUT_FILE="${WORKSPACE}"/../hu.dwim.home

sh "${WORKSPACE}"/hu.dwim.build/bin/build.sh --load-swank --production-build --overwrite-output-file --executable-output --toplevel-function hu.dwim.home::executable-toplevel --output-filename "${OUTPUT_FILE}" hu.dwim.home.all

echo "*** " `date` " Finished building ${PROJECT_NAME}, executable is at ${OUTPUT_FILE}"
