#!/bin/sh

# this file should be included by the start script(s) of the project

export PROJECT_NAME="hu.dwim.home"
export DATABASE_NAME="hu.dwim.home"
export DATABASE_USER_NAME="hu.dwim.home"
export DATABASE_HOST="localhost"
export DATABASE_PORT="5432"
export DYNAMIC_SPACE_SIZE="500"

export WORKSPACE="/opt/${PROJECT_NAME}/workspace"
export PROJECT_HOME="${WORKSPACE}/${PROJECT_NAME}"
export LOG_DIRECTORY=/var/log/$PROJECT_NAME
export EXECUTABLE_CORE_FILE="$WORKSPACE/../$PROJECT_NAME"
export SBCL_HOME="$WORKSPACE/sbcl/"
export BACKUP_DIRECTORY="/opt/${PROJECT_NAME}/backup/"
