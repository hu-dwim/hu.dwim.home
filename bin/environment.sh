#!/bin/sh

export PROJECT_NAME="hu.dwim.home"
export DATABASE_NAME="hu.dwim.home"
export DATABASE_USER_NAME="hu.dwim.home"
export DATABASE_HOST="localhost"
export DATABASE_PORT="5432"
export DYNAMIC_SPACE_SIZE="500"

export INSTALL_PATH="/opt/${PROJECT_NAME}"
. ${INSTALL_PATH}/workspace/hu.dwim.environment/etc/service-scripts/service-environment.sh
