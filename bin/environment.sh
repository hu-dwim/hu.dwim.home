#!/bin/sh

export PROJECT_NAME="hu.dwim.home"
export DATABASE_NAME="hu.dwim.home"
export DATABASE_USER_NAME="hu.dwim.home"
export DATABASE_PASSWORD="engedjbe"
export DATABASE_HOST="localhost"
export DATABASE_PORT="5432"
export HTTP_SERVER_PORT="8080"
export SWANK_PORT="3000"
# currently not in effect... export DYNAMIC_SPACE_SIZE="500"
export DAEMON_USER="home-service"
export TOPLEVEL_FUNCTION="hu.dwim.home::executable-toplevel"
export ASDF_SYSTEM_NAME="hu.dwim.home.all"

export INSTALL_PATH="/opt/${PROJECT_NAME}"

. ${INSTALL_PATH}/workspace/hu.dwim.environment/etc/service-scripts/service-environment.sh
