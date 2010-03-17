#!/bin/sh

export DWIM_PROJECT_NAME="hu.dwim.home"
export DWIM_DATABASE_NAME="hu.dwim.home"
export DWIM_DATABASE_USER_NAME="hu.dwim.home"
export DWIM_DATABASE_PASSWORD="engedjbe"
export DWIM_DATABASE_HOST="localhost"
export DWIM_DATABASE_PORT="5432"
export DWIM_HTTP_SERVER_PORT="8080"
export DWIM_SWANK_PORT="13000"
export DWIM_MAXIMUM_MEMORY_SIZE="1500Mi"
export DWIM_DAEMON_USER="home-service"
export DWIM_TOPLEVEL_FUNCTION="hu.dwim.home::executable-toplevel"
export DWIM_ASDF_SYSTEM_NAME="hu.dwim.home.all"

export DWIM_POSTGRESQL_VERSION="8.4"
export DWIM_INSTALL_PATH="/opt/${DWIM_PROJECT_NAME}"

. ${DWIM_INSTALL_PATH}/workspace/hu.dwim.environment/etc/service-scripts/service-environment.sh
