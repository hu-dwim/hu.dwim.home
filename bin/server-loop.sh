#!/bin/sh

# you can use this file to start the service from the shell the same way as when the daemon is started, but it's not part of the daemon startup

. `dirname "$0"`/environment.sh

sh ${DWIM_INSTALL_PATH}/workspace/hu.dwim.environment/etc/service-scripts/server-loop.sh --database-host ${DWIM_DATABASE_HOST} --database-port ${DWIM_DATABASE_PORT} --database-user-name ${DWIM_DATABASE_USER_NAME} --database-name ${DWIM_DATABASE_NAME} --database-password ${DWIM_DATABASE_PASSWORD} --pid-file ${SBCL_PIDFILE} --swank-port ${DWIM_SWANK_PORT} --http-server-port ${DWIM_HTTP_SERVER_PORT}
