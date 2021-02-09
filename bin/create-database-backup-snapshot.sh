#!/bin/bash

# Create a backup of the running database.
# Write Ahead Log (WAL) directory is simply renamed because the WAL files in it are compressed already.

set -e
umask 007

if [[ `id --user --name` != "postgres" || `id --group --name` != "postgres" ]]; then
    echo "Aborting because not run as postgres:postgres effective user/group" >&2
    exit 1
fi

. "`dirname $0`/environment.sh"

HOST=`hostname --long`
CLUSTER="main"
DATE=`date --rfc-3339 date`
PORT=5432
LOG_FILE=${DWIM_LOG_DIRECTORY}/backup.log
POSTGRESQL_DATA_DIR=/var/lib/postgresql
DATABASE_DIR=${POSTGRESQL_DATA_DIR}/${DWIM_POSTGRESQL_VERSION}/${CLUSTER}

echo `date` - Creating full database backup of $DATABASE_DIR >>${LOG_FILE}

if [ ! -e ${DATABASE_DIR} ]; then
    echo `date` - aborting because database directory does not exist?! $DATABASE_DIR >>${LOG_FILE}
    echo Aborting because database directory does not exist?! $DATABASE_DIR
    exit 1
fi

if [ ! -d "${DWIM_BACKUP_DIRECTORY}/database/" ]; then
    mkdir --mode u=rwx,g=rwxs,o= --parents "${DWIM_BACKUP_DIRECTORY}/database/"
fi

WAL_NEW_NAME=wal-up-to-${DATE}

if [ -d ${WAL_NEW_NAME} ]; then
    echo `date` - FATAL: aborting because `pwd`/${WAL_NEW_NAME} already exists >>${LOG_FILE}
    exit 1
fi

LABEL="${DATE}-${HOST}-${CLUSTER}-${DWIM_POSTGRESQL_VERSION}-backup"
echo `date` "- running pg_start_backup()" >>${LOG_FILE}
ID=`psql -p "$PORT" --tuples-only --command "SELECT pg_start_backup('$LABEL');"`
trap "echo \`date\` '- running pg_stop_backup()' >>${LOG_FILE} && psql -q -p '$PORT' --command 'SELECT pg_stop_backup();'" exit
ID=`echo $ID | sed -e 's/[^a-zA-Z0-9]/_/g'`

cd $POSTGRESQL_DATA_DIR
BACKUP_ARCHIVE="${DWIM_BACKUP_DIRECTORY}/database/${LABEL}-${ID}.tar.bz2"

echo `date` - compressing into ${BACKUP_ARCHIVE} >>${LOG_FILE}

if tar --create --bzip2 --exclude ${DWIM_POSTGRESQL_VERSION}/${CLUSTER}/pg_xlog --file ${BACKUP_ARCHIVE} ${DWIM_POSTGRESQL_VERSION}/${CLUSTER}; then
    echo `date` - tar exited normally, with exit code $? >>${LOG_FILE}
else
    # TODO delme?
    #if [ "$?" = 1 ]; then
    #    echo `date` - tar exited normally, with exit code $? >>${LOG_FILE}
    #else
        echo `date` - FATAL: tar exited with code $? >>${LOG_FILE}
	exit 1
    #fi
fi

cd "${DWIM_BACKUP_DIRECTORY}/database/"

if [ -d wal ]; then
    echo `date` - renaming current backup WAL directory to `pwd`/${WAL_NEW_NAME} >>${LOG_FILE}
    mv wal ${WAL_NEW_NAME}
fi

echo `date` - Full database backup completed successfully >>${LOG_FILE}
