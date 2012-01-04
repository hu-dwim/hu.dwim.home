#!/bin/bash

# invoked by the postgresql process to periodically archive its WAL files as configured per postgresql.conf

if [[ `id --user --name` != "postgres" || `id --group --name` != "postgres" ]]; then
    echo "Aborting because not run as postgres:postgres effective user/group" >&2
    exit 1
fi

umask 0027

. "`dirname $0`/environment.sh"

LOG_FILE=${DWIM_LOG_DIRECTORY}/backup.log

WAL_FILE=$1
WAL_FILE_NAME=$2

WAL_BACKUP_DIR=${DWIM_BACKUP_DIRECTORY}/database/wal
WAL_BACKUP_FILE=${WAL_BACKUP_DIR}/${WAL_FILE_NAME}.tar.bz2

mkdir --mode u=rwx,g=rwxs,o= --parents ${WAL_BACKUP_DIR}

if [ ! -f ${WAL_BACKUP_FILE} ]; then
    if tar --bzip2 --create --file ${WAL_BACKUP_FILE} ${WAL_FILE}; then
        echo `date` - Archived WAL file ${WAL_BACKUP_FILE}. >>${LOG_FILE}
    else
        echo `date` - FATAL: failed to archive WAL file ${WAL_BACKUP_FILE}! >>${LOG_FILE}
    fi
else
    echo `date` - FATAL: WAL file ${WAL_BACKUP_FILE} already exists?! >>${LOG_FILE}
fi
