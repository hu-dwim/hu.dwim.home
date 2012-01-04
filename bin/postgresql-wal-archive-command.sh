#!/bin/bash

# invoked by the postgresql process to periodically archive its WAL files as configured per postgresql.conf

if [ ! "${USER}" = "postgres" ]; then
    echo "Aborting because not run as postgres user" >&2
    exit 1
fi

umask 0027

. "`dirname $0`/environment.sh"

LOG_FILE=${DWIM_LOG_DIRECTORY}/backup.log

WAL_FILE=$1
WAL_FILE_NAME=$2

WAL_BACKUP_DIR=/opt/hu.dwim.home/backup/database/wal
WAL_BACKUP_FILE=${WAL_BACKUP_DIR}/${WAL_FILE_NAME}.tar.7z

mkdir --mode u=rwx,g=rwxs,o= --parents ${WAL_BACKUP_DIR}

if [ ! -f ${WAL_BACKUP_FILE} ]; then
    if tar --create ${WAL_FILE} | 7z a ${WAL_BACKUP_FILE} -mx3 -si${WAL_FILE_NAME}.tar >/dev/null ; then
        echo `date` - Successfully archived WAL file ${WAL_BACKUP_FILE}. >>${LOG_FILE}
    else
        echo `date` - FATAL: failed to archive WAL file ${WAL_BACKUP_FILE}! >>${LOG_FILE}
    fi
else
    echo `date` - FATAL: WAL file ${WAL_BACKUP_FILE} already exists?! >>${LOG_FILE}
fi
