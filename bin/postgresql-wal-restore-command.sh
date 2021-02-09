#!/bin/bash

DIR="$1"
FILE="$2"
DESTINATION="$3"
ARCHIVE="${DIR}/${FILE}.tar.bz2"

echo Extracting WAL archive "${ARCHIVE}" to "${DESTINATION}"

rm -rf /tmp/postgres-data-recovery/
mkdir /tmp/postgres-data-recovery/

if tar --directory=/tmp/postgres-data-recovery/ --bzip2 --extract --verbose --file "${ARCHIVE}" ; then
    cp "/tmp/postgres-data-recovery/pg_xlog/${FILE}" "${DESTINATION}"
else
    echo Exiting because archive extraction failed
    exit 1
fi
