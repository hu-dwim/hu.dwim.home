#!/bin/sh

# the canonical version of this file is hu.dwim.environment/etc/service-scripts/server-loop.sh
# if you edit then make sure you edit the canonical version!

. `dirname "$0"`/environment.sh

# intention: make pwd something specific
cd /tmp/

umask 0002

if [ ! -e "$EXECUTABLE_CORE_FILE" ]; then
  echo
  echo "ERROR: '$EXECUTABLE_CORE_FILE' doesn't exist, try bin/build.sh!"
  exit -1
fi

echo
echo "*** Starting $PROJECT_NAME from $EXECUTABLE_CORE_FILE"
echo

echo `date` - server loop started >>${LOG_DIRECTORY}/start.log

# this doesn't work as expected... trap "echo \`date\` - server loop exiting >>${LOG_DIRECTORY}/start.log" exit

while true; do

  # TODO reinstate --dynamic-space-size and --lose-on-corruption. original was: ${EXECUTABLE_CORE_FILE} --dynamic-space-size ${DYNAMIC_SPACE_SIZE} --lose-on-corruption --end-runtime-options --no-userinit --no-sysinit --end-toplevel-options >>/var/log/${PROJECT_NAME}/standard-output.log 2>&1 $*
  ${EXECUTABLE_CORE_FILE} >>/var/log/${PROJECT_NAME}/standard-output.log 2>&1 $*
  if [ "$?" -ne "0" ]; then
    echo `date` - abnormal exit with code $?, restarting >>${LOG_DIRECTORY}/start.log
    # TODO start sleeping from 1 sec and make it grow with each restart until an upper bound
    sleep 5;
  else
    echo `date` - normal exit, restarting >>${LOG_DIRECTORY}/start.log
    sleep 1;
  fi

done
