#!/bin/sh

. /opt/hu.dwim.home/workspace/hu.dwim.home/bin/env.sh

if [ ! -e "$CORE_FILE" ]; then
  echo
  echo "ERROR: '$CORE_FILE' doesn't exist, try bin/build-image!"
  exit -1
fi

cd "$PROJECT_HOME"

echo
echo "*** Starting ebr42 from $PROJECT_HOME"
echo "*** with sbcl from $SBCL_HOME"
echo

echo `date` - normal >>${LOG_DIRECTORY}/start.log

while true; do

  ${PROJECT_HOME}/bin/run-sbcl.sh ${SBCL_HOME} --core ${PROJECT_HOME}/../../${PROJECT_NAME}.core --dynamic-space-size ${DYNAMIC_SPACE_SIZE} --lose-on-corruption --end-runtime-options --no-userinit --no-sysinit --end-toplevel-options >>/var/log/${PROJECT_NAME}/stdout.log 2>&1 $*
  if [ "$?" -ne "0" ]; then
    echo `date` - exited abnormally, with exit code $? >>${LOG_DIRECTORY}/start.log
    # TODO start it from 1 and make it grow with each restart until an upper bound
    sleep 5;
  else
    echo `date` - exited normally, restarting >>${LOG_DIRECTORY}/start.log
    sleep 1;
  fi

done
