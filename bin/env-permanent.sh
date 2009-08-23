#!/bin/sh

# this file should be included by the bash login script. env.sh is
# split into two because dirname `$0` gets confused when a script is
# sourced using the '.' operator.

export PATH=/opt/hu.dwim.home/workspace/hu.dwim.home/bin:/opt/hu.dwim.home/workspace/environment/bin:$PATH
. /opt/hu.dwim.home/workspace/hu.dwim.home/bin/env.sh

unset SBCL_HOME

absolutize ()
{
  if [ -d "$1" ]; then
    cd "$1"
    echo `pwd`
    cd - >/dev/null
  else
    echo
    echo "ERROR: '$1' doesn't exist or not a directory"
    exit -1
  fi
}

rmfasl ()
{
  pushd ${WORKSPACE}/../fasls/sbcl*-linux-*/opt/ebr*/ >/dev/null
  for i in $*; do
    for dir in `find . -type d -wholename "*$i*"`; do
      if [ -e "$dir" ]; then
        echo Deleting fasl dir `absolutize "$dir"`
        rm -r "$dir"
      fi
    done
  done
  popd >/dev/null
}
