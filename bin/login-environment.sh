#!/bin/sh

# this file should be included by the bash login script. env.sh is
# split into two because dirname `$0` gets confused when a script is
# sourced using the '.' operator.

export PATH=/opt/hu.dwim.home/workspace/hu.dwim.home/bin:/opt/hu.dwim.home/workspace/hu.dwim.environment/bin:$PATH
. /opt/hu.dwim.home/workspace/hu.dwim.home/bin/env.sh

unset SBCL_HOME
