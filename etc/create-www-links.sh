#!/bin/sh
# run this script from the www directory of you hu.dwim.home system

cd `dirname "$0"`/../www/

rm -i wui dojo amCharts

ln -sf ../../hu.dwim.wui/www/wui/ .
ln -sf ../../hu.dwim.wui/www/dojo* .
ln -sf ../../hu.dwim.wui/www/amCharts/ .