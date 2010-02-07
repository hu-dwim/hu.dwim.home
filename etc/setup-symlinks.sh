#!/bin/sh

cd `dirname "$0"`/../www/

rm -i wui amCharts

ln -sf ../../hu.dwim.wui/www/wui/ .
ln -sf ../../hu.dwim.wui/www/amCharts/ .

cd ../bin/

ln -s ../../hu.dwim.environment/etc/service-scripts/build.sh .
ln -s ../../hu.dwim.environment/etc/service-scripts/server-loop.sh .
