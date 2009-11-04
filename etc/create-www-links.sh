#!/bin/sh

cd `dirname "$0"`/../www/

rm -i wui dojo amCharts

ln -sf ../../hu.dwim.wui/www/wui/ .
ln -sf ../../hu.dwim.wui/www/dojo* .
ln -sf ../../hu.dwim.wui/www/amCharts/ .
