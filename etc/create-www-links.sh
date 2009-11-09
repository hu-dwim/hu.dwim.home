#!/bin/sh

cd `dirname "$0"`/../www/

rm -i wui amCharts

ln -sf ../../hu.dwim.wui/www/wui/ .
ln -sf ../../hu.dwim.wui/www/amCharts/ .
