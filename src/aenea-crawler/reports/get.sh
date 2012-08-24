#!/bin/bash

# Generate via puf all the pages that the crawler can report

BASE=$HOME/tmp/g2reports/html
ADDRESS=http://name:word@trillinux.hopto.org:26666
#OPT='-pr++ -na -nd'  # This gets dependencies
OPT='-na -nd'   # This gets just the pages
puf=/usr/bin/puf

if [ ! -d $BASE ]; then
   echo Copying archives...
   mkdir -p $BASE
   cp `dirname $0`/send-ftp-html.sh $BASE
fi

cd $BASE
$puf $OPT $ADDRESS/index.html
$puf $OPT $ADDRESS/history.html
$puf $OPT "$ADDRESS/countries2.html?orden=1&sentido=false"
$puf $OPT "$ADDRESS/versions.html?orden=2&sentido=false"
$puf $OPT "$ADDRESS/vendors.html?orden=2&sentido=false"
$puf $OPT "$ADDRESS/uptimes.html?orden=1&sentido=false"
$puf $OPT "$ADDRESS/leaves.html?orden=1&sentido=false"
$puf $OPT $ADDRESS/raw.html
# $puf $OPT $ADDRESS/events.html
# $puf $OPT $ADDRESS/trace.html
$puf $OPT $ADDRESS/status.html
$puf $OPT $ADDRESS/estilo.css

mv -f countries2* countries.html
mv -f versions*orden* versions.html
mv -f vendors*orden* vendors.html
mv -f uptimes*orden* uptimes.html
mv -f leaves*orden* leaves.html

$BASE/send-ftp-html.sh
