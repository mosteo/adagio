#!/bin/bash

COM=/home/jano/prog/java/com
TEMP=/home/jano/tmp/g2reports/graphs
JOPTIONS='-Xlint:unchecked'
export DISPLAY=:0.0

if [ ! -d $TEMP ]; then
   echo Copying archives...
   mkdir -p $TEMP
   cp -R $COM $TEMP/com
   cp `dirname $0`/Generate.java $TEMP
   cp `dirname $0`/send-ftp-reports.sh $TEMP
fi

export PATH=/usr/local/java/bin:$PATH
export CLASSPATH=/home/jano/prog/java/lib/pg74.1jdbc3.jar:/home/jano/prog/java/lib/jCharts.jar:/home/jano/bin/resin/lib/jsdk-24.jar:.

cd $TEMP
echo Compiling...
javac $JOPTIONS Generate.java
javac $JOPTIONS com/trulisoft/db/*.java
javac $JOPTIONS com/trulisoft/g2/*.java
rm -rf *.png

echo Generating...
java Generate

echo Sending...
$TEMP/send-ftp-reports.sh
