#!/bin/bash

# Generate the adabrowse docs for all .adt files found (but the main procedure).
echo Cleaning...
rm -f ../htmldocs/${1}/*
mkdir -p ../htmldocs/${1}
echo Finding data...
ls obj_debug/*.adt | grep -v 'main.adt' > obj_debug/adt.txt
echo Generating...
adabrowse -wi -c adabrowse.conf --all --private -f @obj_debug/adt.txt -Tobj_debug -o ../htmldocs/${1}/
