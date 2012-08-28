#!/bin/bash

~/bin/incp aenea.gz aenea
~/bin/inssh "cp -f aenea/aenea.gz aenea/old/aenea`date +%Y%m%d`.gz && gunzip -f aenea/aenea.gz && bin/launch.sh" &
