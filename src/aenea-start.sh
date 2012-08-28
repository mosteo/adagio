#!/bin/bash

LOG=/home/mosteo/bin/aenea/aenea-runs.log
AENEA=/home/mosteo/bin/aenea/aenea

{ ps -u mosteo | grep aenea | grep -v grep  > /dev/null ; } || { $AENEA & echo "Launching Aenea..." `date` >> LOG ; }
