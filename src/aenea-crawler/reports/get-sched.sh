#!/bin/bash

GET=$HOME/bin/get.sh

while ((1)); do
   if [ `date +%M` == 00 ]; then
      $GET
      sleep 2m
   fi
   sleep 1s
done
