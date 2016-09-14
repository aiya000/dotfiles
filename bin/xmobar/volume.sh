#!/bin/bash

# See http://pastebin.com/K2G5NpiU
# This script require amixer,
# Please install package.
#  arch: alsa-utils

str=$(amixer sget Master)
str1=${str#Simple*\[}
v1=${str1%%]*]}
echo $v1
