#!/bin/bash

casual_secandary_display=0
x=${1:-${casual_secandary_display}}

conky | dzen2 -dock -ta left -w 950 -h 30 -x "$x" &
