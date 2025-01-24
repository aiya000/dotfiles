#!/bin/bash

: 'Like ./pomodoro-start.sh but starting count $2 from $1'

: Usage example
: pomodoro-start-from.sh 17:00 60

base_dir=$(dirname $0)

from=$1
to=$(date +'%H:%M')  # Now
count=$2
consumed=$($base_dir/date-diff-seconds $from $to)

$base_dir/pomodoro-start.sh $(( $count - $consumed ))
