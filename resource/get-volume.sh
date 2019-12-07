#!/bin/bash

if [[ $(pamixer --get-mute) = 'true' ]] ; then
    echo 'mute'
else
    pamixer --get-volume
fi
