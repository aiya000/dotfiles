#!/bin/env zsh

activity=$(systemctl status bluetooth | egrep '^\s+Active:' | awk '{print $2}')

if [[ $activity != 'active' ]] ; then
    echo 'inactive'
    exit
fi

is_power_on=$(bluetoothctl show | egrep '^\s+Powered:' | awk '{print $2}')
if [[ $is_power_on != 'yes' ]] ; then
    echo 'OFF'
    exit
fi

connected_devices=$(bluetoothctl info | egrep '^\s+Name:' | awk '{ for (i=2; i<NF; ++i) { printf("%s ", $i) } print $NF }')
if [[ $connected_devices = '' ]] ; then
    echo 'ON'
    exit
fi
echo $connected_devices
