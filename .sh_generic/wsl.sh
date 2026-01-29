#!/bin/bash

alias x=explorer.exe
alias pbcopy='clip.exe'
alias pbpaste='powershell.exe -command "Get-Clipboard"'

if ! i-have code && i-have Code.exe ; then
  alias code=Code.exe # VS Code
fi

if i-have soundvolumeview ; then
  function soundvolumeview-list () {
    local tmpfile="/tmp/$RANDOM"
    soundvolumeview /stext "$tmpfile"
    cat "$tmpfile" | rg '^Device Name' | sort | uniq
    rm "$tmpfile" > /dev/null
  }

  function soundvolumeview-set-default () {
    : Exected: 'output' or 'input'
    : TODO: $'Currently, if the device of $device_name exists both as output and input, this function sets it to both as output and input. This maybe SoundVolumeView.exe\'s spec.'
    local device_type=$1
    local device_name=${2:-$(soundvolumeview-list | peco | awk '{print $3}')}

    : 0   - 'Console'
    : 1   - 'Multimedia (Speaker)'
    : 2   - 'Communications (Mic)'
    : all - 'Set all default types (Console, Multimedia, and Communications)'
    :
    : See https://www.nirsoft.net/utils/sound_volume_view.html
    case $device_type in
      output) local device_type_number=1 ;;
      input) local device_type_number=2 ;;
      *)
        echo "Unknown device type: $device_type"
        return 1
        ;;
    esac

    soundvolumeview /SetDefault "$device_name" "$device_type_number"
  }

  function soundvolumeview-get-default () {
    : Exected: 'output' or 'input'
    local device_type=$1
    case $device_type in
      output) local device_type_name='Render' ;;
      input) local device_type_name='Capture' ;;
      *)
        echo "Unknown device type: $device_type"
        return 1
        ;;
    esac

    local tmpfile="/tmp/$RANDOM"
    soundvolumeview /sjson "$tmpfile"
    iconv -f UTF-16LE -t UTF-8 "$tmpfile" | jq ".[] | select(.Default == \"$device_type_name\")" | jq '.["Device Name"]' -r
    rm "$tmpfile" > /dev/null
  }
fi
