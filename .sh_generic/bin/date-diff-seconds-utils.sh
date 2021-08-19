#!/bin/bash

function get_fully_qualified_date () {
  local text=$1

  if [[ $text =~ '^\d\d-\d\ ' ]] ; then
    # If a day is specified.
    echo "$this_year-$text"
  else
    # If a day is omited.
    echo "$this_year-$this_month $text"
  fi
}
