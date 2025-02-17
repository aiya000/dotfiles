#!/bin/zsh

function contains_value() {
: Example
: <<EOF
  target=banana
  my_array=(apple banana cherry)
  if contains_value \"${my_array[@]}\" \"$target\" ';' then
    echo 'It is in the array'
  else
    echo 'It is not in the array'
  fi
EOF

  local array=("$@")
  local value_to_check="${array[-1]}"
  unset 'array[-1]'

  for element in "${array[@]}"; do
    if [[ $element == $value_to_check ]] ; then
      return 0
    fi
  done
  return 1
}
