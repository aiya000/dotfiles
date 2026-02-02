#!/bin/bash
i-have xsel && alias pbcopy='xsel --clipboard --input'

if i-have xdg-open ; then
  alias x=xdg-open
fi
