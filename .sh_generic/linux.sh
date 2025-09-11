#!/bin/bash
i_have xsel && alias pbcopy='xsel --clipboard --input'

if i_have xdg-open ; then
  alias x=xdg-open
fi
