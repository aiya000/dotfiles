-- Git status syntax for gin
vim.cmd([[
  highlight StartBranch ctermfg=Green guifg=Green
  syntax match StartBranch /^## [A-Za-z\/\-]\+/hs=s+2

  highlight EndBranch ctermfg=Red guifg=Red
  syntax match EndBranch /\.\.\.[A-Za-z\/]\+/hs=s+3

  highlight StashSize ctermfg=Cyan guifg=Cyan
  syntax match StashSize /stash:[0-9]\+\]/
  " TODO: Fixed highlighting not working for some reason when following below
  " syntax match StashSize /\[stash:[0-9]\+\]/
]])