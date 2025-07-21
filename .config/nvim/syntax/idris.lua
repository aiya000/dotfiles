-- Idris syntax extensions
vim.cmd([[
  highlight ftIdrisHeadSpace cterm=underline ctermfg=DarkGray gui=underline guifg=Black
  syntax match ftIdrisHeadSpace '^\s\+'
]])