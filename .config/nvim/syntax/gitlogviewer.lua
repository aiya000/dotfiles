-- Git log viewer syntax
vim.cmd('runtime syntax/git.vim')

vim.cmd([[
  highlight ftGitLogViewerFixup cterm=reverse ctermfg=white gui=reverse guifg=white
  syntax match ftGitLogViewerFixup 'fixup!'
]])