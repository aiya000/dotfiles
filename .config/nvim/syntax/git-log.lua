-- Git log syntax
vim.cmd('runtime syntax/git.vim')

vim.cmd([[
  highlight ftGitLogFixup cterm=reverse ctermfg=white gui=reverse guifg=white
  syntax match ftGitLogFixup 'fixup!'
]])
