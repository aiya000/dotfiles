-- Terminal Elm REPL syntax
local vimrc = InitLua or {}
local elm_syntax_file = (vimrc['neovim_home'] or '') .. '/bundle/repos/github.com/ElmCast/elm-vim/syntax/elm.vim'

if vim.fn.filereadable(elm_syntax_file) == 1 then
  vim.cmd('source ' .. elm_syntax_file)
else
  vim.notify(elm_syntax_file .. ' is not found', vim.log.levels.WARN)
end
