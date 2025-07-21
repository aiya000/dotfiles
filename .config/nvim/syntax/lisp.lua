-- Lisp/Clojure syntax extensions
local vimrc = vim.g.vimrc or {}
local clojure_syntax_file = (vimrc['vim_home'] or '') .. '/bundle/repos/github.com/thinca/vim-ft-clojure/syntax/clojure.vim'

if vim.fn.filereadable(clojure_syntax_file) == 1 then
  vim.cmd('source ' .. clojure_syntax_file)
else
  vim.notify('syntax/clojure.vim was not found', vim.log.levels.WARN)
end