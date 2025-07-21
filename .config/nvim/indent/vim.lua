if vim.b.did_indent then
  return
end

-- Load default vim indent
vim.cmd('source $VIMRUNTIME/indent/vim.lua')
vim.bo.indentexpr = 'v:lua.GetVimIndentOverwrite()'

function GetVimIndentOverwrite()
  local lnum = vim.fn.prevnonblank(vim.v.lnum - 1)
  
  if not vim.fn.getline(vim.v.lnum):match('^%s*\\') then
    while (lnum > 0) and vim.fn.getline(lnum):match('^%s*\\') do
      lnum = lnum - 1
    end
  end
  
  if lnum == 0 then
    return 0
  end
  
  local ind = vim.fn.indent(lnum)
  if vim.fn.getline(vim.v.lnum):match('^%s*\\') and (vim.v.lnum > 1) and not vim.fn.getline(lnum):match('^%s*\\') then
    if vim.g.vim_indent_cont then
      ind = ind + vim.g.vim_indent_cont
    else
      --ind = ind + vim.bo.sw * 3
    end
  elseif vim.fn.getline(lnum):match('(^|)%s*(if|wh%[ile]|for|try|cat%[ch]|fina%[lly]|fu%[nction]|el%[seif])>') then
    ind = ind + vim.bo.shiftwidth
  elseif vim.fn.getline(lnum):match('^%s*aug%[roup]') and not vim.fn.getline(lnum):match('^%s*aug%[roup]%s*!?%s+END') then
    ind = ind + vim.bo.shiftwidth
  end
  
  local line = vim.fn.getline(lnum)
  local i = line:find('[^\\]|%s*(ene@!)')
  if (i and i > 0) and not line:match('^%s*au%[tocmd]') then
    if not vim.fn.has('syntax_items') or not vim.fn.synIDattr(vim.fn.synID(lnum, i + 2, 1), 'name'):match('(Comment|String)$') then
      ind = ind - vim.bo.shiftwidth
    end
  end
  
  if vim.fn.getline(vim.v.lnum):match('^%s*(ene@!|cat|fina|el|aug%[roup]%s*!?%s+END)') then
    ind = ind - vim.bo.shiftwidth
  end
  
  return ind
end

vim.b.did_indent = true