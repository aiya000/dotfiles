vim.cmd('let s:V = vital#vimrc#new()')
vim.cmd("let s:JSON = s:V.import('Web.JSON')")
vim.cmd("let s:Job = s:V.import('System.Job')")

vim.opt_local.ts = 4
vim.opt_local.sw = 4
vim.opt_local.et = true
vim.opt_local.conceallevel = 0
vim.opt.commentstring = ' -- %s'
vim.opt.errorformat = "'%f:%l:%c: %m' \" for the below :ElmReportErrors"

-- nnoremap <buffer><silent> <localleader>r :<C-u>ElmMake<CR>
vim.keymap.set('n', 'r', function()
  vim.cmd("<C-u>ElmReportErrors <C-r>=expand('%')<CR>")
end, { buffer = true, silent = true })
vim.keymap.set('n', 'S', function()
  vim.cmd("<C-u>Aref elm-search <C-r>=expand('<cword>')<CR>")
end, { buffer = true, silent = true })

local augroup_FtpluginElm = vim.api.nvim_create_augroup('FtpluginElm', { clear = true })
vim.api.nvim_create_autocmd('BufWritePre', {
  group = augroup_FtpluginElm,
  pattern = '*.elm',
  callback = function()
    vim.cmd('ElmFormat')
  end,
})

-- ElmReportErrors {{{

vim.cmd('command! -bar -nargs=1 ElmReportErrors call s:start_quickfix(<q-args>)')

vim.cmd([[
function! s:aggregate_output(data) abort dict
  let self.output[-1] .= a:data[0]
  call extend(self.output, a:data[1:])
endfunction
]])

vim.cmd([[
function! s:show_quickfix(exit_code) abort dict
  let output = join(self.output, '')
  try
    let errors = s:JSON.decode(output)
  catch E121
    " If no errors are gotten
    caddexpr output
    copen
    execute 'normal!' "\<C-w>p"
    return
  endtry

  for error in errors
    let msg = printf('%s %s - %s', error.type, error.tag, error.details)
    caddexpr printf('%s:%d:%d: %s', error.file, error.region.start.line, error.region.start.column, msg)
  endfor
  copen
  execute 'normal!' "\<C-w>p"
endfunction
]])

vim.cmd([[
function! s:start_quickfix(file) abort
  CClear " clear the old report
  call s:Job.start(['elm-make', '--report', 'json', a:file], {
    \ 'output': [''],
    \ 'on_stdout': function('s:aggregate_output'),
    \ 'on_stderr': function('s:aggregate_output'),
    \ 'on_exit': function('s:show_quickfix'),
  \ })
endfunction
]])

-- }}}
