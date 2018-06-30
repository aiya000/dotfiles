let s:V = vital#vimrc#new()
let s:JSON = s:V.import('Web.JSON')
let s:Job = s:V.import('System.Job')

let b:undo_ftplugin = 'setl ' . join([
  \ 'ts<',
  \ 'sw<',
  \ 'et<',
  \ 'conceallevel<'
\])

setl ts=2 sw=2 et conceallevel=0
let &commentstring = ' -- %s'
let &errorformat = '%f:%l:%c: %m' " for the below :ElmReportErrors

"nnoremap <buffer><silent> <localleader>r :<C-u>ElmMake<CR>
nnoremap <buffer><silent> <localleader><localleader>r :<C-u>ElmReportErrors <C-r>=expand('%')<CR><CR>
nnoremap <buffer><silent> <localleader>S :<C-u>Aref elm-search <C-r>=expand('<cword>')<CR><CR>
nnoremap <buffer><silent> <localleader>o :<C-u>call vimrc#open_terminal_as('term-elm-repl', 'vertical', 'elm repl')<CR>

augroup FtpluginElm
  autocmd!
  autocmd BufWritePre *.elm ElmFormat
augroup END

" ElmReportErrors {{{

command! -bar -nargs=1 ElmReportErrors call s:start_quickfix(<q-args>)

function! s:aggregate_output(data) abort dict
  let self.output[-1] .= a:data[0]
  call extend(self.output, a:data[1:])
endfunction

function! s:show_quickfix(exit_code) abort dict
  let output = join(self.output, '')
  try
    let errors = s:JSON.decode(output)
  catch E121
    " If no errors are gotten
    caddexpr output
    copen
    return
  endtry

  for error in errors
    let msg = printf('%s (%s - %s)', error.type, error.tag, error.details)
    caddexpr printf('%s:%d:%d: %s', error.file, error.region.start.line, error.region.start.column, msg)
  endfor
  copen
endfunction

function! s:start_quickfix(file) abort
  CClear " clear the old report
  call s:Job.start(['elm-make', '--report', 'json', a:file], {
    \ 'output': [''],
    \ 'on_stdout': function('s:aggregate_output'),
    \ 'on_stderr': function('s:aggregate_output'),
    \ 'on_exit': function('s:show_quickfix'),
  \ })
endfunction

" }}}
