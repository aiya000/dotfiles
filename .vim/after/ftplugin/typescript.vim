let b:undo_ftplugin = 'setl ' . join([
  \ 'commentstring<',
  \ 'errorformat<',
  \ 'expandtab<',
  \ 'shiftwidth<',
  \ 'tabstop<',
\ ])

setl tabstop=2 shiftwidth=2 expandtab
let &commentstring = '  // %s'
let &errorformat = '%f(%l\,%c): %m'  " tsc

nnoremap <buffer><silent> <localleader><localleader>r :<C-u>QuickfixRunYarn build<CR>
nnoremap <buffer><silent> <localleader><localleader>w :<C-u>call <SID>start_quickfix()<CR>
nnoremap <buffer><silent> <localleader><localleader>W :<C-u>call <SID>stop_quickfix()<CR>
nmap <silent><buffer> <C-l> <C-[>:syntax sync fromstart<CR>

" TODO: Enable this if needed
" nnoremap <buffer> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('', 'horizontal', 'yarn build', {'path': g:vimrc.path_at_started, 'noclose': v:true})<CR>

augroup FtpluginTypeScript
  autocmd!
  autocmd BufWritePost *.ts,*.tsx call s:exec_quickfix_if_available()
augroup END

function! s:start_quickfix() abort
  let s:does_quickfix_watch = v:true
  QuickfixRunYarn build
endfunction

function! s:stop_quickfix() abort
  let s:does_quickfix_watch = v:false
  cclose
endfunction

function! s:exec_quickfix_if_available() abort
  if get(s:, 'does_quickfix_watch', v:false)
    QuickfixRunYarn build
  endif
endfunction
