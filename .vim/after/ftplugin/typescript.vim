let b:undo_ftplugin = 'setlocal ' .. join([
  \ 'commentstring<',
  \ 'errorformat<',
  \ 'makeprg<',
\ ])

let &commentstring = ' // %s'
let &errorformat = '%f(%l\,%c): %m'  " tsc

" Use git root dir if available to support bun workspace
try
  let project_root = vimrc#read_git_root_sync()
catch
  let project_root = expand('%:p:h')
endtry
let manager = vimrc#check_node_project_manager(project_root)
if manager !=# v:null
  let &makeprg = $'{manager} typecheck'
endif

command! -bar Make execute ':AsyncRun' &makeprg | copen

" TODO: Check this working and fix this
function! s:run_yarn_quickfix(yarn_subcmd) abort
  const current = getcwd()
  try
    CClear
    const yarn_cmd = ['yarn'] + split(a:yarn_subcmd, ' ')
    execute ':lcd' g:vimrc.path_at_started
    call s:read_to_quickfix_it(yarn_cmd)
    copen
  finally
    execute ':lcd' current
  endtry
endfunction


" TODO: Do this only when lsp started
" To lighten the completion performance
" setl complete-=t

nnoremap <buffer><silent> <localleader><localleader>r :<C-u>QuickfixRunYarn build<CR>
nnoremap <buffer><silent> <localleader><localleader>w :<C-u>call <SID>start_quickfix()<CR>
nnoremap <buffer><silent> <localleader><localleader>W :<C-u>call <SID>stop_quickfix()<CR>
nmap <silent><buffer> <C-l> <C-[>:syntax sync fromstart<CR>

" TODO: Enable this if needed
" nnoremap <buffer> <localleader><localleader>R :<C-u>call vimrc#open_terminal_as('', 'horizontal', 'yarn build', {'path': g:vimrc.path_at_started, 'noclose': v:true})<CR>

" augroup FtpluginTypeScript
"   autocmd!
"   autocmd BufWritePost *.ts,*.tsx call s:exec_quickfix_if_available()
" augroup END
