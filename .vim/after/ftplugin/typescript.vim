let b:undo_ftplugin = 'setlocal ' .. join([
  \ 'commentstring<',
  \ 'errorformat<',
  \ 'makeprg<',
  \ 'omnifunc<',
\ ])

let &commentstring = ' // %s'

" .vimrcでも同じ内容を設定しているものの、なぜかemptyになるので、設定
setlocal omnifunc=lsp#complete

function! s:run_script(subcmd, errorformat) abort
  let &errorformat = a:errorformat

  " Use git root dir if available to support bun workspace
  try
    const project_root = vimrc#read_node_root_dir(expand('%:p:h'))
  catch
    const project_root = expand('%:p:h')
  endtry
  const manager = vimrc#check_node_project_manager(project_root)
  if manager !=# v:null
    let &makeprg = $'{manager} run {a:subcmd}'
    " .. ' | sed -r ''s/\x1B\[([0-9]{1,2}(;[0-9]{1,2})?)?[m|K]//g''' " sed to remove ansi colors
  endif

  " echo $':AsyncRun -cwd={project_root} {&makeprg}'
  execute ':AsyncRun' $'-cwd={project_root}' &makeprg
  copen
endfunction

function! s:run_typecheck() abort
  call s:run_script('typecheck', '%f(%l\,%c): %m')  " tsc's errorformat
endfunction

" eslintは逐次出力をせずに、最後に一気に出力するので、焦らずに待とう
function! s:run_lint() abort
  call s:run_script('lint', 'TODO: efm-langserverが使える？')
endfunction

function! s:run_dev(subcmd) abort
  const subcmd = a:subcmd ==# ''
    \ ? 'dev:local'
    \ : a:subcmd
  " TODO: eslintのエラーフォーマットもサポートできる？
  call s:run_script(subcmd, '%f(%l\,%c): %m')  " tsc's errorformat
endfunction

function! s:run_all() abort
  call s:run_typecheck()
  call s:run_lint()
endfunction

command! -bar RunTypeCheck call s:run_typecheck()
command! -bar RunLint call s:run_typecheck() | copen
command! -bar RunAll call s:run_all()
" TODO: Ansi Colorが削除できない
" command! -bar -nargs=? Make call s:run_dev(<q-args>)
command! -bar Make RunAll

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
