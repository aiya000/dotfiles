let s:V = vital#vimrc#new()

let s:List = s:V.import('Data.List')
let s:Msg = s:V.import('Vim.Message')

" Clone dein.vim to target dir
function! vimrc#fetch_dein(install_dirname) " {{{
    if executable('git')
        echo 'dein.vim was not installed yet.'
        echo 'Installing dein.vim now.'
        execute '!git clone https://github.com/Shougo/dein.vim' a:install_dirname
    else
        call vimrc#echo_error('Sorry, You do not have git command.')
        call vimrc#echo_error('I cannot introduce dein.vim.')
        throw 'FALIED: cloning deim.vim failed.'
    endif
endfunction " }}}

" Absorbs the different of Vim and NeoVim.
"   open_mode: 'vertical' | 'horizontal' | 'stay' | 'tabnew'
function! vimrc#open_terminal_as(filetype, open_mode, command, ...) abort " {{{
  let options = get(a:000, 1, {})
  let terminal
    \ = has('nvim') && (a:open_mode ==# '++open') ? s:terminal_with_warn()
    \ : has('nvim') ? ':terminal'
    \ : (a:open_mode ==# '++open') ? ':terminal ++curwin ++close ++open'
    \ : get(options, 'noclose', v:false) ? ':terminal ++curwin'
    \ : ':terminal ++curwin ++close'

  if a:open_mode ==# 'vertical'
    vnew
  elseif a:open_mode ==# 'horizontal'
    new
  elseif a:open_mode ==# 'stay'
    enew!
  elseif a:open_mode ==# 'tabnew'
    tabnew
  elseif a:open_mode ==# '++open'
    " :D
  else
    throw 'undefined open_mode is detected: ' . string(a:open_mode)
  endif

  execute ':lcd' get(options, 'path', getcwd())
  execute terminal a:command
  if a:filetype !=# ''
    execute 'setf' a:filetype
  endif

  if !has('nvim')
    " TODO: Use TermOpen event in .vimrc after vim implements
    " TODO: for any registers
    nnoremap <buffer><expr> p vimrc#keys#put_as_stdin(@")
    nnoremap <buffer><expr> "+p vimrc#keys#put_as_stdin(@+)
    nmap <buffer> 'p "+p
  endif
endfunction " }}}

function! s:terminal_with_warn() abort
  call s:Msg.warn('throw ++open is not available for NeoVim, do :terminal without ++open instead')
  return ':terminal'
endfunction
