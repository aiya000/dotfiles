let s:V = vital#vimrc#new()

let s:Msg = s:V.import('Vim.Message')

" Clone dein.vim to target dir
function! vimrc#fetch_dein(install_dirname)
    if executable('git')
        echo 'dein.vim was not installed yet.'
        echo 'Installing dein.vim now.'
        execute '!git clone https://github.com/Shougo/dein.vim' a:install_dirname
    else
        call s:Msg.error('Sorry, You do not have git command.')
        call s:Msg.error('I cannot introduce dein.vim.')
        throw 'FALIED: cloning deim.vim failed.'
    endif
endfunction

" NOTE: open_mode 'open' ってなんだっけ？
" Absorbs the different of Vim and NeoVim.
"   open_mode: 'vertical' | 'horizontal' | 'stay' | 'tabnew' | 'hidden' | 'open'
function! vimrc#open_terminal_as(filetype, open_mode, command, ...) abort
  let options = get(a:000, 0, {})
  let terminal =
    \ (has('nvim') && !s:is_supported_by_neovim(a:open_mode))
      \ ? s:terminal_with_warn(a:open_mode)
    \ : has('nvim')
      \ ? ':terminal'
    \ : (a:open_mode ==# 'hidden')
      \ ? ':terminal ++hidden'
    \ : (a:open_mode ==# 'open')
      \ ? ':terminal ++curwin ++close ++open'
    \ : get(options, 'noclose', v:false)
      \ ? ':terminal ++curwin'
      \ : ':terminal ++curwin ++close'

  if a:open_mode ==# 'vertical'
    vnew
  elseif a:open_mode ==# 'horizontal'
    new
  elseif a:open_mode ==# 'stay'
    enew!
  elseif a:open_mode ==# 'tabnew'
    tabnew
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
endfunction

function! s:is_supported_by_neovim(open_mode) abort
  return a:open_mode ==# 'vertical'
    \ || a:open_mode ==# 'horizontal'
    \ || a:open_mode ==# 'stay'
    \ || a:open_mode ==# 'tabnew'
endfunction

function! s:terminal_with_warn(unsupprted_open_mode) abort
  call s:Msg.warn(printf('throw %s is not available for NeoVim, now do `:terminal` with no arguments instead.', a:unsupprted_open_mode))
  return ':terminal'
endfunction
