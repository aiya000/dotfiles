scriptencoding utf-8

let s:V = vital#vimrc#new()

let s:HTML = s:V.import('Web.HTML')
let s:List = s:V.import('Data.List')
let s:Msg = s:V.import('Vim.Message')
let s:Optional = s:V.import('Data.Optional')

" If list has elem, return v:true
" otherwise return v:false
function! s:contains(list, elem) abort " {{{
  for x in a:list
    if x ==# a:elem
      return v:true
    endif
  endfor
  return v:false
endfunction " }}}

"-------------------"

" Compress continuous space
function! vimrc#keys#compress_spaces() " {{{
  let recent_pattern = @/
  try
    execute 'substitute/\s\+/ /g'
    normal! ==
  finally
    let @/ = recent_pattern
  endtry
  nohlsearch
endfunction " }}}

" Clear all lines end space
function! vimrc#keys#clear_ends_space() " {{{
  let recent_pattern = @/
  let curpos = getcurpos()
  try
    execute '%substitute/\s*\?$//g'
  catch /E486/
    echo 'nothing todo'
  finally
    let @/ = recent_pattern
    call setpos('.', curpos)
  endtry
endfunction " }}}

" Toggle diffthis - diffoff
function! vimrc#keys#toggle_diff() " {{{
  if &diff
    diffoff
    "NOTE: This restores a [c and ]c keymaps of .vimrc,
    "NOTE: please see [c, ]c, [ale-previous], and [ale-next] at .vimrc.
    nmap <buffer> [c [ale-previous]
    nmap <buffer> ]c [ale-next]
  else
    diffthis
    " Get the origin
    nnoremap <buffer> [c [c
    nnoremap <buffer> ]c ]c
  endif
  set diff?
endfunction " }}}

" If you has nofile buffer, close it.
function! vimrc#keys#bufclose_filetype(filetypes) " {{{
  let closed = 0
  for w in range(1, winnr('$'))
    let buf_ft = getwinvar(w, '&filetype')
    if s:contains(a:filetypes, buf_ft)
      execute ':' . w . 'wincmd w'
      execute ':quit'
      let closed = 1
    endif
  endfor
  return closed
endfunction " }}}

" Toggle open netrw explorer ( vertical split )
function! vimrc#keys#toggle_netrw_vexplorer() " {{{
  let closed = vimrc#keys#bufclose_filetype(['netrw'])
  if !closed
    call vimrc#keys#netrw_wrapper('vertical')
  endif
endfunction " }}}

" Wrap netrw commands to avoid opening failure on :ternimal buffers
function! vimrc#keys#netrw_wrapper(open_method) abort " {{{
  let open =
    \ a:open_method ==# 'stay' ? 'enew' :
    \ a:open_method ==# 'horizontal' ? 'new' :
    \ a:open_method ==# 'vertical' ? 'vertical new' :
    \ a:open_method ==# 'tabnew' ? 'tabnew' :
      \ s:Msg.error(printf("'%s' is not expected", a:open_method))

  let current_dir = fnameescape(getcwd())
  try
    execute ':lcd' fnameescape(getcwd())
    execute open
    Explore
  finally
    execute ':lcd' current_dir
  endtry
endfunction " }}}

" Get a detail of <title> from + register
function! vimrc#keys#get_webpage_title() abort " {{{
  return substitute(s:HTML.parseURL(@+).find('title').value(), '·', '-', 'g')
endfunction " }}}

" :quit if only a window is existent,
" :hide otherwise
function! vimrc#keys#hide_or_quit() abort " {{{
  let tabnum = tabpagenr('$')
  let winnum = tabpagewinnr(tabpagenr(), '$')
  if tabnum is 1 && winnum is 1
    quit
  else
    hide
  endif
endfunction " }}}

" Toggle b:ale_enabled
function! vimrc#keys#toggle_ale_at_buffer() abort " {{{
  let b:ale_enabled = !get(b:, 'ale_enabled', 1)
  " Refresh the state
  ALEToggle
  ALEToggle
endfunction " }}}

" Toggle showing indent-guides with variable
function! vimrc#keys#toggle_indent_guides() " {{{
  let g:vimrc#keys#indent_guides_enable = !get(g:, 'vimrc#keys#indent_guides_enable', v:true)
  IndentGuidesToggle
endfunction " }}}

" Delete a surround `{ _ }` -> ` _ `
function! vimrc#keys#delete_mostly_inner() abort " {{{
  call dein#source('vim-operator-surround')

  let obj_keys = s:get_current_obj_keys()
  call s:Optional.map(s:input_obj_key_on(obj_keys), { obj_key ->
  \ execute('normal ' . ('va' . obj_key . "\<Plug>(operator-surround-delete)"))
  \ })
endfunction " }}}

function! s:get_current_obj_keys() abort " {{{
  let surrounds = g:operator#surround#blocks['-'] + get(g:operator#surround#blocks, &filetype, [])
  let obj_keys = map(surrounds, { _, x -> x.keys })
  return s:List.flatten(obj_keys)
endfunction " }}}

function! s:input_obj_key_on(obj_keys) abort " {{{
  let stroke = ''
  while !s:List.has(a:obj_keys, stroke)
    let char = nr2char(getchar())
    if s:List.has(['', '', ''], char)
      return s:Optional.none()
    endif
    let stroke .= char
  endwhile
  return s:Optional.new(stroke)
endfunction " }}}

" Replace a surround to a surround `{ _ }` -> `[ _ ]`
function! vimrc#keys#replace_mostly_inner() abort " {{{
  call dein#source('vim-operator-surround')

  let obj_keys = s:get_current_obj_keys()
  call s:Optional.map(s:input_obj_key_on(obj_keys), { from ->
  \ s:Optional.map(s:input_obj_key_on(obj_keys), { to ->
  \ execute('normal ' . ('va' . from  . "\<Plug>(operator-surround-replace)" . to))
  \ })
  \ })
endfunction " }}}

" a:visualizer <- 'v' or 'V'
function! vimrc#keys#append_surround(visualizer) abort " {{{
  call dein#source('vim-operator-surround')

  let obj_keys = s:get_current_obj_keys()
  call s:Optional.map(s:input_obj_key_on(obj_keys), { obj_key ->
  \ execute('normal ' . (a:visualizer . "\<Plug>(operator-surround-append)" . obj_key))
  \ })
endfunction " }}}

" Put a regsiter as stdin to the terminal buffer
function! vimrc#keys#put_as_stdin(detail) abort " {{{
  let current_bufnr = bufnr('%')
  call timer_start(0, { _ -> term_sendkeys(current_bufnr, a:detail) }, {'repeat': 1})
  return 'i'
endfunction " }}}

function! vimrc#keys#move_window_forward() " {{{
  let tabwin_num = len(tabpagebuflist())
  mark Z
  hide
  if tabwin_num isnot 1
    tabnext
  endif
  vsp
  normal! 'Z

  if foldlevel('.') > 0
    normal! zO
  endif
  normal! zz
endfunction " }}}

" Current buffer move to next tab
function! vimrc#keys#move_window_backward() " {{{
  mark Z
  hide
  tabprevious
  vsp
  normal! 'Z

  if foldlevel('.') > 0
    normal! zO
  endif
  normal! zz
endfunction " }}}

function! vimrc#keys#move_tab_prev() " {{{
  if tabpagenr() is 1
    $tabmove
  else
    tabmove -1
  endif
endfunction " }}}

function! vimrc#keys#move_tab_next() " {{{
  if tabpagenr() is tabpagenr('$')
    0tabmove
  else
    +tabmove
  endif
endfunction " }}}

function! vimrc#keys#execute_on_base_path(func, ...) abort
  let current = getcwd()
  try
    execute ':lcd' g:vimrc.path_at_started
    echo getcwd()
    call call(a:func, a:000)
  finally
    execute ':lcd' current
  endtry
endfunction

function! vimrc#keys#open_scratch_buffer() abort
  sp ~/.backup/scratch.md
  normal! ggVG"_d
  resize 5
  nnoremap <buffer> ghq :<C-u>w! \| close<CR>
  nnoremap <buffer> ghc :<C-u>w! \| close<CR>
endfunction
