scriptencoding utf-8
scriptversion 3

let s:V = vital#vimrc#new()

let s:HTML = s:V.import('Web.HTML')
let s:Job = s:V.import('System.Job')
let s:List = s:V.import('Data.List')
let s:Msg = s:V.import('Vim.Message')
let s:Optional = s:V.import('Data.Optional')

" Clone dein.vim to target dir
function vimrc#fetch_dein(install_dirname)
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
function vimrc#open_terminal_as(filetype, open_mode, command, ...) abort
  const options = get(a:000, 0, {})
  const terminal =
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
    throw 'undefined open_mode is detected: ' .. string(a:open_mode)
  endif

  execute ':lcd' get(options, 'path', getcwd())
  execute terminal a:command
  if a:filetype !=# ''
    execute 'setf' a:filetype
  endif
endfunction

function s:is_supported_by_neovim(open_mode) abort
  return a:open_mode ==# 'vertical'
    \ || a:open_mode ==# 'horizontal'
    \ || a:open_mode ==# 'stay'
    \ || a:open_mode ==# 'tabnew'
endfunction

function s:terminal_with_warn(unsupprted_open_mode) abort
  call s:Msg.warn(printf('throw %s is not available for NeoVim, now do `:terminal` with no arguments instead.', a:unsupprted_open_mode))
  return ':terminal'
endfunction

" Compress continuous space
function vimrc#compress_spaces()
  const recent_pattern = @/
  try
    execute 'substitute/\s\+/ /g'
    normal! ==
  finally
    let @/ = recent_pattern
  endtry
  nohlsearch
endfunction

" Clear all lines end space
function vimrc#clear_ends_space()
  const recent_pattern = @/
  const curpos = getcurpos()
  try
    execute '%substitute/\s*\?$//g'
  catch /E486/
    echo 'nothing todo'
  finally
    let @/ = recent_pattern
    call setpos('.', curpos)
  endtry
endfunction

" Toggle diffthis - diffoff
function vimrc#toggle_diff()
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
endfunction

function vimrc#bufclose_auto() abort
  call vimrc#bufclose_filetype([
    \ 'aref_web',
    \ 'diff',
    \ 'gina-branch',
    \ 'gina-log',
    \ 'gina-status',
    \ 'gitdiffviewer',
    \ 'gitlogviewer',
    \ 'gitreflogviewer',
    \ 'gitshowviewer',
    \ 'help',
    \ 'man',
    \ 'netrw',
    \ 'dirvish',
    \ 'qf',
    \ 'quickrun',
    \ 'scratch',
    \ 'denite',
    \ 'denite-filter',
  \ ])
endfunction

" If you has nofile buffer, close it.
function vimrc#bufclose_filetype(filetypes)
  let closed = 0
  for w in range(1, winnr('$'))
    let buf_ft = getwinvar(w, '&filetype')
    if s:List.has(a:filetypes, buf_ft)
      execute ':' .. w .. 'wincmd w'
      quit
      let closed = 1
    endif
  endfor
  return closed
endfunction

" Toggle open netrw explorer ( vertical split )
function vimrc#toggle_explorer(...)
  const path = get(a:000, 0, '')
  const closed = vimrc#bufclose_filetype(['dirvish'])
  if !closed
    leftabove vsplit
    execute ':Dirvish' path
  endif
endfunction

" Get a detail of <title> from + register
function vimrc#get_webpage_title() abort
  try
    return substitute(s:HTML.parseURL(@+).find('title').value(), '·', '-', 'g')
  catch
    return 'vimrc#get_webpage_title(): something happened: ' .. v:exception
  endtry
endfunction

" :quit if only a window is existent,
" :hide otherwise
function vimrc#hide_or_quit() abort
  const tabnum = tabpagenr('$')
  const winnum = tabpagewinnr(tabpagenr(), '$')
  if tabnum is 1 && winnum is 1
    quit
  else
    hide
  endif
endfunction

" Toggle b:ale_enabled
function vimrc#toggle_ale_at_buffer() abort
  let b:ale_enabled = !get(b:, 'ale_enabled', 1)
  " Refresh the state
  ALEToggle
  ALEToggle
endfunction

" Toggle showing indent-guides with variable
function vimrc#toggle_indent_guides()
  let g:vimrc#indent_guides_enable = !get(g:, 'vimrc#indent_guides_enable', v:true)
  IndentGuidesToggle
endfunction

" Delete a surround `{ _ }` -> ` _ `
function vimrc#delete_mostly_inner_surround() abort
  call dein#source('vim-operator-surround')
  const obj_keys = s:get_current_obj_keys()
  const obj_key = s:input_obj_key_of(obj_keys)
  execute 'normal' ('va' .. obj_key .. "\<Plug>(operator-surround-delete)")
  call repeat#set("\<Plug>(vimrc-surround-delete-mostly-inner)" .. obj_key)
endfunction

function s:get_current_obj_keys() abort
  const surrounds = g:operator#surround#blocks['-'] + get(g:operator#surround#blocks, &filetype, [])
  const obj_keys = map(surrounds, { _, x -> x.keys })
  return s:List.flatten(obj_keys)
endfunction

function s:input_obj_key_of(obj_keys) abort
  let stroke = ''
  while !s:List.has(a:obj_keys, stroke)
    let char = nr2char(getchar())
    if s:List.has(['', '', ''], char)
      return v:null
    endif
    let stroke ..= char
  endwhile
  return stroke
endfunction

" Replaces a surround to a surround `{ _ }` -> `[ _ ]`
function vimrc#replace_mostly_inner_surround() abort
  call dein#source('vim-operator-surround')
  const obj_keys = s:get_current_obj_keys()
  const obj_key_from = s:input_obj_key_of(obj_keys)
  const obj_key_to = s:input_obj_key_of(obj_keys)
  execute 'normal' ('va' .. obj_key_from  .. "\<Plug>(operator-surround-replace)" .. obj_key_to)
  call repeat#set("\<Plug>(vimrc-surround-replace-mostly-inner)" .. obj_key_from .. obj_key_to)
endfunction

" a:visualizer: e.g. 'viw', 'viW'
function s:append_choose_surround(visualizer) abort
  call dein#source('vim-operator-surround')
  const obj_keys = s:get_current_obj_keys()
  const obj_key = s:input_obj_key_of(obj_keys)
  execute 'normal' (a:visualizer .. "\<Plug>(operator-surround-append)" .. obj_key)
  return obj_key
endfunction

function vimrc#append_choose_surround() abort
  const obj_key = s:append_choose_surround('viw')
  call repeat#set("\<Plug>(vimrc-surround-append-choice)" .. obj_key)
endfunction

function vimrc#append_choose_surround_wide() abort
  const obj_key = s:append_choose_surround('viW')
  call repeat#set("\<Plug>(vimrc-surround-append-choice-wide)" .. obj_key)
endfunction

" Puts a regsiter as stdin to the terminal buffer
function vimrc#put_as_stdin(detail) abort
  const current_bufnr = bufnr('%')
  call timer_start(0, { _ -> term_sendkeys(current_bufnr, a:detail) }, {'repeat': 1})
  return 'i'
endfunction

function vimrc#move_window_forward()
  const tabwin_num = len(tabpagebuflist())
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
endfunction

" Current buffer move to next tab
function vimrc#move_window_backward()
  mark Z
  hide
  tabprevious
  vsp
  normal! 'Z

  if foldlevel('.') > 0
    normal! zO
  endif
  normal! zz
endfunction

function vimrc#move_tab_prev()
  if tabpagenr() is 1
    $tabmove
  else
    tabmove -1
  endif
endfunction

function vimrc#move_tab_next()
  if tabpagenr() is tabpagenr('$')
    0tabmove
  else
    +tabmove
  endif
endfunction

function vimrc#execute_on_base_path(f, ...) abort
  call call(function('vimrc#execute_on'), [{ -> g:vimrc.path_at_started }, a:f] + a:000)
endfunction

function vimrc#execute_on_file_path(f, ...) abort
  call call(function('vimrc#execute_on'), [{ -> expand('%:p:h') }, a:f] + a:000)
endfunction

function vimrc#execute_on(get_path, f, ...) abort
  const current = getcwd()

  try
    execute ':lcd' a:get_path()
    call call(a:f, a:000)
  catch
    echo 'Using getcwd() instead of a:get_path()'
    call call(function('vimrc#execute_on'), [{ -> getcwd() }, a:f] + a:000)
  finally
    execute ':lcd' current
  endtry
endfunction

function vimrc#open_scratch_buffer() abort
  const file = s:find_fresh_scratch_file()
  if file is v:null
    call s:Msg.error('no fresh scratch file found.')
    return
  endif

  execute 'silent' 'sp' file
  silent write
  setl noswapfile
  normal! ggVG"_d
  resize 5
endfunction

function s:find_fresh_scratch_file() abort
  for i in range(0, 100000)
    let file = printf('/tmp/scratch%d.md', i)
    if !filereadable(expand(file))
      return file
    endif
  endfor
  return v:null
endfunction

function vimrc#open_buffer_to_execute(cmd) abort
  call vimrc#open_scratch_buffer()
  put=execute(a:cmd)
  normal! gg2dd
  silent write
endfunction

" Yank posted gist to clipboard
function vimrc#yank_gista_posted_url() abort
  const gistid = g:gista#avars.gistid
  execute printf('Gista browse --yank --gistid=%s', gistid)
  let @+ = @"
endfunction

" Auto set cursor position in the file
function vimrc#visit_past_position()
  const past_posit = line("'\"")
  if past_posit > 0 && past_posit <= line('$')
    execute 'normal! g`"'
  endif
endfunction

function vimrc#lcd_base_dir() abort
  try
    lcd %:p:h
  catch /\(E344\|E472\)/  " if the buffer has no file
    if !s:List.has(term_list(), winbufnr('.'))
      " NOTE: Delegate :lcd to sync-term-cwd.vim if this is a terminal buffer
      execute ':lcd' g:vimrc.path_at_started
    endif
  endtry
endfunction

" Rename the file of current buffer
function vimrc#rename_to(new_name) abort
  const this_file = fnameescape(expand('%'))
  const new_name  = fnameescape(a:new_name)

  if fnamemodify(this_file, ':t') ==# new_name
    call s:Msg.error('New name is same old name, operation abort')
    return
  endif

  const file_editing = &modified
  if file_editing
    call s:Msg.error('Please :write this file')
    return
  endif

  const new_file = fnamemodify(this_file, ':h') .. '/' .. new_name
  const failed   = rename(this_file, new_file)
  if failed
    call s:Msg.error(printf('Rename %s to %s is failed', this_file, new_file))
    return
  endif

  execute ':edit' new_file
  silent write
  silent execute ':bdelete' this_file

  echo printf('Renamed %s to %s', this_file, new_file)
endfunction

" Make session_name from git repository
" and Save current session by :UniteSessionSave
function vimrc#git_branch_session_save() abort
  const repo_path = fnameescape(system('git rev-parse --show-toplevel'))

  const repo_name  = fnamemodify(repo_path, ':t')
  const repo_name_ = substitute(repo_name, '\n', '', '')  " Remove tail line break

  const branch_name  = system(printf("cd %s ; git branch | sort | tail -1 | awk '{print $2}'", repo_path))  " Don't use double quote in awk
  const branch_name_ = substitute(branch_name, '\n', '', '')  " Remove tail line break

  const session_name  = repo_name_ .. '-' .. branch_name_
  const session_name_ = substitute(session_name, '/', '-', 'g')
  const session_name__ = substitute(session_name_, '#', '-', 'g')  "NOTE: '#' shouldn't be used as a file name

  execute 'mksession!' (g:vimrc['sessiondir'] .. '/' .. session_name__ .. '.vim')
endfunction

function vimrc#increment_gui_fontsize() abort
  let g:vimrc.guifont.size += 1
  let &guifont = 'RictyDiminished NF ' .. g:vimrc.guifont.size
endfunction

function vimrc#decrement_gui_fontsize() abort
  let g:vimrc.guifont.size -= 1
  let &guifont = 'RictyDiminished NF ' .. g:vimrc.guifont.size
endfunction

function s:caddexpr_on_stdout(data) abort
  for line in a:data
    " NOTE:
    " On NeoVim v0.3.1, the job's on_stdout may take 『『『無』』』 as a line break and else.
    " This passes it.
    if line ==# ''
      caddexpr "\n"
      continue
    endif
    caddexpr line
  endfor

  " Always show the last lines
  for n in range(1, winnr('$'))
    if getwinvar(n, '&filetype') ==# 'qf'
      execute n 'windo' 'normal! G'
      wincmd p
      break
    endif
  endfor
endfunction

" Open tweetvim by private account
function vimrc#twitter(account) abort
  execute ':TweetVimSwitchAccount' a:account
  TweetVimHomeTimeline
endfunction

function vimrc#tweet(account) abort
  execute ':TweetVimSwitchAccount' a:account
  TweetVimSay
endfunction

" const s:read_to_quickfix_it {{{

const s:read_to_quickfix_it = { cmd ->
  \ s:Job.start(cmd, {
    \ 'on_stdout': function('s:caddexpr_on_stdout'),
    \ 'on_stderr': function('s:caddexpr_on_stdout'),
  \ })
\ }

" }}}

" TODO: Unify run_foo_quickfix to one
function vimrc#run_gradle_quickfix(gradle_subcmd) abort
  const current = getcwd()
  try
    CClear
    const gradle_cmd = ['gradle', '--console=plain'] + split(a:gradle_subcmd, ' ')
    execute ':lcd' g:vimrc.path_at_started
    call s:read_to_quickfix_it(gradle_cmd)
    copen
  finally
    execute ':lcd' current
  endtry

  " Avoid
  augroup ScalaCompileWatch
    autocmd!
    autocmd VimLeave * call vimrc#stop_scala_compile_watch_quickfix()
  augroup END
endfunction

" NOTE: This requires to add sbt-launch.jar to $PATH
function vimrc#run_scala_compile_watch_quickfix(sbt_subcmd) abort
  CClear
  call vimrc#stop_scala_compile_watch_quickfix() " Avoid running more processes
  " Run sbt directly for killing the sbt process (vimrc#stop_scala_compile_watch_quickfix)
  const sbt_launcher = system('which sbt-launch.jar')[0:-2]
  const sbt_cmd = ['java', '-jar', sbt_launcher, '-Dsbt.log.noformat=true', '~test:compile'] + split(a:sbt_subcmd, ' ')
  let s:sbt_compile_watch_job = s:read_to_quickfix_it(sbt_cmd)
  copen
endfunction

function vimrc#stop_scala_compile_watch_quickfix() abort
  if get(s:, 'sbt_compile_watch_job', v:null) isnot v:null
    call s:sbt_compile_watch_job.stop()
    let s:sbt_compile_watch_job = v:null
    cclose
  endif
endfunction

function vimrc#run_yarn_quickfix(yarn_subcmd) abort
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

function vimrc#run_make_quickfix(make_args) abort
  const current = getcwd()
  try
    CClear
    const make_args = map(split(a:make_args, '  '), { _, x ->
      \ substitute(x, '''', '', 'g')
    \ })  " Please see ftplugin c.vim
    const make_cmd = ['make'] + make_args
    echomsg make_cmd
    execute ':lcd' g:vimrc.path_at_started
    call s:read_to_quickfix_it(make_cmd)
    copen
  finally
    execute ':lcd' current
  endtry
endfunction

function vimrc#grep_those(...) abort
  CClear
  call s:List.map(a:000, { word ->
    \ execute('grepadd ' .. word .. ' %', 'silent!')
  \ })
  copen
endfunction

function vimrc#open_this_file_in_gui() abort
  const file = expand('%:p')
  call s:Job.start([g:vimrc.gui_editor, file])
endfunction

function vimrc#execute_repeatable_macro(name) abort
  const name = '@' .. a:name

  execute 'normal!' name
  call repeat#set(name)
endfunction
