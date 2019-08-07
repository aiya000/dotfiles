scriptencoding utf-8

let s:V = vital#vimrc#new()

let s:HTML = s:V.import('Web.HTML')
let s:Job = s:V.import('System.Job')
let s:List = s:V.import('Data.List')
let s:Msg = s:V.import('Vim.Message')
let s:Optional = s:V.import('Data.Optional')

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

" Compress continuous space
function! vimrc#compress_spaces()
  let recent_pattern = @/
  try
    execute 'substitute/\s\+/ /g'
    normal! ==
  finally
    let @/ = recent_pattern
  endtry
  nohlsearch
endfunction

" Clear all lines end space
function! vimrc#clear_ends_space()
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
endfunction

" Toggle diffthis - diffoff
function! vimrc#toggle_diff()
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

function! vimrc#bufclose_auto() abort
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
    \ 'qf',
    \ 'quickrun',
    \ 'scratch',
    \ 'denite',
    \ 'denite-filter',
  \ ])
endfunction

" If you has nofile buffer, close it.
function! vimrc#bufclose_filetype(filetypes)
  let closed = 0
  for w in range(1, winnr('$'))
    let buf_ft = getwinvar(w, '&filetype')
    if s:List.has(a:filetypes, buf_ft)
      execute ':' . w . 'wincmd w'
      quit
      let closed = 1
    endif
  endfor
  return closed
endfunction

" Toggle open netrw explorer ( vertical split )
function! vimrc#toggle_explorer(...)
  let path = get(a:000, 0, '')
  let closed = vimrc#bufclose_filetype(['netrw'])
  if !closed
    execute ':Vexplore' path
  endif
endfunction

" Get a detail of <title> from + register
function! vimrc#get_webpage_title() abort
  return substitute(s:HTML.parseURL(@+).find('title').value(), '·', '-', 'g')
endfunction

" :quit if only a window is existent,
" :hide otherwise
function! vimrc#hide_or_quit() abort
  let tabnum = tabpagenr('$')
  let winnum = tabpagewinnr(tabpagenr(), '$')
  if tabnum is 1 && winnum is 1
    quit
  else
    hide
  endif
endfunction

" Toggle b:ale_enabled
function! vimrc#toggle_ale_at_buffer() abort
  let b:ale_enabled = !get(b:, 'ale_enabled', 1)
  " Refresh the state
  ALEToggle
  ALEToggle
endfunction

" Toggle showing indent-guides with variable
function! vimrc#toggle_indent_guides()
  let g:vimrc#indent_guides_enable = !get(g:, 'vimrc#indent_guides_enable', v:true)
  IndentGuidesToggle
endfunction

" Delete a surround `{ _ }` -> ` _ `
function! vimrc#delete_mostly_inner() abort
  call dein#source('vim-operator-surround')
  let obj_keys = s:get_current_obj_keys()
  call s:Optional.map(s:input_obj_key_on(obj_keys), { obj_key ->
    \ s:normal('va' . obj_key . "\<Plug>(operator-surround-delete)")
  \ })
endfunction

function! s:get_current_obj_keys() abort
  let surrounds = g:operator#surround#blocks['-'] + get(g:operator#surround#blocks, &filetype, [])
  let obj_keys = map(surrounds, { _, x -> x.keys })
  return s:List.flatten(obj_keys)
endfunction

function! s:input_obj_key_on(obj_keys) abort
  let stroke = ''
  while !s:List.has(a:obj_keys, stroke)
    let char = nr2char(getchar())
    if s:List.has(['', '', ''], char)
      return s:Optional.none()
    endif
    let stroke .= char
  endwhile
  return s:Optional.new(stroke)
endfunction

" Avoids E930
function! s:normal(stroke) abort
  execute 'normal ' . a:stroke
endfunction

" Replaces a surround to a surround `{ _ }` -> `[ _ ]`
function! vimrc#replace_mostly_inner() abort
  call dein#source('vim-operator-surround')
  let obj_keys = s:get_current_obj_keys()
  call s:Optional.map(s:input_obj_key_on(obj_keys), { from ->
    \ s:Optional.map(s:input_obj_key_on(obj_keys), { to ->
      \ s:normal(('va' . from  . "\<Plug>(operator-surround-replace)" . to))
    \ })
  \ })
endfunction

" a:visualizer: e.g. 'viw', 'viW'
function! vimrc#append_surround(visualizer) abort
  call dein#source('vim-operator-surround')
  let obj_keys = s:get_current_obj_keys()
  call s:Optional.map(s:input_obj_key_on(obj_keys), { obj_key ->
    \ s:normal(a:visualizer . "\<Plug>(operator-surround-append)" . obj_key)
  \ })
endfunction

" Puts a regsiter as stdin to the terminal buffer
function! vimrc#put_as_stdin(detail) abort
  let current_bufnr = bufnr('%')
  call timer_start(0, { _ -> term_sendkeys(current_bufnr, a:detail) }, {'repeat': 1})
  return 'i'
endfunction

function! vimrc#move_window_forward()
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
endfunction

" Current buffer move to next tab
function! vimrc#move_window_backward()
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

function! vimrc#move_tab_prev()
  if tabpagenr() is 1
    $tabmove
  else
    tabmove -1
  endif
endfunction

function! vimrc#move_tab_next()
  if tabpagenr() is tabpagenr('$')
    0tabmove
  else
    +tabmove
  endif
endfunction

function! vimrc#execute_on_base_path(f, ...) abort
  let current = getcwd()
  try
    execute ':lcd' g:vimrc.path_at_started
    call call(a:f, a:000)
  finally
    execute ':lcd' current
  endtry
endfunction

function! vimrc#open_scratch_buffer() abort
  let file = s:find_fresh_scratch_file()
  if file is v:null
    call s:Msg.error('no fresh scratch file found.')
    return
  endif

  execute 'silent' 'sp' file
  silent write
  normal! ggVG"_d
  resize 5
endfunction

function! s:find_fresh_scratch_file() abort
  for i in range(0, 100000)
    let file = printf('/tmp/scratch%d.md', i)
    if !filereadable(expand(file))
      return file
    endif
  endfor
  return v:null
endfunction

function! vimrc#open_buffer_to_execute(cmd) abort
  call vimrc#open_scratch_buffer()
  put=execute(a:cmd)
  normal! gg2dd
  silent write
endfunction

" Yank posted gist to clipboard
function! vimrc#yank_gista_posted_url() abort
  let gistid = g:gista#avars.gistid
  execute printf('Gista browse --yank --gistid=%s', gistid)
  let @+ = @"
endfunction

" Auto set cursor position in the file
function! vimrc#visit_past_position()
  let past_posit = line("'\"")
  if past_posit > 0 && past_posit <= line('$')
    execute 'normal! g`"'
  endif
endfunction

function! vimrc#lcd_buffer_dir_or_base() abort
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
function! vimrc#rename_to(new_name) abort
  let this_file = fnameescape(expand('%'))
  let new_name  = fnameescape(a:new_name)

  if fnamemodify(this_file, ':t') ==# new_name
    call s:Msg.error('New name is same old name, operation abort')
    return
  endif

  let file_editing = &modified
  if file_editing
    call s:Msg.error('Please :write this file')
    return
  endif

  let new_file = fnamemodify(this_file, ':h') . '/' . new_name
  let failed   = rename(this_file, new_file)
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
function! vimrc#git_branch_session_save() abort
  let repo_path = fnameescape(system('git rev-parse --show-toplevel'))

  let repo_name  = fnamemodify(repo_path, ':t')
  let repo_name_ = substitute(repo_name, '\n', '', '')  " Remove tail line break

  let branch_name  = system(printf("cd %s ; git branch | sort | tail -1 | awk '{print $2}'", repo_path))  " Don't use double quote in awk
  let branch_name_ = substitute(branch_name, '\n', '', '')  " Remove tail line break

  let session_name  = repo_name_ . '-' . branch_name_
  let session_name_ = substitute(session_name, '/', '-', 'g')
  let session_name__ = substitute(session_name_, '#', '-', 'g')  "NOTE: '#' shouldn't be used as a file name

  execute 'mksession!' (g:vimrc['sessiondir'] . '/' . session_name__ . '.vim')
endfunction

function! vimrc#increment_gui_fontsize() abort
  let g:vimrc.guifont.size += 1
  let &guifont = 'RictyDiminished NF ' . g:vimrc.guifont.size
endfunction

function! vimrc#decrement_gui_fontsize() abort
  let g:vimrc.guifont.size -= 1
  let &guifont = 'RictyDiminished NF ' . g:vimrc.guifont.size
endfunction

function! s:caddexpr_on_stdout(data) abort
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

" Append quickrun config for windows
function! vimrc#append_config_quickrun_windows() abort
  " C#
  let g:quickrun_config.cs['command'] = 'csc.exe'
  let g:quickrun_config.cs['hook/output_encode/encoding'] = 'cp932:utf-8'
  " Java
  let g:quickrun_config.java['hook/output_encode/encoding'] = 'cp932:utf-8'
  " HTML
  let g:quickrun_config.html['command'] = 'rundll32'
  let g:quickrun_config.html['exec']    = '%c url.dll,FileProtocolHandler uri file://%s:p'
endfunction

" Open tweetvim by private account
function! vimrc#twitter_private() abort
  if !exists('g:vimrc.private["twitter"]["private_account"]')
    call s:Msg.error('Not set env variable => g:vimrc.private["twitter"]["private_account"]')
    return
  endif

  execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['private_account']
  let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['private_account']

  TweetVimHomeTimeline
endfunction

" Open tweetvim_say by private account
function! vimrc#tweet_private() abort
  if !exists('g:vimrc.private["twitter"]["private_account"]')
    call s:Msg.error('Not set env variable => g:vimrc.private["twitter"]["private_account"]')
    return
  endif

  execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['private_account']
  TweetVimSay
endfunction

" Open tweetvim by public account
function! vimrc#twitter_public() abort
  if !exists("g:vimrc.private['twitter']['public_account']")
    call s:Msg.error("Not set env variable => g:vimrc.private['twitter']['public_account']")
    return
  endif

  execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['public_account']
  let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['public_account']

  TweetVimHomeTimeline
endfunction

" Open tweetvim_say by public account
function! vimrc#tweet_public(...) abort
  if !exists('g:vimrc.private["twitter"]["public_account"]')
    call s:Msg.error('Not set env variable => g:vimrc.private["twitter"]["public_account"]')
    return
  endif

  execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['public_account']
  if len(a:000) is 0
    TweetVimSay
  else
    execute 'TweetVimCommandSay' join(a:000)
  endif

  "@Incomplete('wait here')
  "execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['curr_ac']
endfunction

" let s:read_to_quickfix_it {{{

let s:read_to_quickfix_it = { cmd ->
  \ s:Job.start(cmd, {
    \ 'on_stdout': function('s:caddexpr_on_stdout'),
    \ 'on_stderr': function('s:caddexpr_on_stdout'),
  \ })
\ }

" }}}
" TODO: Unify run_foo_quickfix to one
function! vimrc#run_gradle_quickfix(gradle_subcmd) abort
  let current = getcwd()
  try
    CClear
    let gradle_cmd = ['gradle', '--console=plain'] + split(a:gradle_subcmd, ' ')
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
function! vimrc#run_scala_compile_watch_quickfix(sbt_subcmd) abort
  CClear
  call vimrc#stop_scala_compile_watch_quickfix() " Avoid running more processes
  " Run sbt directly for killing the sbt process (vimrc#stop_scala_compile_watch_quickfix)
  let sbt_launcher = system('which sbt-launch.jar')[0:-2]
  let sbt_cmd = ['java', '-jar', sbt_launcher, '-Dsbt.log.noformat=true', '~test:compile'] + split(a:sbt_subcmd, ' ')
  let s:sbt_compile_watch_job = s:read_to_quickfix_it(sbt_cmd)
  copen
endfunction

function! vimrc#stop_scala_compile_watch_quickfix() abort
  if get(s:, 'sbt_compile_watch_job', v:null) isnot v:null
    call s:sbt_compile_watch_job.stop()
    let s:sbt_compile_watch_job = v:null
    cclose
  endif
endfunction

function! vimrc#run_yarn_quickfix(yarn_subcmd) abort
  let current = getcwd()
  try
    CClear
    let yarn_cmd = ['yarn'] + split(a:yarn_subcmd, ' ')
    execute ':lcd' g:vimrc.path_at_started
    call s:read_to_quickfix_it(yarn_cmd)
    copen
  finally
    execute ':lcd' current
  endtry
endfunction

function! vimrc#run_make_quickfix(make_args) abort
  let current = getcwd()
  try
    CClear
    let make_cmd = ['make'] + split(a:make_args, ' ')
    execute ':lcd' g:vimrc.path_at_started
    call s:read_to_quickfix_it(make_cmd)
    copen
  finally
    execute ':lcd' current
  endtry
endfunction

function! vimrc#grep_those(...) abort
  CClear
  call s:List.map(a:000, { word ->
    \ execute('grepadd ' . word . ' %', 'silent!')
  \ })
  copen
endfunction

function! vimrc#open_this_file_in_gui() abort
  let file = expand('%:p')
  call s:Job.start([g:vimrc.gui_editor, file])
endfunction
