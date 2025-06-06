scriptencoding utf-8
scriptversion 3

let s:Job = vital#vimrc#import('System.Job')
let s:List = vital#vimrc#import('Data.List')
let s:Msg = vital#vimrc#import('Vim.Message')
let s:Promise = vital#vimrc#import('Async.Promise')

" Allows to reuse `self`.
" {{{
"
" Params:
"   self: A
"   f: (self: A) -> B
"
" Result: B
"
" Example:
"   join(a:stdout, '')->vimrc#let({ result ->
"     \ result ==# foo
"       \ ? bar
"       \ : result
"    \ })
"
" }}}
function! vimrc#let(self, f) abort
  return a:f(a:self)
endfunction

" Applies `f` if `p(value)`.
" To re-use a:value.
" {{{
"
" Params:
"   value: A
"   p: (value: A) -> Bool
"   f: (value: A) -> B
"
" Result: A | B
"
" Example:
"   let git_root = fnameescape(join(a:stdout, ''))->vimrc#apply_if(
"     \ { git_root -> (git_root !~# '^/') && executable('wslpath') },
"     \ { git_root_windows -> fnameescape(system("wslpath '%s'", git_root_windows)) },
"   \ )
"
" }}}
function! vimrc#apply_if(value, p, f) abort
  return a:value->vimrc#let({ value ->
    \ a:p(value) ? a:f(value) : value
  \ })
endfunction

function! vimrc#identity(x) abort
  return a:x
endfunction

" Returns a:alt if f() throws an exception.
function! vimrc#catch(f, alt) abort
  try
    call a:f()
  catch
    return a:alt
  endtry
endfunction

" Converts from a Windows path to the WSL2 path if you are on WSL2.
"
" Params:
"   cont: (git_root: string) -> A
"   stdout: Array<string> | string
"   stderr: Array<string> | string
function! s:parse_git_root(cont, stdout, stderr) abort
  if type(a:stderr) is type([]) && a:stderr !=# []
    throw join(a:stderr)
  endif
  if type(a:stderr) is type('') && a:stderr !=# ''
    throw a:stderr
  endif

  let stdout = type(a:stdout) is type([])
    \ ? join(a:stdout, '')
    \ : a:stdout

  " Replace to the wsl2's path if git_root is a windows path (by git.exe)
  " NOTE: -2 removes the trailing line break
  let git_root = fnameescape(stdout)->vimrc#apply_if(
    \ { git_root -> (git_root !~# '^/') && executable('wslpath') },
    \ { git_root_windows -> fnameescape(system('wslpath "git_root_windows"'))[:-3] },
  \ )
  " ^^ TODO: Use timer_start() instead of system()

  return a:cont(git_root)
endfunction

function! vimrc#read_git_root(cont) abort
  call vimrc#job#start_simply(
    \ ['git', 'rev-parse', '--show-toplevel'],
    \ function('s:parse_git_root', [a:cont]),
  \ )
endfunction

function! s:set_git_root_to_g_vimrc(git_root) abort
  echomsg 'vimrc: a git root detected: ' .. a:git_root
  let g:vimrc.git_root = a:git_root
endfunction

function! vimrc#read_git_root_to_set_g_vimrc_async() abort
  call vimrc#read_git_root(function('s:set_git_root_to_g_vimrc'))
endfunction

function! vimrc#read_git_root_sync() abort
  const result = system('git rev-parse --show-toplevel')

  if v:shell_error
    throw 'Failed to read a git root directory'
  endif

  return result
endfunction

" git-clones dein.vim to a:install_dirname.
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

" Compresses continuously spaces to a space.
function! vimrc#compress_spaces()
  const recent_pattern = @/
  try
    execute 's/\s\+/ /g'
    normal! ==
  finally
    let @/ = recent_pattern
  endtry
  nohlsearch
endfunction

" Removes trailing spaces of all lines.
function! vimrc#remove_trailing_spaces()
  const recent_pattern = @/
  const curpos = getcurpos()
  try
    execute '%s/\s*\?$//g'
  catch /E486/
    echo 'nothing todo'
  finally
    let @/ = recent_pattern
    call setpos('.', curpos)
  endtry
endfunction

" Toggles diffthis and diffoff with some keymappings.
function! vimrc#toggle_diff()
  if &diff
    diffoff
    "NOTE: This restores [c and ]c of .vimrc,
    "NOTE: Please see [c, ]c, [ale-previous], and [ale-next] in .vimrc.
    nmap <buffer> [c [ale-previous]
    nmap <buffer> ]c [ale-next]
  else
    diffthis
    " Get original
    nnoremap <buffer> [c [c
    nnoremap <buffer> ]c ]c
  endif
  set diff?
endfunction

" Closes buffers of a specified filetypes.
function! vimrc#bufclose_filetype(filetypes)
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

" Toggles a file explorer
function! vimrc#toggle_explorer(...)
  const path = get(a:000, 0, expand('%:p:h'))
  const closed = vimrc#bufclose_filetype(['dirvish'])
  if !closed
    call vimrc#open_explorer('vsplit', path)
  endif
endfunction

function! vimrc#open_explorer(split, ...) abort
  const path = get(a:000, 0, expand('%:p:h'))
  const cmd =
    \ a:split ==# 'stay'  ? ':Dirvish' :
    \ a:split ==# 'split' ? ':split | silent Dirvish' :
    \ a:split ==# 'vsplit' ? ':vsplit | silent Dirvish' :
    \ a:split ==# 'tabnew' ? ':tabnew | silent Dirvish' :
      \ execute($'throw "an unexpected way to open the explorer: {a:split}"')

  if !isdirectory(path)
    " :silent to ignore an error message. Because opening seems success.
    silent execute cmd
    return
  endif

  execute cmd path
endfunction

" Fetches a detail of <title> from a URL
function! vimrc#get_webpage_title(url) abort
  try
    echo 'fetching now...'
    return system($'curl --silent {a:url} | pup --plain "title json{{}}" | jq -r ".[0].text"')
  catch
    return $'vimrc#get_webpage_title(): something happened: {v:exception}'
  endtry
endfunction

" :quit if only a window is existent.
" :hide otherwise.
function! vimrc#hide_or_quit() abort
  const tabnum = tabpagenr('$')
  const winnum = tabpagewinnr(tabpagenr(), '$')
  if tabnum is 1 && winnum is 1
    quit
  else
    hide
  endif
endfunction

function! vimrc#toggle_ale_at_buffer() abort
  let b:ale_enabled = !get(b:, 'ale_enabled', 1)
  " Refresh the state
  ALEToggle
  ALEToggle
endfunction

" Toggles indent-guides
function! vimrc#toggle_indent_guides()
  let g:vimrc#indent_guides_enable = !get(g:, 'vimrc#indent_guides_enable', v:true)
  IndentGuidesToggle
endfunction

" Deletes the surround of `{ _ }` -> ` _ `
function! vimrc#delete_mostly_inner_surround() abort
  call dein#source('vim-operator-surround')

  const obj_keys = s:get_current_obj_keys()
  const obj_key = s:input_obj_key_of(obj_keys)
  if obj_key ==# v:null
    echo 'Cancelled'
    return
  endif

  " TODO: Workaround. For some reason 'B' ('**foo**') cannot be deleted
  if obj_key ==# 'B'
    execute 'normal' $"d\<Plug>(textobj-between-i)*"
    execute $'s/\*\*\*\*/{@"}/'
    echo '**deleted**'
    return
  endif

  execute 'normal' ('va' .. obj_key .. "\<Plug>(operator-surround-delete)")
  call repeat#set("\<Plug>(vimrc-surround-delete-mostly-inner)" .. obj_key)
endfunction

function! s:get_current_obj_keys() abort
  const surrounds = g:operator#surround#blocks['-'] + get(g:operator#surround#blocks, &filetype, [])
  const obj_keys = s:List.map(surrounds, { x -> x.keys })
  return s:List.flatten(obj_keys)
endfunction

function! s:input_obj_key_of(obj_keys) abort
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

" Replaces a surround to the surround of `{ _ }` -> `[ _ ]`.
function! vimrc#replace_mostly_inner_surround() abort
  call dein#source('vim-operator-surround')
  const obj_keys = s:get_current_obj_keys()
  const obj_key_from = s:input_obj_key_of(obj_keys)
  const obj_key_to = s:input_obj_key_of(obj_keys)
  execute 'normal' ('va' .. obj_key_from  .. "\<Plug>(operator-surround-replace)" .. obj_key_to)
  call repeat#set("\<Plug>(vimrc-surround-replace-mostly-inner)" .. obj_key_from .. obj_key_to)
endfunction

" a:visualizer: e.g. 'viw', 'viW'
function! s:append_choose_surround(visualizer) abort
  call dein#source('vim-operator-surround')
  const obj_keys = s:get_current_obj_keys()
  const obj_key = s:input_obj_key_of(obj_keys)
  execute 'normal' (a:visualizer .. "\<Plug>(operator-surround-append)" .. obj_key)
  return obj_key
endfunction

function! vimrc#append_choose_surround() abort
  const obj_key = s:append_choose_surround('viw')
  call repeat#set("\<Plug>(vimrc-surround-append-choice)" .. obj_key)
endfunction

function! vimrc#append_choose_surround_wide() abort
  const obj_key = s:append_choose_surround('viW')
  call repeat#set("\<Plug>(vimrc-surround-append-choice-wide)" .. obj_key)
endfunction

" Puts a regsiter as stdin into the terminal buffer.
function! vimrc#put_as_stdin(detail) abort
  const current_bufnr = bufnr('%')
  call timer_start(0, { _ -> term_sendkeys(current_bufnr, a:detail) }, {'repeat': 1})
  return 'i'
endfunction

" Moves a current buffer to left of tab.
function! vimrc#move_window_forward()
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

" Moves a current buffer to right of tab.
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

" Moves tab to left.
function! vimrc#move_tab_prev()
  if tabpagenr() is 1
    $tabmove
  else
    tabmove -1
  endif
endfunction

" Moves tab to right.
function! vimrc#move_tab_next()
  if tabpagenr() is tabpagenr('$')
    0tabmove
  else
    +tabmove
  endif
endfunction

function! vimrc#execute_on_base_path(f, ...) abort
  call call(function('vimrc#execute_on'), [{ -> g:vimrc.path_at_started }, a:f] + a:000)
endfunction

function! vimrc#execute_on_file_path(f, ...) abort
  call call(function('vimrc#execute_on'), [{ -> expand('%:p:h') }, a:f] + a:000)
endfunction

function! vimrc#execute_on(get_path, f, ...) abort
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

" Moves the cursor position to the last position of a file.
function! vimrc#visit_past_position()
  const past_posit = line("'\"")
  if past_posit > 0 && past_posit <= line('$')
    execute 'normal! g`"'
  endif
endfunction

" Renames a file name of the current buffer.
function! vimrc#rename_to(new_name) abort
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
  const failed = rename(this_file, new_file)
  if failed
    call s:Msg.error($'Rename {this_file} to {new_file} is failed')
    return
  endif

  execute ':edit' new_file
  silent write
  silent execute ':bdelete' this_file

  echo printf('Renamed %s to %s', this_file, new_file)
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

" Opens tweetvim' buffer with a private account.
function! vimrc#twitter(account) abort
  execute ':TweetVimSwitchAccount' a:account
  TweetVimHomeTimeline
endfunction

function! vimrc#tweet(account) abort
  execute ':TweetVimSwitchAccount' a:account
  TweetVimSay
endfunction

const s:read_to_quickfix_it = { cmd ->
  \ s:Job.start(cmd, {
    \ 'on_stdout': function('s:caddexpr_on_stdout'),
    \ 'on_stderr': function('s:caddexpr_on_stdout'),
  \ })
\ }

" TODO: Unify run_foo_quickfix to one
function! vimrc#run_gradle_quickfix(gradle_subcmd) abort
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
function! vimrc#run_scala_compile_watch_quickfix(sbt_subcmd) abort
  CClear
  call vimrc#stop_scala_compile_watch_quickfix() " Avoid running more processes
  " Run sbt directly for killing the sbt process (vimrc#stop_scala_compile_watch_quickfix)
  const sbt_launcher = system('which sbt-launch.jar')[0:-2]
  const sbt_cmd = ['java', '-jar', sbt_launcher, '-Dsbt.log.noformat=true', '~test:compile'] + split(a:sbt_subcmd, ' ')
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

function! vimrc#run_make_quickfix(make_args) abort
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

function! vimrc#execute_repeatable_macro(name) abort
  const name = '@' .. a:name

  execute 'normal!' name
  call repeat#set(name)
endfunction

function! vimrc#operator_camelize_toggle_current_word_with_setting_repeatable() abort
  execute 'normal' "viw\<Plug>(operator-camelize-toggle)"
  call repeat#set("viw\<Plug>(operator-camelize-toggle)")
endfunction

" Useful to check we can do `:tabclose`.
function! vimrc#has_two_or_more_tabpages() abort
  return tabpagenr('$') > 1
endfunction

function! s:cd_git_root(cd, git_root) abort
  echo 'vimrc: The current directory changed to: ' .. a:git_root

  if type(a:cd) is type('')
    execute a:cd a:git_root
    return
  endif

  call a:cd(a:git_root)
endfunction

" Params
"   cd: string | (git_root: string) -> void
"     a cd command. e.g. ':cd' or ':lcd'.
function! vimrc#cd_git_root(cd) abort
  call vimrc#read_git_root(function('s:cd_git_root', [a:cd]))
endfunction

function! s:set_gvimrc_path_at_started_to_git_root(git_root) abort
  let g:vimrc.path_at_started = a:git_root
endfunction

function! vimrc#cd_git_root_with_gvimrc_path_at_started() abort
  call vimrc#read_git_root(
    \ function('s:cd_git_root'),
    \ [function('s:set_gvimrc_path_at_started_to_git_root')],
  \ )
endfunction

" :h Vital.Async.Promise-example-timer
function! vimrc#wait(ms)
  return s:Promise.new({resolve -> timer_start(a:ms, resolve)})
endfunction

" Starts ddu from filter mode
function! vimrc#ddu_start_from_insert(options) abort
  let g:vimrc_ddu_start_with_insert_next = v:true " Please see .vimrc what is this
  call ddu#start(a:options)
endfunction

" Similar to `vimrc#ddu_start_from_insert()`, but opens with a word instead (if a word is taken)
function! vimrc#ddu_start_from_input(options, ...) abort
  let search_word = get(a:000, 0, '')
  let g:vimrc_ddu_start_with_insert_next = search_word !=# '' ? search_word : v:true
  call ddu#start(a:options)
endfunction

function! vimrc#deepl_translate(line_count, start_line, end_line, target_lang, source_lang, methods) abort
  " If range is not specified, translate the current line, or translate the specified range
  const lines = a:line_count is -1
    \ ? [getline('.')]
    \ : getline(a:start_line, a:end_line)

  const func = #{
    \ yank: { result -> execute($'let @" = "{escape(result, '"')}"') },
    \ echo: { result -> s:Msg.echo('Normal', result) },
    \ buffer: { result -> s:deepl_traslate_open_buffer(result) },
  \ }
  const result = lines
    \ ->join("\n")
    \ ->deepl#translate(a:target_lang, a:source_lang)

  for method in a:methods
    if !func->has_key(method)
      throw $'Unknown method: {a:method}'
    endif
    call func[method](result)
  endfor
endfunction

function! s:deepl_traslate_open_buffer(result) abort
  ScratchBufferOpenNext md sp
  put=a:result
  normal! gg
  normal! "zdd
endfunction

function! vimrc#open_buffer_to_execute(cmd) abort
  const full_size = 100
  call scratch_buffer#open(v:true, 'md', 'sp', full_size)
  put=execute(a:cmd)
  normal! gg2dd
endfunction

function! vimrc#get_current_buffer_dir(...) abort
  const dir =
    \ &buftype !=# 'terminal' && &buftype !=# 'nofile'
      \ ? expand('%:p:h')
      \ : g:vimrc.git_root
  const alt_dir = get(a:000, 0, v:null)
  return (dir !=# '' && dir !=# v:null)
    \ ? dir
    \ : (alt_dir !=# v:null)
      \ ? alt_dir
      \ : execute('throw "The current buffer directory does not exist and an altlter directory is not specified"')
endfunction

" Finds a node root directory.
" This means 'Find a project root directory or a (bun) workspace directory'.
" Returns v:null if no package.json found.
function! vimrc#read_node_root_dir(base_dir) abort
  const base_dir = system($'realpath {fnameescape(a:base_dir)}')[:-2]
  if base_dir ==# '/'
    return v:null
  endif

  const is_package_json_existent = filereadable($'{base_dir}/package.json')
  if !is_package_json_existent
    return vimrc#read_node_root_dir($'{base_dir}/..')
  endif

  return base_dir
endfunction

" Returns: 'bun', 'yarn', 'npm', or v:null
function! vimrc#check_node_project_manager(base_dir) abort
  const node_root = vimrc#read_node_root_dir(a:base_dir)
  return
    \ node_root ==# v:null ? v:null :
    \ filereadable($'{node_root}/bun.lockb') ? 'bun' :
    \ filereadable($'{node_root}/yarn.lock') ? 'yarn' :
    \ 'npm'
endfunction

" :cd to the directory that have a package.json, closest to the current child directory.
" NOTE:
" In some cases, this does not mean the npm root or bun root.
" If you hit a child workspace of npm workspaces or bun workspaces, :cd there.
" @param a:cd {string}
" @param [a:1] {string}
" @returns {string}
" @throws nothing
function! vimrc#cd_node_root(cd, ...) abort
  const base_dir = get(a:000, 0, expand('%:p:h'))
  const node_dir = vimrc#read_node_root_dir(base_dir) abort
  if node_dir ==# v:null
    call s:Msg.error('No node directory found')
    return
  endif
  execute a:cd node_dir
endfunction

" Shows a popup window by `popup_atcursor()` with good options
" @param messages {Array<string> | string} -- messages or a message. See `popup_create()`
function! vimrc#popup_atcursor(messages) abort
  return popup_atcursor(a:messages, #{
    \ pos: 'topleft',
    \ line: 'cursor-2',
    \ border: [1, 1, 1, 1],
    \ borderchars: ['-', '|', '-', '|', '+', '+', '+', '+'],
    \ moved: [0, 0, 0],
  \ })
endfunction
