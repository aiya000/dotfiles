let s:V = vital#vimrc#new()
let s:Msg = s:V.import('Vim.Message')
let s:Job = s:V.import('System.Job')
let s:List = s:V.import('Data.List')

" TODO: Move functions to the ftplugin

" General functions {{{

function! s:caddexpr_on_stdout(data) abort " {{{
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
endfunction " }}}

" ------- }}}

" Append quickrun config for windows
function! vimrc#plugins#append_config_quickrun_windows() abort " {{{
    " C#
    let g:quickrun_config.cs['command'] = 'csc.exe'
    let g:quickrun_config.cs['hook/output_encode/encoding'] = 'cp932:utf-8'
    " Java
    let g:quickrun_config.java['hook/output_encode/encoding'] = 'cp932:utf-8'
    " HTML
    let g:quickrun_config.html['command'] = 'rundll32'
    let g:quickrun_config.html['exec']    = '%c url.dll,FileProtocolHandler uri file://%s:p'
endfunction " }}}

" Append quickrun config for cygwin
function! vimrc#plugins#append_config_quickrun_cygwin() abort " {{{
    " C#
    let g:quickrun_config.cs['command'] = 'csc.exe'
    let g:quickrun_config.cs['hook/output_encode/encoding'] = 'cp932:utf-8'
    " Java
    let g:quickrun_config.java['exec']                        = ['%c %o `cygpath -w %s:p`', '%c %s:t:r %a']
    let g:quickrun_config.java['hook/output_encode/encoding'] = 'cp932:utf-8'
    let g:quickrun_config.java['tempfile']                    = printf('%s/{tempname()}.java', $TMP)
    " Haskell
    "@Bugs('cannot run rightly')
    let g:quickrun_config['haskell'] = {
    \   'exec' : '%c %o `cygpath -w "%s:p"` | tr -d "\\r"'
    \}
    " TypeScript
    let g:quickrun_config['typescript'] = {
    \   'exec' : ['%c %o "`cygpath -w %s:p`"', 'node "`cygpath -w %s:p:r`.js"']
    \}
    " HTML
    let g:quickrun_config.html['command'] = 'cygstart'
    " LaTex
    let g:quickrun_config.tex['exec'] = '%c %o "`cygpath -w %s:r`" | tr -d "\r"'
    " Clojure
    let g:quickrun_config.clojure['exec'] = '%c %o "`cygpath -w %s`" | tr -d "\r"'
endfunction " }}}

" Delete otiose lines
function! vimrc#plugins#weblio_filter(output) abort " {{{
    let l:lines = split(a:output, "\n")
    return join(l:lines[17 : ], "\n")
endfunction " }}}

" Open tweetvim by private account
function! vimrc#plugins#twitter_private() abort " {{{
    if !exists('g:vimrc.private["twitter"]["priv_ac"]')
        call s:Msg.error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
        return
    endif

    execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
    let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['priv_ac']

    TweetVimHomeTimeline
endfunction " }}}

" Open tweetvim_say by private account
function! vimrc#plugins#tweet_private() abort " {{{
    if !exists('g:vimrc.private["twitter"]["priv_ac"]')
        call s:Msg.error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
        return
    endif

    execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
    TweetVimSay

    "@Incomplete('wait sync here')
    "execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['curr_ac']
endfunction " }}}

" Open tweetvim by public account
function! vimrc#plugins#twitter_public() abort " {{{
    if !exists("g:vimrc.private['twitter']['publ_ac']")
        call s:Msg.error("Not set env variable => g:vimrc.private['twitter']['publ_ac']")
        return
    endif

    execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
    let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['publ_ac']

    TweetVimHomeTimeline
endfunction " }}}

" Open tweetvim_say by public account
function! vimrc#plugins#tweet_public(...) abort " {{{
    if !exists('g:vimrc.private["twitter"]["publ_ac"]')
        call s:Msg.error('Not set env variable => g:vimrc.private["twitter"]["publ_ac"]')
        return
    endif

    execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
    if len(a:000) is 0
        TweetVimSay
    else
        execute 'TweetVimCommandSay' join(a:000)
    endif

    "@Incomplete('wait here')
    "execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['curr_ac']
endfunction " }}}

" Run `haskdogs` command as a job
function! vimrc#plugins#execute_haskdogs_async() abort " {{{
    if exists('s:haskdogs_job')
        echomsg 'haskdogs is skipped (haskdogs is already running at now)'
        return
    endif
    echomsg 'haskdogs is started'

    let git_top_dir = system('git rev-parse --show-toplevel')[:-2] " [:-2] removes a line break
    let dot_git     = git_top_dir . '/.git' " This is a file (not a directory) if here is a git submodule
    let ctags_path  = isdirectory(dot_git) ? dot_git . '/tags'
        \                                  : './tags'

    let previous_current_dir = execute('pwd')[1:]
    execute ':tcd' git_top_dir

    let cmd = ['haskdogs', '--hasktags-args', '"--ignore-close-implementation', '--tags-absolute', '--ctags', printf('--file=%s"', ctags_path)]
    echomsg 'haskdogs will make a ctags on ' . git_top_dir

    let s:haskdogs_job = s:Job.start(cmd, {
        \ 'on_exit': { _ -> [
            \ s:Msg.echomsg('None', 'haskdogs might make a ctags to ' . ctags_path),
            \ execute('unlet s:haskdogs_job')
        \ ]}
    \ })

    execute ':tcd' previous_current_dir
    return s:haskdogs_job
endfunction " }}}

" Execute vimrc#plugins#execute_haskdogs_async(),
" and Append the tags of eta libraries to it
" after it is executed
function! vimrc#plugins#execute_haskdogs_in_eta_async() abort " {{{
    function! s:body_of_execute_haskdogs_in_eta_async() abort
        let git_top_dir = system('git rev-parse --show-toplevel')[:-2] " [:-2] removes a line break
        let ctags_path  = isdirectory(git_top_dir) ? git_top_dir . '/.git/tags'
        \                                          : './tags'
        if !isdirectory($HOME . '/git/eta')
            echo '~/git/eta is not found'
            return
        elseif !filereadable(ctags_path)
            echo "'" . ctags_path . "' is not found"
            return
        endif
        call system('hasktags ~/git/eta/libraries --ignore-close-implementation --tags-absolute --ctags -f /tmp/eta_tags && cat /tmp/eta_tags >> ' . ctags_path)
    endfunction
    call vimrc#plugins#execute_haskdogs_async(function('s:body_of_execute_haskdogs_in_eta_async'))
endfunction " }}}

function! vimrc#plugins#espeak_say(msg) abort " {{{
    call  vimrc#plugins#espeak_doesnt_say()

    let options = printf('-s %d -v %s ',
        \ g:espeak_speed,
        \ g:espeak_voice,
    \)
    let cmd = 'espeak ' . options . shellescape(printf('"%s"', a:msg))
    "echo system(cmd)
    let s:espeak_job = s:Job.start(cmd)
endfunction

function! vimrc#plugins#espeak_doesnt_say() abort
    if exists('s:espeak_job')
        silent! call s:espeak_job.stop()
    endif
endfunction " }}}

" let s:read_to_quickfix_it {{{

let s:read_to_quickfix_it = { cmd ->
    \ s:Job.start(cmd, {
        \ 'on_stdout': function('s:caddexpr_on_stdout'),
        \ 'on_stderr': function('s:caddexpr_on_stdout'),
    \ })
\ }

" }}}
" TODO: Unify to one
function! vimrc#plugins#run_stack_quickfix(stack_subcmd) abort " {{{
    CClear
    let stack_cmd = ['stack'] + split(a:stack_subcmd, ' ')
    call s:read_to_quickfix_it(stack_cmd)
    copen
endfunction " }}}
function! vimrc#plugins#run_gradle_quickfix(gradle_subcmd) abort " {{{
    CClear
    let gradle_cmd = ['gradle', '--console=plain'] + split(a:gradle_subcmd, ' ')
    call s:read_to_quickfix_it(gradle_cmd)
    copen

    " Avoid
    augroup ScalaCompileWatch
        autocmd!
        autocmd VimLeave * call vimrc#plugins#stop_scala_compile_watch_quickfix()
    augroup END
endfunction " }}}

" NOTE: This requires to add sbt-launch.jar to $PATH
function! vimrc#plugins#run_scala_compile_watch_quickfix(sbt_subcmd) abort " {{{
    CClear
    call vimrc#plugins#stop_scala_compile_watch_quickfix() " Avoid running more processes
    " Run sbt directly for killing the sbt process (vimrc#plugins#stop_scala_compile_watch_quickfix)
    let sbt_launcher = system('which sbt-launch.jar')[0:-2]
    let sbt_cmd = ['java', '-jar', sbt_launcher, '-Dsbt.log.noformat=true', '~test:compile'] + split(a:sbt_subcmd, ' ')
    let s:sbt_compile_watch_job = s:read_to_quickfix_it(sbt_cmd)
    copen
endfunction " }}}

function! vimrc#plugins#stop_scala_compile_watch_quickfix() abort " {{{
    if get(s:, 'sbt_compile_watch_job', v:null) isnot v:null
        call s:sbt_compile_watch_job.stop()
        let s:sbt_compile_watch_job = v:null
        cclose
    endif
endfunction " }}}

function! vimrc#plugins#ctags_auto() abort " {{{
    if exists('s:ctags_job')
        echomsg 'ctags is skipped (ctags is already running at now)'
        return s:ctags_job
    endif
    echomsg 'ctags is started'

    let git_top_dir = system('git rev-parse --show-toplevel')[:-2] " [:-2] removes a line break
    let dot_git     = git_top_dir . '/.git' " This is a file (not a directory) if here is a git submodule
    let ctags_path  = isdirectory(dot_git)
        \ ? dot_git . '/tags'
        \ : './tags'

    let cmd = ['ctags', '--tag-relative=yes', '--recurse', '--sort=yes', '-f', fnameescape(ctags_path)]
    let s:ctags_job = s:Job.start(cmd, {
        \ 'on_exit' : { _ -> [
            \ s:Msg.echomsg('None', 'ctags generated ctags to ' . fnameescape(ctags_path)),
            \ execute('unlet s:ctags_job')
        \ ]}
    \ })
    return s:ctags_job
endfunction " }}}

function! vimrc#plugins#grep_those(...) abort " {{{
    CClear
    call s:List.map(a:000, { word ->
        \ execute('grepadd ' . word . ' %', 'silent!')
    \ })
    copen
endfunction " }}}

function! vimrc#plugins#open_this_file_in_gui() abort " {{{
    let file = expand('%:p')
    call s:Job.start(['gonvim', file])
endfunction " }}}

function! vimrc#plugins#delete_lines(...) abort " {{{
    let lineNums = s:List.map(a:000, { x -> str2nr(x) })
    let lineNums = s:List.sort(lineNums, { x, y -> x - y })
    while v:true
        if empty(lineNums)
            break
        endif
        let lineNum = s:List.shift(lineNums)
        let lineNums = s:List.map(lineNums, { x -> x - 1 })
        execute lineNum 'delete'
    endwhile
endfunction " }}}

function! vimrc#plugins#exec_at_this_buffer_dir(cmd) abort " {{{
    let current_dir = execute('pwd')[1:]
    let buffuer_dir = expand('%:p:h')
    if !isdirectory(buffuer_dir)
        call s:Msg.error('vimrc#plugins#exec_at_this_buffer_dir: the buffer was not a file, :pwd directory was used instead.')
        let buffuer_dir = current_dir
    endif

    execute 'tcd' buffuer_dir
    execute a:cmd
    execute 'tcd' current_dir
endfunction " }}}
