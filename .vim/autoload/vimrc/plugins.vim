let s:V = vital#vimrc#new()
let s:M = s:V.import('Vim.Message')
let s:Job = s:V.import('System.Job')

" Append quickrun config for unix
function! vimrc#plugins#append_config_quickrun_unix() abort " {{{
    " C#
    let g:quickrun_config['cs'] = {'command' : 'mcs'}
    " HTML
    let g:quickrun_config.html['command'] = 'xdg-open'
    " Haskell
    if executable('stack')
        "TODO: Why this specific is invalid ?
        "let g:quickrun_config['haskell'] = {'command' : 'stack runghc'}
        let g:quickrun_config['haskell'] = {'exec' : 'stack runghc -- %s'}
    endif
endfunction " }}}

" Append quickrun config for windows
function! vimrc#plugins#append_config_quickrun_windows() abort " {{{
    " C#
    let g:quickrun_config['cs'] = {
    \   'command' : 'csc.exe',
    \   'hook/output_encode/encoding' : 'cp932:utf-8'
    \}
    " Java
    let g:quickrun_config.java['hook/output_encode/encoding'] = 'cp932:utf-8'
    " HTML
    "TODO: Don't specify firefox.exe
    let g:quickrun_config.html['command'] = 'firefox.exe'
    let g:quickrun_config.html['exec']    = '%c file://%s:p'
endfunction " }}}

" Append quickrun config for cygwin
function! vimrc#plugins#append_config_quickrun_cygwin() abort " {{{
    "NOTE: for vimproc runner problem
    let g:quickrun_config._['runner'] = 'system'
    " C#
    let g:quickrun_config['cs'] = {
    \   'command' : 'csc.exe',
    \   'hook/output_encode/encoding' : 'cp932:utf-8'
    \}
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
        call s:M.error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
        return
    endif

    execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
    let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['priv_ac']

    TweetVimHomeTimeline
endfunction " }}}

" Open tweetvim_say by private account
function! vimrc#plugins#tweet_private() abort " {{{
    if !exists('g:vimrc.private["twitter"]["priv_ac"]')
        call s:M.error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
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
        call s:M.error("Not set env variable => g:vimrc.private['twitter']['publ_ac']")
        return
    endif

    execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
    let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['publ_ac']

    TweetVimHomeTimeline
endfunction " }}}

" Open tweetvim_say by public account
function! vimrc#plugins#tweet_public(...) abort " {{{
    if !exists('g:vimrc.private["twitter"]["publ_ac"]')
        call s:M.error('Not set env variable => g:vimrc.private["twitter"]["publ_ac"]')
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

" Start the job of `watchexec -w . snowtify {test|build}`
function! vimrc#plugins#start_snowtify_watch(subcommand_or_empty) abort " {{{
    let subcommand = empty(a:subcommand_or_empty)
    \                ? 'test'
    \                : a:subcommand_or_empty
    let s:snowtify_watch_job = s:Job.start('watchexec -w . snowtify ' . subcommand)
endfunction " }}}

" Kill the job of vimrc#plugins#start_snowtify_watch()
function! vimrc#plugins#stop_snowtify_watch() abort " {{{
    if exists('s:snowtify_watch_job')
        if s:snowtify_watch_job.status() ==# 'run'
            call s:snowtify_watch_job.stop()
            echo 'snowtify watch is killed'
        else
            echo 'snowtify watch is alerady killed'
        endif
        unlet s:snowtify_watch_job
    else
        echo 'snowtify watch maybe not running'
    endif
endfunction " }}}

" Run `haskdogs` command as a job
function! vimrc#plugins#execute_haskdogs_async() abort " {{{
    if exists('s:haskdogs_job')
        echomsg 'haskdogs is skipped (haskdogs is already running at now)'
        return
    endif

    let git_top_dir = system('git rev-parse --show-toplevel')[:-2] " [:-2] removes a line break
    let ctags_path  = isdirectory(git_top_dir) ? git_top_dir . '/.git/tags'
    \                                          : './tags'

    let s:haskdogs_job =
    \   s:Job.start(printf('haskdogs --hasktags-args "--ignore-close-implementation --tags-absolute --ctags --file=%s"', ctags_path), {
    \      'on_exit' : {_, __, ___ -> [
    \           s:M.echo('None', 'haskdogs may generated ctags to ' . ctags_path),
    \           execute('unlet s:haskdogs_job')
    \       ]}
    \   })
endfunction " }}}
