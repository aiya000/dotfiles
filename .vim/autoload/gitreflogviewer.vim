function! gitreflogviewer#git_reflog_viewer(args)
    enew!
    setl buftype=nofile
    call s:read_git_reflog(a:args)
    set  filetype=gitreflogviewer
    set  foldmethod=expr
endfunction

function! s:read_git_reflog(args)
    let s:system = exists('*vimproc#system') ? function('vimproc#system')
    \                                        : function('system')
    let l:z = @z
    let @z  = s:system('git reflog ' . a:args)
    normal! "zP
    let @z  = l:z
    normal! gg
endfunction
