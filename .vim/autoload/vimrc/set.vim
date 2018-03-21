" Maybe this is specified to 'tabline' with a pattern like `set tabline=%!this_function()`.
"
" See ':h hl-User1..9' for what is the pattern of '%n*string%*' (n is a naturalnumer),
" and below augroup 'HighlightPref'.
function! vimrc#set#tabline_as_statusline() abort " {{{
    return '%1*[PWD=%{getcwd()}]%*'
        \. '%2*%{vimrc#set#tabline_tags_if_present()}%*'
        \. '%3*%{vimrc#set#tabline_marks_if_present()}%*'
        \. "%4*%{'[' . tabpagenr('$') . ']'}%*"
endfunction " }}}

function! vimrc#set#tabline_tags_if_present() abort " {{{
    let tags = tagfiles()
    return empty(tags)    ? ''
        \: len(tags) is 1 ? ('[Tag=' . tags[0] . ']')
        \                 : ('[Tag=' . tags[0] . ' +]')
endfunction " }}}

function! vimrc#set#tabline_marks_if_present() abort " {{{
    let marks = s:get_buf_marks()
    return empty(marks) ? ''
        \               : '[Mark=' . join(marks, '') . ']'
endfunction " }}}

function! s:get_buf_marks() abort " {{{
    let lines       = split(execute(':marks'), "\n")[2:]  " List only the lines like ` a 30 48 let lines = split...`
    let all_marks   = map(lines, {_, x -> x[1]})
    let local_marks = filter(all_marks, {_, x -> match(x, '\l', 0, 0) is 0})
    return local_marks
endfunction " }}}
