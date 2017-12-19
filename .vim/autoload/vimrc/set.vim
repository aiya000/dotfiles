" Maybe this is specified to 'tabline' with a pattern like `set tabline=%!this_function()`.
"
" See ':h hl-User1..9' for what is the pattern of '%n*string%*' (n is a naturalnumer),
" and below augroup 'HighlightPref'.
function! vimrc#set#tabline_as_statusline() abort " {{{
    let tabs = s:tabline_tabs()
    return '%1*[PWD=%{getcwd()}]%*'
        \. tabs
        \. '%2*%{vimrc#set#tabline_tags_if_present()}%*'
endfunction " }}}

function! s:tabline_tabs() abort " {{{
    let current_tabnr = tabpagenr()
    let tabs = map(range(1, tabpagenr('$')), {_, x -> x is current_tabnr ? '%#TabLineSel#o' : '%#TabLine#o'})
    return join(tabs, '') . '%#TabLineFill#'
endfunction " }}}

function! vimrc#set#tabline_tags_if_present() abort " {{{
    let tags = tagfiles()
    return empty(tags)    ? ''
        \: len(tags) is 1 ? ('[Tag=' . tags[0] . ']')
        \                 : ('[Tag=' . tags[0] . ' +]')
endfunction " }}}
