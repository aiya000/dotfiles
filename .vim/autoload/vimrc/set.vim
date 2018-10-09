let s:List = vital#vimrc#import('Data.List')

" Maybe this is specified to 'tabline' with a pattern like `set tabline=%!this_function()`.
"
" See ':h hl-User1..9' for what is the pattern of '%n*string%*' (n is a naturalnumer),
" and below augroup 'HighlightPref'.
function! vimrc#set#tabline() abort " {{{
    let language_client_status =
        \ get(g:vimrc, 'language_client', {'has_started': v:false})['has_started']
            \ ? '%6*%{LanguageClient_statusLine()}%*'
            \ : ''
    return '%1*[%{tabpagenr("$")}]%* '
        \. s:tabs() . ' => '
        \. '%2*[PWD=%{getcwd()}]%*'
        \. '%3*%{vimrc#set#tabline_tags_if_present()}%*'
        \. "%4*%{'[' . tabpagenr('$') . ']'}%*"
        \. '%5*%{vimrc#set#tabline_marks_if_present()}%*'
        \. '%6*%{get(g:, "ale_enabled", 0) ? "[ale]" : ""}%*'
        \. language_client_status
endfunction " }}}

function! vimrc#set#tabline_tags_if_present() abort " {{{
    let tags = tagfiles()
    return empty(tags)    ? ''
        \: len(tags) is 1 ? ('[Tag=' . tags[0] . ']')
        \                 : ('[Tag=' . tags[0] . ' +]')
endfunction " }}}

function! vimrc#set#tabline_marks_if_present() abort " {{{
    let marks = s:get_buf_marks()
    return empty(marks)
        \ ? ''
        \ : '[Mark=' . join(marks, '') . ']'
endfunction " }}}

function! s:get_buf_marks() abort " {{{
    let lines       = split(execute(':marks'), "\n")[2:]  " List only the lines like ` a 30 48 let lines = split...`
    let all_marks   = map(lines, {_, x -> x[1]})
    let local_marks = filter(all_marks, {_, x -> match(x, '\l', 0, 0) is 0})
    return local_marks
endfunction " }}}

" NOTE: http://d.hatena.ne.jp/thinca/20111204/1322932585
function! s:tabs() " {{{
    let titles = s:List.map(range(1, tabpagenr('$')), {
        \ tabnr -> vimrc#set#tabpage_label(tabnr)
    \ })
    return join(titles) . '%#TabLineFill#%T'
endfunction " }}}

function! vimrc#set#tabpage_label(tabnr) " {{{
    let title = gettabvar(a:tabnr, 'title')
    if title != ''
        return title
    endif
    let focused_winnr = tabpagewinnr(a:tabnr)
    let curbufnr = tabpagebuflist(a:tabnr)[focused_winnr - 1]
    let file_name = fnamemodify(bufname(curbufnr), ':t')
    let file_name =
        \  (file_name == '')     ? '[NoName]'
        \: (len(file_name) > 20) ? (file_name[0:7] . '...' . file_name[-10:-1])
        \: file_name

    " Please see `:h TabLineSel` and `:h TabLine`
    let window_num = '[' . len(tabpagebuflist(a:tabnr)) . ']'
    let label_of_a_buf = s:is_stayed_tab(a:tabnr)
        \ ? '%#TabLineSel#[* ' . s:get_mod_mark_for_window(focused_winnr) . window_num . file_name . ' *]'
        \ : '%#TabLine#[' . s:get_mod_mark_for_tab(a:tabnr) . window_num . file_name . ']'

    return '%' . a:tabnr . 'T' . label_of_a_buf . '%T%#TabLineFill#'
endfunction " }}}

" Do you staying the specified tab?
function! s:is_stayed_tab(tabnr) abort " {{{
    return a:tabnr is tabpagenr()
endfunction " }}}

" Return '+' if the buffer of the specified window is modified
function! s:get_mod_mark_for_window(winnr) abort " {{{
    return getbufvar(winbufnr(a:winnr), '&modified') ? '+' : ''
endfunction " }}}

" Return '+' if one or more the modified buffer is existent on the specified tab
function! s:get_mod_mark_for_tab(tabnr) abort " {{{
    let modified_buffer = s:List.find(tabpagebuflist(a:tabnr), v:null, { bufnr_at_tab ->
        \ getbufvar(bufnr_at_tab, '&modified')
    \ })
    return (modified_buffer is v:null) ? '' : '+'
endfunction " }}}
