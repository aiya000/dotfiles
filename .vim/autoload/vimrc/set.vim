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
    " Please see `:h TabLineSel` and `:h TabLine`
    let highlight = a:tabnr is tabpagenr() ? '%#TabLineSel#' : '%#TabLine#'

    let bufnrs_at_current = tabpagebuflist(a:tabnr)
    let modified_buffers = s:List.filter(bufnrs_at_current, { bufnr ->
        \ getbufvar(bufnr, '&modified')
    \ })
    let window_num = '[' . len(tabpagebuflist(a:tabnr)) . ']'
    let mod_mark = (len(modified_buffers) is 0) ? '' : '+'
    let curbufnr = bufnrs_at_current[tabpagewinnr(a:tabnr) - 1]
    let file_name = fnamemodify(bufname(curbufnr), ':t')
    let file_name = (file_name == '') ? '[NoName]'
        \: (len(file_name) > 20) ? (file_name[0:7] . '...' . file_name[-10:-1])
        \: file_name
    let label_of_a_buf = '[' . mod_mark . window_num . file_name . ']'

    return '%' . a:tabnr . 'T' . highlight . label_of_a_buf . '%T%#TabLineFill#'
endfunction " }}}
