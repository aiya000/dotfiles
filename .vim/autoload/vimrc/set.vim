"@See('http://sourceforge.jp/magazine/07/11/06/0151231')
" View loaded tags path
function! vimrc#set#tag_load_status() " {{{
    let l:STATUS_FORMAT = '[Tag(%s)]' | lockvar l:STATUS_FORMAT
    let l:tags_status   = empty(tagfiles()) ? 'none'
    \                                       : 'loaded'
    return printf(l:STATUS_FORMAT, l:tags_status)
endfunction " }}}

"@See('http://d.hatena.ne.jp/thinca/20111204/1322932585')
" Sugoi view tabline
function! vimrc#set#with_delimitter_tab_line() " {{{
    let l:titles     = map(range(1, tabpagenr('$')), 'vimrc#set#tabpage_label(v:val)')
    let l:delimitter = ' | '
    let l:tabpages   = l:delimitter . join(l:titles, l:delimitter) . l:delimitter . '%#TabLineFill#%T'
    return l:tabpages
endfunction " }}}
function! vimrc#set#tabpage_label(n) " {{{
    let l:title = gettabvar(a:n, 'title')
    if l:title !=# ''
        return l:title
    endif

    let l:bufnrs = tabpagebuflist(a:n)
    let l:hi = a:n is tabpagenr() ? '%#TabLineSel#' : '%#TabLine#'

    let l:no = len(l:bufnrs)
    if l:no is 1
        let l:no = ''
    endif

    let l:mod = len(filter(copy(l:bufnrs), 'getbufvar(v:val, "&modified")')) ? '+' : ''
    let l:sp = (l:no . l:mod) ==# '' ? '' : ' '

    let l:curbufnr = l:bufnrs[tabpagewinnr(a:n) - 1]
    let l:fname = pathshorten(bufname(l:curbufnr))
    if l:fname ==# ''
        let l:fname = '[ NoName ]'
    endif

    let l:label = l:no . l:mod . l:sp . l:fname
    return '%' . a:n . 'T' . l:hi . l:label . '%T%#TabLineFill#'
endfunction " }}}
