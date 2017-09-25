if exists('b:did_indent')
    finish
endif

source $VIMRUNTIME/indent/vim.vim
set indentexpr=GetVimIndentOverwrite()

function! GetVimIndentOverwrite()
    let l:lnum = prevnonblank(v:lnum - 1)

    if getline(v:lnum) !~# '^\s*\\'
        while (l:lnum > 0) && (getline(l:lnum) =~# '^\s*\\')
            let l:lnum = l:lnum - 1
        endwhile
    endif

    if l:lnum is 0
        return 0
    endif

    let l:ind = indent(l:lnum)
    if (getline(v:lnum) =~# '^\s*\\') && (v:lnum > 1) && (getline(l:lnum) !~# '^\s*\\')
        if exists('g:vim_indent_cont')
            let l:ind = l:ind + g:vim_indent_cont
        else
            "let l:ind = l:ind + &sw * 3
        endif
    elseif getline(l:lnum) =~# '\(^\||\)\s*\(if\|wh\%[ile]\|for\|try\|cat\%[ch]\|fina\%[lly]\|fu\%[nction]\|el\%[seif]\)\>'
        let l:ind = l:ind + &sw
    elseif (getline(l:lnum) =~# '^\s*aug\%[roup]') && (getline(l:lnum) !~# '^\s*aug\%[roup]\s*!\=\s\+END')
        let l:ind = l:ind + &sw
    endif

    let l:line = getline(l:lnum)
    let l:i    = match(l:line, '[^\\]|\s*\(ene\@!\)')
    if (l:i > 0) && (l:line !~# '^\s*au\%[tocmd]')
        if !has('syntax_items') || synIDattr(synID(l:lnum, l:i + 2, 1), 'name') !~# '\(Comment\|String\)$'
            let l:ind = l:ind - &sw
        endif
    endif

    if getline(v:lnum) =~ '^\s*\(ene\@!\|cat\|fina\|el\|aug\%[roup]\s*!\=\s\+END\)'
        let l:ind = l:ind - &sw
    endif

    return l:ind
endfunction

let b:did_indent = 1
