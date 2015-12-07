highlight syntaxVimMyHint cterm=standout ctermfg=DarkYellow gui=bold guifg=#ef5939
call matchadd('syntaxVimMyHint', '\s*"\zs@\w\+(.*)\ze')
