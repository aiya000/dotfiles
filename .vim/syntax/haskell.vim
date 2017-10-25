highlight ftHaskellHeadSpace cterm=underline ctermfg=DarkGray gui=underline guifg=Black
syntax match ftHaskellHeadSpace '^\s\+'
highlight ftHaskellKeywordUndefined cterm=underline ctermfg=Red gui=underline guifg=Red
syntax keyword ftHaskellKeywordUndefined undefined

" PatternSynonyms
highlight ftHaskellKeywordPattern ctermfg=cyan guifg=cyan
syntax keyword ftHaskellKeywordPattern pattern
