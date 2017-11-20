highlight ftHaskellHeadSpace cterm=underline ctermfg=DarkGray gui=underline guifg=Black
syntax match ftHaskellHeadSpace '^\s\+'
highlight ftHaskellKeywordUndefined cterm=underline ctermfg=Red gui=underline guifg=Red
syntax keyword ftHaskellKeywordUndefined undefined

highlight ftHaskellGHCExtensionKeyword ctermfg=cyan guifg=cyan

" PatternSynonyms
highlight default link ftHaskellKeywordPattern ftHaskellGHCExtensionKeyword
syntax keyword ftHaskellKeywordPattern pattern

" TypeFamilies
highlight link ftHaskellKeywordFamily ftHaskellGHCExtensionKeyword
syntax keyword ftHaskellKeywordPattern family

" anything
highlight link ftHaskellAnythingExtension ftHaskellGHCExtensionKeyword
syntax keyword ftHaskellAnythingExtension forall
