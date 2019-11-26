highlight ftHaskellHeadSpace cterm=underline ctermfg=DarkGray gui=underline guifg=Black
syntax match ftHaskellHeadSpace '^\s\+'

highlight ftHaskellKeywordUndefined cterm=underline ctermfg=Red gui=underline guifg=Red
syntax keyword ftHaskellKeywordUndefined undefined

" TODO: できてない
highlight ftHaskellUnderscore cterm=bold ctermfg=DarkCyan gui=bold guifg=DarkCyan
syntax match ftHaskellUnderscore '_'

highlight ftHaskellGHCExtensionKeyword ctermfg=cyan guifg=cyan

" PatternSynonyms
highlight default link ftHaskellPatternSynonyms ftHaskellGHCExtensionKeyword
syntax keyword ftHaskellPatternSynonyms pattern

" TypeFamilies
highlight link ftHaskellTypeFamilies ftHaskellGHCExtensionKeyword
syntax keyword ftHaskellPatternSynonyms family

" ForeignFunctionInterface
highlight link ftHaskellForeignFunctionInterface ftHaskellGHCExtensionKeyword
syntax keyword ftHaskellForeignFunctionInterface foreign
syntax keyword ftHaskellForeignFunctionInterface export
syntax keyword ftHaskellForeignFunctionInterface unsafe

" RoleAnnotations
highlight link ftHaskellRoleAnnotations ftHaskellGHCExtensionKeyword
syntax keyword ftHaskellRoleAnnotations role
syntax keyword ftHaskellRoleAnnotations nominal
syntax keyword ftHaskellRoleAnnotations representational

" eta
highlight ftHaskellEtaKeyword ctermfg=darkred guifg=darkred
"TODO: Don't highlight /^java/ (e.g. a function)
syntax keyword ftHaskellEtaKeyword java
syntax keyword ftHaskellForeignFunctionInterface safe

" anything
highlight link ftHaskellAnythingExtension ftHaskellGHCExtensionKeyword
syntax keyword ftHaskellAnythingExtension forall
syntax keyword ftHaskellAnythingExtension via
