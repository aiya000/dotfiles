vim.cmd([[
highlight ftHaskellHeadSpace cterm=underline ctermfg=DarkGray gui=underline guifg=Black
syntax match ftHaskellHeadSpace '^\s\+'

highlight ftHaskellKeywordUndefined cterm=underline ctermfg=Red gui=underline guifg=Red
syntax keyword ftHaskellKeywordUndefined undefined

highlight ftHaskellGHCExtensionKeyword ctermfg=cyan guifg=cyan

syntax keyword ftHaskellGHCExtensionKeyword pattern
syntax keyword ftHaskellGHCExtensionKeyword family
syntax keyword ftHaskellGHCExtensionKeyword foreign
syntax keyword ftHaskellGHCExtensionKeyword export
syntax keyword ftHaskellGHCExtensionKeyword unsafe
syntax keyword ftHaskellGHCExtensionKeyword role
syntax keyword ftHaskellGHCExtensionKeyword nominal
syntax keyword ftHaskellGHCExtensionKeyword representational
syntax keyword ftHaskellGHCExtensionKeyword forall
syntax keyword ftHaskellGHCExtensionKeyword via
syntax keyword ftHaskellGHCExtensionKeyword anyclass
syntax keyword ftHaskellGHCExtensionKeyword stock
syntax keyword ftHaskellGHCExtensionKeyword newtype

" eta
highlight ftHaskellEtaKeyword ctermfg=darkred guifg=darkred
"TODO: Don't highlight /^java/ (e.g. a function)
syntax keyword ftHaskellEtaKeyword java
syntax keyword ftHaskellForeignFunctionInterface safe
]])