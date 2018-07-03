function! vimrc#dein#hook_source#gina() abort " {{{
    call gina#custom#mapping#nmap('status' , 'o'     , ':<C-u>call gina#action#call("edit")<CR>'                , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('status' , 'Q'     , ':<C-u>bdelete!<CR>'                                     , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('status' , 'cc'    , ':<C-u>Gina commit --verbose<CR>'                        , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('status' , 'ca'    , ':<C-u>Gina commit --verbose --amend<CR>'                , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('status' , 'cf'    , ':<C-u>GCF<Space>'                                       , {'noremap': 1 , 'silent': 0})
    call gina#custom#mapping#nmap('status' , '<C-j>' , ':<C-u>call gina#action#call("diff:preview:bottom")<CR>' , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('status' , '<C-r>' , ':<C-u>Gina status<CR>'                                  , {'noremap': 1 , 'silent': 1})

    call gina#custom#mapping#nmap('commit' , 'ZZ'    , ':<C-u>wq<CR>' , {'noremap': 1   , 'silent': 1}) " Avoid to don't commit
    call gina#custom#mapping#nmap('commit' , '<C-j>' , '<C-o><Esc>'   , {'noremap': 1})

    call gina#custom#mapping#nmap('branch' , 'Q' , ':<C-u>bdelete!<CR>' , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('log'    , 'Q' , ':<C-u>bdelete!<CR>' , {'noremap': 1 , 'silent': 1})
endfunction " }}}

function! vimrc#dein#hook_source#LanguageClient_neovim() abort " {{{
    augroup DeinHookSourceLanguageClient
        autocmd!
        " Please see vimrc#set#tabline_as_statusline() for g:vimrc.language_client_neovim
        autocmd User LanguageClientStarted
            \ let s:prev_completefunc = &completefunc
            \|set completefunc=LanguageClient#complete
            \|let g:vimrc['language_client_neovim'] = {'has_started': v:true}
            \|redrawstatus
            \|echomsg 'hi'
        autocmd User LanguageClientStopped
            \ let &completefunc = s:prev_completefunc
    augroup END
endfunction " }}}

function! vimrc#dein#hook_source#operator_surround() abort " {{{
    let g:operator#surround#blocks = {
        \ '-' : [
            \ { 'block' : ['(', ')'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['(', ')', 'b'] },
            \ { 'block' : ['[', ']'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['[', ']', 'k'] },
            \ { 'block' : ['{', '}'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['{', '}', 'B'] },
            \ { 'block' : ['<', '>'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['<', '>', 'K'] },
            \ { 'block' : ['"', '"'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['"'] },
            \ { 'block' : ["'", "'"], 'motionwise' : ['char', 'line', 'block'], 'keys' : ["'"] },
            \ { 'block' : ['`', '`'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['`'] },
            \ { 'block' : [' ', ' '], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['  '] },
            \ { 'block' : ['( ', ' )'], 'motionwise' : ['char', 'line', 'block'], 'keys' : [' (', ' )'] },
            \ { 'block' : ['{ ', ' }'], 'motionwise' : ['char', 'line', 'block'], 'keys' : [' {', ' }'] },
            \ { 'block' : ['（', '）'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['（', ' ）', 'j(', 'j)', 'jb'] },
            \ { 'block' : ['「', '」'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['「', ' 」', 'j[', 'j]', 'jk'] },
            \ { 'block' : ['｛', '｝'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['｛', ' ｝', 'j{', 'j}', 'jB'] },
            \ { 'block' : ['＜', '＞'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['＜', ' ＞', 'j<', 'j>', 'jK'] },
        \ ],
    \ }
endfunction " }}}
