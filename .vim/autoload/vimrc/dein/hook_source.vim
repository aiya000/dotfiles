function! vimrc#dein#hook_source#gina() abort
    call gina#custom#mapping#nmap('status' , 'o'     , ':<C-u>call gina#action#call("edit")<CR>'                , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('status' , 'Q'     , ':<C-u>bdelete!<CR>'                                     , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('status' , 'cc'    , ':<C-u>Gina commit --verbose<CR>'                        , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('status' , 'ca'    , ':<C-u>Gina commit --verbose --amend<CR>'                , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('status' , '<C-j>' , ':<C-u>call gina#action#call("diff:preview:bottom")<CR>' , {'noremap': 1 , 'silent': 1})
    call gina#custom#mapping#nmap('status' , '<C-r>' , ':<C-u>Gina status<CR>'                                  , {'noremap': 1 , 'silent': 1})
endfunction
