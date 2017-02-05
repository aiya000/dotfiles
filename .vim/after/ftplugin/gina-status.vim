nnoremap <buffer><silent> Q  :<C-u>bdelete!<CR>
"nnoremap <buffer><silent> cc :<C-u>Gina commit --verbose<CR>
"nnoremap <buffer><silent> ca :<C-u>Gina commit --verbose --amend<CR>
"FIXME: Remove this if gina commit is fixed
nnoremap <buffer><silent> cc :<C-u>terminal git commit --verbose<CR>
nnoremap <buffer><silent> ca :<C-u>terminal git commit --verbose --amend<CR>
