let s:V   = vital#vimrc#new()
let s:Job = s:V.import('System.Job')
let s:M   = s:V.import('Vim.Message')

let b:undo_ftplugin = 'setl ' . join([
\   'ts<',
\   'sw<',
\   'et<',
\   'conceallevel<'
\])

setl ts=2 sw=2 et conceallevel=0
let &commentstring = ' -- %s'

nnoremap <buffer><silent> <leader><leader>r :<C-u>echo 'stack test is started'<CR>:QuickRun stack_test<CR>
nnoremap <buffer><silent> <leader><leader>R :<C-u>terminal stack test<CR>
nnoremap <buffer><silent> <leader><leader>S :<C-u>Snowtify<CR>

augroup FtpluginHaskell
    autocmd!
    autocmd BufWritePost *.hs HaskDogs
augroup END
