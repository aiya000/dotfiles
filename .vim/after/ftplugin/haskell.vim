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
    autocmd BufWritePost *.hs call s:haskdogs()
augroup END

function! s:haskdogs() abort
    if exists('s:ftplugin_haskell_haskdogs_job')
        echomsg 'haskdogs is skipped (haskdogs is already running at now)'
        return
    endif

    let git_top_dir = system('git rev-parse --show-toplevel')[:-2] " [:-2] removes a line break
    let ctags_path  = isdirectory(git_top_dir) ? git_top_dir . '/.git/tags'
    \                                          : './tags'

    let s:ftplugin_haskell_haskdogs_job =
    \   s:Job.start(printf('haskdogs --hasktags-args "--ignore-close-implementation --tags-absolute --ctags --file=%s"', ctags_path), {
    \      'on_exit' : {_, __, ___ -> [
    \           s:M.echo('None', 'haskdogs may generated ctags to ' . ctags_path),
    \           execute('unlet s:ftplugin_haskell_haskdogs_job')
    \       ]}
    \   })
endfunction
