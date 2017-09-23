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
nnoremap <buffer><silent> <leader><leader>R :<C-u>sp<CR>:terminal stack test<CR>
nnoremap <buffer><silent> <leader><leader>S :<C-u>Snowtify<CR>

augroup FtpluginHaskell
    autocmd!
    autocmd BufWritePost *.hs call s:haskdogs()
augroup END

function! s:haskdogs() abort
    let git_top_dir = system('git rev-parse --show-toplevel')[:-2] " [:-2] removes a line break
    let ctags_path  = isdirectory(git_top_dir) ? git_top_dir . '/.git/tags'
    \                                          : './tags'
    call s:Job.start(printf('haskdogs --hasktags-args "--ignore-close-implementation --tags-absolute --ctags --file=%s"', ctags_path), {
    \   'on_exit' : {_, __, ___ -> s:M.echo('None', 'haskdogs may generated ctags to ' . ctags_path)}
    \})
    "\   'on_exit' : {_, __, ___ ->
    "\       executable('notify-send')
    "\         ? system(printf('notify-send "ftplugin/haskell" "haskdogs may generated ctags to %s"', ctags_path))
    "\         : s:M.echo('None', 'haskdogs may generated ctags to ' . ctags_path)
    "\   }
endfunction
