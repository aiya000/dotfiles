let b:undo_ftplugin = 'setl ' . join([
  \ 'tabstop<',
  \ 'shiftwidth<',
  \ 'expandtab<',
  \ 'completefunc<',
  \ 'commentstring<',
  \ 'conceallevel<',
\ ])

setl tabstop=4 shiftwidth=4 expandtab
setl completefunc=github_complete#complete
let &commentstring = '<!-- %s -->'
set conceallevel=0

let s:git_root = system('git rev-parse --show-toplevel')[:-2]
if filereadable(s:git_root .. '/.textlintrc')
  let g:ale_fixers['markdown'] = ['textlint']
endif

nnoremap <silent><buffer> <localleader>r :<C-u>PrevimOpen<CR>
nnoremap <silent><buffer> <localleader><localleader>d :<C-u>w<CR>:!doctoc %<CR>:edit %<CR>
nnoremap <silent><buffer> <localleader><localleader>r :<C-u>call <SID>open_grip()<CR>
nnoremap <silent><buffer> <localleader><localleader>R :<C-u>silent !shiba % > /dev/null 2>&1 &<CR>
"nnoremap <silent><buffer> <localleader>f :<C-u>!textlint --fix <C-r>=expand('%:p')<CR><CR>
nmap <silent><buffer> <C-l> <C-[>:syntax sync fromstart<CR>

vnoremap <silent><buffer> i{ :<C-u>call <SID>organize_this_table()<CR>

syntax sync fromstart

augroup FtpluginMarkdown
  autocmd!
augroup END

function s:open_grip() abort
  let cmd = printf(
    \ 'grip --pass %s --browser %s',
    \ g:vimrc.github.access_token,
    \ expand('%:p'),
  \ )
  call vimrc#open_terminal_as('none', 'horizontal', cmd)
  hide
endfunction

"TODO: Don't remove `:--`, `:-:`, `--:`
function! s:organize_this_table() abort
  execute 'normal' "viio\<Esc>j"
  execute 's/-//g'
  execute 'normal' "vii:Alignta => \|\<CR>"
  normal! j
  execute 's/[^\|]/-/g'
  normal! k
endfunction

function s:setup_markdown2ctags_if_ctags_is_not_latest(stdout, _stderr) abort
  for lang in a:stdout
    if lang ==# 'Markdown'
      return
    endif
  endfor

  augroup FtpluginMarkdown
    autocmd BufEnter,BufWinEnter *.md
      \  call denite#custom#var('outline', 'command', ['markdown2ctags.py'])
      \| call denite#custom#var('outline', 'options', ['--sort=no'])
      \| call denite#custom#var('outline', 'file_opt', '-f')
  augroup END
endfunction

call job_start(
  \ ['ctags', '--list-languages'],
  \ vimrc#job#get_basic_options_completes_with(
    \ function('s:setup_markdown2ctags_if_ctags_is_not_latest')
  \ )
\ )
