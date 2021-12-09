scriptencoding utf-8

let s:V = vital#vimrc#new()

let s:List = s:V.import('Data.List')
let s:Msg = s:V.import('Vim.Message')

function! vimrc#dein#hook_source#operator_surround() abort
  const basic_symbols = s:List.char_range('!', "'") + ['*', '&', '_', '|', '~', ':', '/'] " Excluding brackets () [] {} and ` for unique mappings ('keys')
  const basic_between = s:List.map(basic_symbols, { char ->
    \ #{ block: [char, char], motionwise: ['char', 'line', 'block'], keys: [char] }
  \ })

  const basic_html_tags = [
    \ #{ block: ['<p>', '</p>'], motionwise: ['char'], keys: ['[p'] },
    \ #{ block: ['<a>', '</a>'], motionwise: ['char'], keys: ['[a'] },
    \ #{ block: ['<div>', '</div>'], motionwise: ['char'], keys: ['[d'] },
    \ #{ block: ['<span>', '</span>'], motionwise: ['char'], keys: ['[s'] },
    \ #{ block: ['<h1>', '</h1>'], motionwise: ['char'], keys: ['[h1'] },
    \ #{ block: ['<h2>', '</h2>'], motionwise: ['char'], keys: ['[h2'] },
    \ #{ block: ['<h3>', '</h3>'], motionwise: ['char'], keys: ['[h3'] },
    \ #{ block: ['<h4>', '</h4>'], motionwise: ['char'], keys: ['[h4'] },
    \ #{ block: ['<h5>', '</h5>'], motionwise: ['char'], keys: ['[h5'] },
    \ #{ block: ['<ol>', '</ol>'], motionwise: ['char'], keys: ['[ol'] },
    \ #{ block: ['<ul>', '</ul>'], motionwise: ['char'], keys: ['[ul'] },
    \ #{ block: ['<li>', '</li>'], motionwise: ['char'], keys: ['[li'] },
  \ ]

  let g:operator#surround#blocks = {
    \ '-': [
      \ #{ block: ['(', ')'], motionwise: ['char', 'line', 'block'], keys: ['(', ')', 'p'] },
      \ #{ block: ['[', ']'], motionwise: ['char', 'line', 'block'], keys: [']', 'k'] },
      \ #{ block: ['{', '}'], motionwise: ['char', 'line', 'block'], keys: ['{', '}', 'P'] },
      \ #{ block: ['<', '>'], motionwise: ['char', 'line', 'block'], keys: ['<', '>', 'K'] },
      \ #{ block: [' ', ' '], motionwise: ['char', 'line', 'block'], keys: ['  '] },
      \ #{ block: ['`', '`'], motionwise: ['char', 'line', 'block'], keys: ['`', 'b'] },
      \ #{ block: ['（', '）'], motionwise: ['char', 'line', 'block'], keys: ['（', ' ）', 'j(', 'j)', 'jp'] },
      \ #{ block: ['｛', '｝'], motionwise: ['char', 'line', 'block'], keys: ['｛', ' ｝', 'j{', 'j}', 'jP'] },
      \ #{ block: ['「', '」'], motionwise: ['char', 'line', 'block'], keys: ['「', ' 」', 'j[', 'j]', 'jk'] },
      \ #{ block: ['〈', '〉'], motionwise: ['char', 'line', 'block'], keys: ['〈', ' 〉', 'jK'] },
      \ #{ block: ['『', '』'], motionwise: ['char', 'line', 'block'], keys: ['『', ' 』', 'j-k'] },
      \ #{ block: ['＜', '＞'], motionwise: ['char', 'line', 'block'], keys: ['『', ' 』', 'j<', 'j>'] },
      \ #{ block: ['**', '**'], motionwise: ['char'], keys: ['B'] },
    \ ] + basic_between,
    \ 'review': [
      \ #{ block: ['@<b>{', '}'], motionwise: ['char'], keys: ['B'] },
      \ #{ block: ['@<i>{', '}'], motionwise: ['char'], keys: ['i'] },
      \ #{ block: ['@<u>{', '}'], motionwise: ['char'], keys: ['u'] },
      \ #{ block: ['@<tt>{', '}'], motionwise: ['char'], keys: ['t'] },
      \ #{ block: ['@<idx>{', '}'], motionwise: ['char'], keys: ['x'] },
      \ #{ block: ['@<ruby>{', ', ruby}'], motionwise: ['char'], keys: ['r'] },
      \ #{ block: ['@<code>{', '}'], motionwise: ['char'], keys: ['c'] },
      \ #{ block: ['@<mathcode>{', '}'], motionwise: ['char'], keys: ['m'] },
      \ #{ block: ['@<img>{', '}'], motionwise: ['char'], keys: ['[i'] },
      \ #{ block: ['@<list>{', '}'], motionwise: ['char'], keys: ['[l'] },
    \ ],
    \ 'html': basic_html_tags,
    \ 'vue': basic_html_tags,
    \ 'typescript.tsx': basic_html_tags,
  \ }

  " NOTE: Can operator-surround allow <localleader> by some way?
endfunction

function vimrc#dein#hook_source#gina() abort
  " TODO: Move to ftplugins
  call gina#custom#mapping#nmap('status' , 'A'     , ':<C-u>call vimrc#dein#hook_source#gina_git_add_patch_this()<CR>'                  , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'o'     , ':<C-u>call gina#action#call("edit")<CR>'                                          , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'Q'     , ':<C-u>bdelete!<CR>'                                                               , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'sa'    , ':<C-u>call vimrc#dein#hook_source#gina_git_stash_patch_this()<CR>'                , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'sp'    , ':<C-u>Gina stash pop<CR>'                                                         , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'cc'    , ':<C-u>call vimrc#dein#hook_source#gina_commit_very_verbose("")<CR>'               , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'ca'    , ':<C-u>call vimrc#dein#hook_source#gina_commit_very_verbose("--amend")<CR>'        , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'cf'    , ':<C-u>GCommitFixup<Space>'                                                        , { 'noremap': 1 , 'silent': 0})
  call gina#custom#mapping#nmap('status' , 'co'    , ':<C-u>call vimrc#dein#hook_source#gina_git_restore_someones_this("--ours")<CR>'   , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'ct'    , ':<C-u>call vimrc#dein#hook_source#gina_git_restore_someones_this("--theirs")<CR>' , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , '<C-j>' , ':<C-u>call gina#action#call("diff:preview:bottom")<CR>'                           , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , '<C-r>' , ':<C-u>Gina status<CR>'                                                            , { 'noremap': 1 , 'silent': 1})

  call gina#custom#mapping#nmap('commit' , 'ZZ' , ':<C-u>call vimrc#dein#hook_source#gina_commit_close()<CR>' , {'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('commit' , '<C-j>' , 'o<Esc>' , {'noremap': 1})

  call gina#custom#mapping#nmap('branch' , 'Q' , ':<C-u>bdelete!<CR>' , {'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('log'    , 'Q' , ':<C-u>bdelete!<CR>' , {'noremap': 1 , 'silent': 1})
endfunction

function vimrc#dein#hook_source#gina_git_add_patch_this() abort
  " TODO: This may fail if the file name contains spaces
  let file = split(getline('.'), ' ')[1]
  let git_add = 'git add --patch ' .. file
  CdGitRoot
  call vimrc#open_terminal_as('', 'tabnew', git_add)
endfunction

" TODO: Don't open the window. Refresh instead.
" someones: '--ours' or '--theirs'
function vimrc#dein#hook_source#gina_git_restore_someones_this(someones) abort
  " TODO: This may fail if the file name contains spaces
  let file = split(getline('.'), ' ')[1]
  let git_restore = 'git restore ' .. a:someones .. ' ' .. file
  CdGitRoot
  call vimrc#open_terminal_as('', 'tabnew', git_restore)
endfunction

function vimrc#dein#hook_source#gina_git_stash_patch_this() abort
  "TODO: This may fail if the file name contains spaces
  let name = input('stash name: ')
  if empty(name)
    return
  endif
  let file = split(getline('.'))[2]
  let git_stash = 'git stash push -m ' .. shellescape(name) .. ' --patch ' .. file
  CdGitRoot
  call vimrc#open_terminal_as('', 'tabnew', git_stash, {'noclose': v:true})
endfunction

function vimrc#dein#hook_source#gina_commit_very_verbose(subcmd) abort
  call vimrc#bufclose_filetype(['diff'])
  if len(tabpagebuflist()) isnot 1
    quit
    tabnew
  endif

  Gina diff -u --cached --no-color --no-ext-diff --group=diff
  Gina log --oneline --opener='bot split'
  resize 10
  execute 'normal!' "\<C-w>H"

  try
    execute 'Gina' 'commit' '--verbose' '--group=commit' '--opener=split' a:subcmd
    let b:gina_commit_very_verbose = v:true
  catch
    echomsg 'Opening terminal instead of `:Gina commit` because:'
    echomsg v:exception
    call s:open_term_to_commit(a:subcmd)
  endtry
endfunction

function vimrc#dein#hook_source#gina_commit_close() abort
  let gina_commit_very_verbose = get(b:, 'gina_commit_very_verbose', v:false)
  wq

  if !gina_commit_very_verbose
    quit
    return
  endif

  if vimrc#has_two_or_more_tabpages()
    tabclose
    return
  endif

  only
endfunction

function vimrc#dein#hook_source#emmet() abort
  let g:user_emmet_install_global = 0
  let g:user_emmet_leader_key = '<C-g>'

  augroup vimrc
    autocmd! FileType html,xml,markdown EmmetInstall
  augroup END
endfunction

function s:open_term_to_commit(subcmd) abort
  call vimrc#open_terminal_as('term-shell-git-commit', 'stay', &shell, #{ path: g:vimrc.git_root })

  const current_bufnr = bufnr('%')
  const sleeping_time_to_wait_spawning_terminal = 1500
  call timer_start(sleeping_time_to_wait_spawning_terminal, { _ ->
    \ term_sendkeys(current_bufnr, 'git commit ' .. a:subcmd .. "\<CR>i:sparkles:\<Space>")
  \ }, #{ repeat: 1 })
endfunction
