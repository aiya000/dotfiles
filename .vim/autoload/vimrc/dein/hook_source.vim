scriptencoding utf-8

let s:V = vital#vimrc#new()
let s:List = s:V.import('Data.List')

function! vimrc#dein#hook_source#operator_surround() abort
    " Exclude brackets () [] {} and ` for unique mappings ('keys')
    let basic_symbols = s:List.char_range('!', "'") + ['*', '&', '_', '|', '~']
    let basic_between = map(basic_symbols, { _, char ->
      \ { 'block' : [char, char], 'motionwise' : ['char', 'line', 'block'], 'keys' : [char] }
    \ })

    let g:operator#surround#blocks = {
      \ '-' : [
        \ { 'block' : ['(', ')'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['(', ')', 'p'] },
        \ { 'block' : ['[', ']'], 'motionwise' : ['char', 'line', 'block'], 'keys' : [']', 'k'] },
        \ { 'block' : ['{', '}'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['{', '}', 'P'] },
        \ { 'block' : ['<', '>'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['<', '>', 'K'] },
        \ { 'block' : [' ', ' '], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['  '] },
        \ { 'block' : ['`', '`'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['`', 'b'] },
        \ { 'block' : ['（', '）'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['（', ' ）', 'j(', 'j)', 'jp'] },
        \ { 'block' : ['｛', '｝'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['｛', ' ｝', 'j{', 'j}', 'jP'] },
        \ { 'block' : ['「', '」'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['「', ' 」', 'j[', 'j]', 'jk'] },
        \ { 'block' : ['〈', '〉'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['〈', ' 〉', 'jK'] },
        \ { 'block' : ['『', '』'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['『', ' 』', 'j-k'] },
        \ { 'block' : ['＜', '＞'], 'motionwise' : ['char', 'line', 'block'], 'keys' : ['『', ' 』', 'j<', 'j>'] },
      \ ] + basic_between,
      \ 'review' : [
        \ { 'block' : ['@<b>{', '}'], 'motionwise' : ['char'], 'keys' : ['[b'] },
        \ { 'block' : ['@<i>{', '}'], 'motionwise' : ['char'], 'keys' : ['[i'] },
        \ { 'block' : ['@<u>{', '}'], 'motionwise' : ['char'], 'keys' : ['[u'] },
        \ { 'block' : ['@<tt>{', '}'], 'motionwise' : ['char'], 'keys' : ['[tt'] },
        \ { 'block' : ['@<idx>{', '}'], 'motionwise' : ['char'], 'keys' : ['[x'] },
        \ { 'block' : ['@<ruby>{', ', ""}'], 'motionwise' : ['char'], 'keys' : ['[r'] },
        \ { 'block' : ['@<code>{', '}'], 'motionwise' : ['char'], 'keys' : ['[c'] },
        \ { 'block' : ['@<mathcode>{', '}'], 'motionwise' : ['char'], 'keys' : ['[m'] },
      \ ],
      \ 'markdown' : [
        \ { 'block' : ['**', '**'], 'motionwise' : ['char'], 'keys' : ['B'] },
      \ ],
    \ }
    " NOTE: Does operator-surround allow <localleader> by some way?
endfunction

function! vimrc#dein#hook_source#gina() abort
  " TODO: Move to ftplugins
  call gina#custom#mapping#nmap('status' , 'A'     , ':<C-u>call vimrc#dein#hook_source#gina_git_add_patch_this()<CR>'      , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'o'     , ':<C-u>call gina#action#call("edit")<CR>'                              , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'Q'     , ':<C-u>bdelete!<CR>'                                                   , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'sa'    , ':<C-u>call vimrc#dein#hook_source#gina_git_stash_patch_this()<CR>'    , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'sp'    , ':<C-u>Gina stash pop<CR>'                                             , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'cc'    , ':<C-u>call vimrc#dein#hook_source#gina_commit_very_verbose("")<CR>'        , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'ca'    , ':<C-u>call vimrc#dein#hook_source#gina_commit_very_verbose("--amend")<CR>' , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , 'cf'    , ':<C-u>GCommitFixup<Space>'                                            , { 'noremap': 1 , 'silent': 0})
  call gina#custom#mapping#nmap('status' , '<C-j>' , ':<C-u>call gina#action#call("diff:preview:bottom")<CR>'               , { 'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('status' , '<C-r>' , ':<C-u>Gina status<CR>'                                                , { 'noremap': 1 , 'silent': 1})

  call gina#custom#mapping#nmap('commit' , 'ZZ' , ':<C-u>call vimrc#dein#hook_source#gina_commit_close()<CR>' , {'noremap': 1 , 'silent': 1})  " Avoid to failure commit
  call gina#custom#mapping#nmap('commit' , '<C-j>' , 'o<Esc>' , {'noremap': 1})

  call gina#custom#mapping#nmap('branch' , 'Q' , ':<C-u>bdelete!<CR>' , {'noremap': 1 , 'silent': 1})
  call gina#custom#mapping#nmap('log'    , 'Q' , ':<C-u>bdelete!<CR>' , {'noremap': 1 , 'silent': 1})
endfunction

function! vimrc#dein#hook_source#gina_git_add_patch_this() abort
  "TODO: This may fail if the file name contains spaces
  let file = split(getline('.'), ' ')[1]
  let git_add = 'git add --patch ' . file
  CdGitRoot
  call vimrc#open_terminal_as('', 'tabnew', git_add)
endfunction

function! vimrc#dein#hook_source#gina_git_stash_patch_this() abort
  "TODO: This may fail if the file name contains spaces
  let name = input('stash name: ')
  if empty(name)
    return
  endif
  let file = split(getline('.'))[2]
  let git_stash = 'git stash push -m ' . shellescape(name) . ' --patch ' . file
  CdGitRoot
  call vimrc#open_terminal_as('', 'tabnew', git_stash, {'noclose': v:true})
endfunction

function! vimrc#dein#hook_source#gina_commit_very_verbose(subcmd) abort
  if len(tabpagebuflist()) isnot 1
    quit
    tabnew
  endif

  Gina diff -u --cached --no-color --no-ext-diff --group=gina-commit
  vsplit
  Gina status --group=gina-commit
  split
  execute 'Gina' 'commit' '--verbose' ' --group=gina-commit' a:subcmd
  let b:gina_commit_very_verbose = v:true
endfunction

function! vimrc#dein#hook_source#gina_commit_close() abort
  let gina_commit_very_verbose = get(b:, 'gina_commit_very_verbose', v:false)

  wq
  if gina_commit_very_verbose && tabpagenr('$') > 1
    tabclose
  elseif gina_commit_very_verbose
    only
  else
    quit
  endif
endfunction

function! vimrc#dein#hook_source#emmet() abort
  let g:user_emmet_install_global = 0
  let g:user_emmet_leader_key = '<C-g>'

  augroup vimrc
    autocmd! FileType html,xml,markdown EmmetInstall
  augroup END
endfunction
