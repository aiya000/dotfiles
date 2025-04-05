scriptencoding utf-8
scriptversion 3

let s:List = vital#vimrc#import('Data.List')

" Virtual keymaps
nnoremap <silent> <Plug>(vimrc-surround-append-choice) :<C-u>call vimrc#append_choose_surround()<CR>
nnoremap <silent> <Plug>(vimrc-surround-append-choice-wide) :<C-u>call vimrc#append_choose_surround_wide()<CR>
nnoremap <silent> <Plug>(vimrc-surround-delete-mostly-inner) :<C-u>call vimrc#delete_mostly_inner_surround()<CR>
nnoremap <silent> <Plug>(vimrc-surround-replace-mostly-inner) :<C-u>call vimrc#replace_mostly_inner_surround()<CR>

" Vim systems
"" Scripts
command! -bar -nargs=? -complete=filetype FtpluginEditAfter
    \ execute ':edit' printf('%s/after/ftplugin/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
command! -bar -nargs=? -complete=filetype FtDictionaryEdit
    \ execute ':edit' printf('%s/dict/filetype/%s.dict', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
command! -bar -nargs=? -complete=filetype SyntaxEdit
    \ execute ':edit' printf('%s/syntax/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
command! -bar -nargs=? -complete=filetype IndentEdit
    \ execute ':edit' printf('%s/indent/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
command! -bar -nargs=? -complete=filetype FtDetectEdit
    \ execute ':edit' printf('%s/ftdetect/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
command! -bar -nargs=? -complete=filetype PluginEdit
    \ execute ':edit' printf('%s/plugin/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
command! -bar -nargs=? -complete=filetype AutoloadEdit
    \ execute ':edit' printf('%s/autoload/%s.vim', g:vimrc['vim_home'], (empty(<q-args>) ? &filetype : <q-args>))
"" Clear quickfix
command! -bar CClear call setqflist([])


" Rename a file of the current buffer
command! -bar -nargs=1 -complete=file Rename call vimrc#rename_to(<q-args>)

" Haskell
command! -bar HaskDogs call vimrc#execute_haskdogs_async()
command! -bar EtaDogs call vimrc#execute_haskdogs_in_eta_async()

" Kotlin
command! -bar KtlintAutoFix call system('ktlint --format ' . fnameescape(expand('%'))) | edit %
command! -bar -nargs=* QuickfixRunGradle call vimrc#run_gradle_quickfix(<q-args>)

" Scala
command! -bar -nargs=* QuickfixRunSbtCompileWatch call vimrc#run_scala_compile_watch_quickfix(<q-args>)
command! -bar QuickfixStopSbtCompileWatch call vimrc#stop_scala_compile_watch_quickfix()

" Make
command! -bar -nargs=* QuickfixRunMake call vimrc#run_make_quickfix(<q-args>)

" Git commands (cushion)
command! -bar -nargs=* GStatus GinStatus <args>
command! -bar -nargs=* GLog GitLogViewer -100 --name-only <args>
command! -bar -nargs=* GLogPatch GitLogViewer --patch -100 <args>
command! -bar -nargs=* GLogOneline GitLogViewer --oneline <args>
command! -bar -nargs=* GDiff GitDiffViewer <args>
command! -bar -nargs=* GDS GitDiffViewer --staged <args>
command! -bar -nargs=* GDH GitDiffViewer HEAD~ <args>
command! -bar -nargs=* GCommit Gin commit --verbose <args>
command! -bar -nargs=* GCommitAmmend Gin commit --verbose --amend <args>
command! -bar -nargs=1 GCommitFixup echomsg system('git commit --fixup ' .. <q-args>)
command! -bar -nargs=* GAddPatch terminal git add -p <args>
command! -bar -nargs=* GTree GinLog --graph --decorate --oneline <args>
command! -bar -nargs=* GTreeAll GinLog --graph --decorate --oneline --all <args>
command! -bar -nargs=* GBrahcnAll GinBranch --all <args>
command! -bar -nargs=* GBlame Gin blame <args>

" vim-webpage
command! -bar -nargs=+ Weblio WebpageShow weblio <args>
command! -nargs=+ Stackage WebpageShow stackage <args>

" :cd
command! -bar CdBufDir execute ':cd' fnameescape(expand('%:p:h'))
command! -bar CdStarted execute ':cd' g:vimrc.path_at_started
command! -bar CdGitRoot call vimrc#cd_git_root(':cd')
command! -bar LcdBufDir execute ':lcd' fnameescape(expand('%:p:h'))
command! -bar LcdStarted execute ':lcd' g:vimrc.path_at_started
command! -bar LcdGitRoot call vimrc#cd_git_root(':lcd')
command! -bar TcdBufDir execute ':tcd' fnameescape(expand('%:p:h'))
command! -bar TcdStarted execute ':tcd' g:vimrc.path_at_started
command! -bar TcdGitRoot call vimrc#cd_git_root(':tcd')
""
command! -bar ScdBufDir let g:vimrc.path_at_started = expand('%:p:h')
command! -bar ScdCurrentDir let g:vimrc.path_at_started = getcwd()
command! -bar ScdGitRoot let g:vimrc.path_at_started = g:vimrc.git_root

" Others
command! -bar GitReadRoot call vimrc#read_to_set_git_root()
command! -bar ReadGitRoot call vimrc#read_to_set_git_root()
""
command! -bar -nargs=? Grep call vimrc#ddu_start_from_input(#{
  \ sources: [#{
    \ name: 'rg',
    \ options: #{
      \ matchers: [],
      \ volatile: v:true,
    \ },
  \ }],
  \ uiParams: #{
    \ ff: #{
      \ startFilter: v:true,
      \ ignoreEmpty: v:false,
      \ autoResize: v:false,
    \ },
  \ },
\ }, <f-args>)
command! -bar -nargs=+ SetTabTitle let t:vimrc_tabtitle = <q-args>
command! -bar UnsetTabTitle unlet t:vimrc_tabtitle
command! -bar ReverseLines !tac

" deepl.vim
command! -bar -range=% DeeplTranslateToEn call vimrc#deepl_translate(<count>, <line1>, <line2>, 'EN', 'JA', ['yank', 'echo'])
command! -bar -range=% DeeplTranslateToJa call vimrc#deepl_translate(<count>, <line1>, <line2>, 'JA', 'EN', ['yank', 'echo'])
command! -bar -range=% DeeplTranslateToEnOpenBuffer call vimrc#deepl_translate(<count>, <line1>, <line2>, 'EN', 'JA', ['yank', 'buffer'])
command! -bar -range=% DeeplTranslateToJaOpenBuffer call vimrc#deepl_translate(<count>, <line1>, <line2>, 'JA', 'EN', ['yank', 'buffer'])

"
" Tapis
"

function! Tapi_Tabnew(_, args) abort
  const files = a:args[1:]
  const paths = s:List.map(files, { file -> fnameescape(file) })

  for path in paths
    execute 'tabnew' path
  endfor
endfunction

function! Tapi_Verticalnew(_, args) abort
  const files = a:args[1:]
  const paths = s:List.map(files, { file -> fnameescape(file) })

  for path in paths
    execute 'vertical' 'new' path
  endfor
endfunction
