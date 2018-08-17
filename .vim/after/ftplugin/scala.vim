let b:undo_ftplugin = 'setl ' . join([
    \ 'tabstop<',
    \ 'shiftwidth<',
    \ 'expandtab<',
    \ 'commentstring<',
\ ])

let &commentstring = ' /*%s*/'
setl tabstop=2 shiftwidth=2 expandtab
let &errorformat = '[%t%.%#] %f:%l:%m'
"let &errorformat='[%t%.%#] %f:%l:%c: %m'
"[error] /Users/yoichiroishikawa/Repository/ecms2-contents-checker/src/main/scala/jp/eflow/ecms2/contentschecker/Main.scala:21: not found: value json
"[error] /Users/yoichiroishikawa/Repository/ecms2-contents-checker/src/main/scala/jp/eflow/ecms2/contentschecker/validator/Validator.scala:51: type mismatch;

nnoremap <buffer><silent> <localleader><localleader>w :<C-u>QuickfixRunSbtCompileWatch<CR>
nnoremap <buffer><silent> <localleader><localleader>W :<C-u>QuickfixStopSbtCompileWatch<CR>

augroup FtpluginScala
    autocmd!
    " Clear past results for :QuickfixRunSbtCompileWatch
    autocmd BufWritePost *.scala CClear
augroup END

function! TestErrFmt(errfmt,lines)
  let temp_errorfomat = &errorformat
  try
    let &errorformat = a:errfmt
    cexpr join(a:lines,"\n")
    copen
  catch
    echo v:exception
    echo v:throwpoint
  finally
    let &errorformat = temp_errorfomat
  endtry
endfunction
