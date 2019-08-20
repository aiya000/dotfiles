" Inspired by ujihisa's vimrc
" And deris's code (http://deris.hatenablog.jp/entry/2013/05/10/003430)
function! gitlogviewer#git_log_viewer(args)
  enew!
  setl buftype=nofile
  let b:gitlogviewer_args = a:args
  call s:read_git_log(a:args)
  setl filetype=gitlogviewer
  setl foldmethod=expr
  setl foldexpr=FoldExprOfGitLog(v:lnum)
  setl foldtext=FoldTextOfGitLog()
endfunction

function! s:read_git_log(args)
  put!=system('git log ' . a:args)
  normal! gg
endfunction

function! FoldExprOfGitLog(lnum)
  return getline(a:lnum)     =~# '^commit' ? '>1'
    \  : getline(a:lnum + 1) =~# '^commit' ? '<1' : '='
endfunction

function! FoldTextOfGitLog()
  let month_map = #{
    \ Jan: '01',
    \ Feb: '02',
    \ Mar: '03',
    \ Apr: '04',
    \ May: '05',
    \ Jun: '06',
    \ Jul: '07',
    \ Aug: '08',
    \ Sep: '09',
    \ Oct: '10',
    \ Nov: '11',
    \ Dec: '12',
  \ }

  if getline(v:foldstart) !~# '^commit'
    return getline(v:foldstart)
  endif

  if getline(v:foldstart + 1) =~# '^Author:'
    let author_lnum = v:foldstart + 1
  elseif getline(v:foldstart + 2) =~# '^Author:'
    " commitの次の行がMerge:の場合があるので
    let author_lnum = v:foldstart + 2
  else
    " commitの下2行がどちらもAuthor:で始まらなければ諦めて終了
    return getline(v:foldstart)
  endif

  if getline(author_lnum - 1) =~# '^commit'
    let commit_lnum = author_lnum - 1
  else
    " commitの次の行がMerge:の場合があるので
    let commit_lnum = author_lnum - 2
  endif
  let date_lnum    = author_lnum + 1
  let message_lnum = date_lnum + 2

  let commit  = matchstr(getline(commit_lnum), '^commit \zs.*\ze')[0:6]  " reflogと同じ文字数で表示
  let author  = matchstr(getline(author_lnum), '^Author: \zs.*\ze <.\{-}>')
  let date    = matchlist(getline(date_lnum), ' \(\a\{3}\) \(\d\{1,2}\) \(\d\{2}:\d\{2}:\d\{2}\) \(\d\{4}\)')
  let message = getline(message_lnum)[3:]

  let month = date[1]
  let day   = printf('%02s', date[2])
  let time  = join(split(date[3], ':')[0:1], ':')  " 秒を非表示
  let year  = date[4]

  let datestr = join([year, month_map[month], day], '-')
  return join([commit, datestr, time, author, message], ' ')
endfunction
