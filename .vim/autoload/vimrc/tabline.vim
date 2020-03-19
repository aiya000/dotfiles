let s:List = vital#vimrc#import('Data.List')

function s:shorten_path_if_needed(path) abort
  let mettya_nagai = 60 | lockvar! mettya_nagai
  return len(a:path) > mettya_nagai
    \ ? pathshorten(a:path)
    \ : a:path
endfunction

" Maybe this is specified to 'tabline' with a pattern like `set tabline=%!this_function()`.
"
" See ':h hl-User1..9' for what is the pattern of '%n*string%*' (n is a naturalnumer),
" and below augroup 'HighlightPref'.
function vimrc#tabline#make() abort
  let running_lsp_servers = execute(':LspStatus')
    \ ->split('\n')
    \ ->filter({ _, x ->
      \ x !~# 'not running$'
    \ })
    \ ->map({ _, x -> x->split(':')[0] })
    \ ->join(', ')

  return '%1*[%{tabpagenr("$")}]%* '
    \. s:tabs() . ' => '
    \. '%2*[PWD=%{vimrc#tabline#cwd_or_shorten()}]%*'
    \. '%3*%{vimrc#tabline#tags_if_present()}%*'
    \. '%4*%{vimrc#tabline#marks_if_present()}%*'
    \. '%5*%{vimrc#tabline#ale_if_present()}%*'
    \. $'%6*[${running_lsp_servers}]%*'
endfunction

function vimrc#tabline#tags_if_present() abort
  let tags = tagfiles()
  return
    \ empty(tags)
      \ ? '' :
    \ len(tags) is 1
      \ ? ('[Tag=' . s:shorten_path_if_needed(tags[0]) . ']') :
    \ ('[Tag=' . s:shorten_path_if_needed(tags[0]) . ' +]')
endfunction

function vimrc#tabline#marks_if_present() abort
  let marks = s:get_buf_marks()
  return empty(marks)
    \ ? ''
    \ : '[Mark=' . join(marks, '') . ']'
endfunction

function s:get_buf_marks() abort
  let lines       = split(execute(':marks'), "\n")[2:]  " List only the lines like ` a 30 48 let lines = split...`
  let all_marks   = map(lines, {_, x -> x[1]})
  let local_marks = filter(all_marks, {_, x -> match(x, '\l', 0, 0) is 0})
  return local_marks
endfunction

" NOTE: http://d.hatena.ne.jp/thinca/20111204/1322932585
function s:tabs()
  let titles = map(range(1, tabpagenr('$')), { _, tabnr ->
    \ vimrc#tabline#tabpage_label(tabnr)
  \ })
  return join(titles) . '%#TabLineFill#%T'
endfunction

function vimrc#tabline#tabpage_label(tabnr)
  let title = gettabvar(a:tabnr, 'title')
  if title !=# ''
    return title
  endif
  let focused_winnr = tabpagewinnr(a:tabnr)
  let curbufnr = tabpagebuflist(a:tabnr)[focused_winnr - 1]
  let file_name = fnamemodify(bufname(curbufnr), ':t')
  let file_name =
    \ (file_name == '')
      \ ? '[NoName]' :
    \ (len(file_name) > 20)
      \ ? (file_name[0:7] . '...' . file_name[-10:-1]) :
    \ file_name

  " Please see `:h TabLineSel` and `:h TabLine`
  let window_num = '[' . len(tabpagebuflist(a:tabnr)) . ']'
  let label_of_a_buf = s:is_stayed_tab(a:tabnr)
    \ ? ('%#TabLineSel#[* ' . s:get_mod_mark_for_window(focused_winnr) . window_num . file_name . ' *]')
    \ : ('%#TabLine#[' . s:get_mod_mark_for_tab(a:tabnr) . window_num . file_name . ']')

  return '%' . a:tabnr . 'T' . label_of_a_buf . '%T%#TabLineFill#'
endfunction

" Do you staying the specified tab?
function s:is_stayed_tab(tabnr) abort
  return a:tabnr is tabpagenr()
endfunction

" Return '+' if the buffer of the specified window is modified
function s:get_mod_mark_for_window(winnr) abort
  return getbufvar(winbufnr(a:winnr), '&modified') ? '+' : ''
endfunction

" Return '+' if one or more a modified non terminal buffer is existent in the taken tab
function s:get_mod_mark_for_tab(tabnr) abort
  let term_buffers = term_list()
  let modified_buffer = s:List.find(tabpagebuflist(a:tabnr), v:null, { bufnr_at_tab ->
    \ !s:List.has(term_buffers, bufnr_at_tab) &&
    \ getbufvar(bufnr_at_tab, '&modified')
  \ })
  return (modified_buffer is v:null) ? '' : '+'
endfunction

function vimrc#tabline#cwd_or_shorten() abort
  return s:shorten_path_if_needed(getcwd())
endfunction

function vimrc#tabline#ale_if_present() abort
  let g_label = get(g:, 'ale_enabled', 1) ? '' : 'g'
  let b_label = get(b:, 'ale_enabled', 1) ? '' : 'b'
  return (g_label !=# '') || (b_label !=# '')
    \ ? ('[ale_disabled:' . g_label . b_label . ']')
    \ : '[ale]'
endfunction
