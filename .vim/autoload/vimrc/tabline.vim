let s:List = vital#vimrc#import('Data.List')

function s:shorten_path_if_needed(path) abort
  const mettya_nagai = 60 | lockvar! mettya_nagai
  return len(a:path) > mettya_nagai
    \ ? pathshorten(a:path)
    \ : a:path
endfunction

function s:shorten_bufname(buf_name) abort
  return a:buf_name[0:7] .. '...' .. a:buf_name[-10:-1]
endfunction

" Shows tab-titles and some statuses
"
" See ':h hl-User1..9' for what is the pattern of '%n*string%*' (n is a naturalnumer),
" and below augroup 'HighlightPref'.
function vimrc#tabline#make() abort
  return '%1*[%{tabpagenr("$")}]%* '
    \ .. s:tabs() .. ' => '
    \ .. '%2*[PWD=%{vimrc#tabline#cwd_or_shorten()}]%*'
    \ .. '%3*%{vimrc#tabline#tags_if_present()}%*'
    \ .. '%4*%{vimrc#tabline#marks_if_present()}%*'
    \ .. '%5*%{vimrc#tabline#ale_if_present()}%*'
    \ .. '%6*%{vimrc#tabline#running_lsp_servers()}%*'
endfunction

function vimrc#tabline#running_lsp_servers() abort
  const IsNotePc = { -> &columns < 160 }

  const servers = execute(':LspStatus')
    \ ->split('\n')
    \ ->filter({ _, x ->
      \ split(x, ' ')[1] ==# 'running'
    \ })
    \ ->map({ _, x ->
      \ split(x, ':')[0]
    \ })

  const full = '[' .. join(servers, ', ') .. ']'
  if IsNotePc() || len(full) < 30
    return full
  endif

  const shorten_servers = servers
    \ ->map({_, x -> x[:8] .. '..' })
    \ ->join(', ')
  return '[' .. shorten_servers .. ']'
endfunction

function vimrc#tabline#tags_if_present() abort
  const tags = tagfiles()
  return
    \ empty(tags) ? '' :
    \ len(tags) is 1 ? ('[Tag=' .. s:shorten_path_if_needed(tags[0]) .. ']') :
    \ '[Tag=' .. s:shorten_path_if_needed(tags[0]) .. ' +]'
endfunction

function vimrc#tabline#marks_if_present() abort
  const marks = s:get_buf_mark_chars()
  return empty(marks)
    \ ? ''
    \ : '[Mark=' .. join(marks, '') .. ']'
endfunction

function s:get_buf_mark_chars() abort
  const mark_chars = execute(':marks')
    \ ->split("\n")[2:]
    \ ->map({_, x -> x[1]})

  return
    \ s:List.filter(mark_chars, { x ->
      \ match(x, '\l', 0, 0) is 0
    \ })
endfunction

function s:tabs()
  const labels = map(range(1, tabpagenr('$')), { _, tabnr ->
    \ vimrc#tabline#tabpage_label(tabnr)
  \ })
  return join(labels) .. '%#TabLineFill#%T'
endfunction

" Returns: string
function vimrc#tabline#tabpage_label(tabnr)
  const title = gettabvar(a:tabnr, 'vimrc_tabtitle')
  if title !=# ''
    return s:get_qualified(a:tabnr, printf(" '%s'", title))
  endif
  return s:get_file_label(a:tabnr)
endfunction

function s:get_file_label(tabnr) abort
  const focused_winnr = tabpagewinnr(a:tabnr)
  const curbufnr = tabpagebuflist(a:tabnr)[focused_winnr - 1]
  const file_name = fnamemodify(bufname(curbufnr), ':t')->vimrc#let({ buf_name ->
    \ (buf_name ==# '') ? '[NoName]' :
    \ (len(buf_name) > 20) ? s:shorten_bufname(buf_name) :
    \ buf_name
  \ })

  return s:get_qualified(a:tabnr, file_name)
endfunction

" Returns '+' if the buffer of the specified window is modified.
function s:get_mod_mark_for_window(winnr) abort
  return getbufvar(winbufnr(a:winnr), '&modified') ? '+' : ''
endfunction

" Colorizes by it either a current tab or an another window.
" Please see `:h TabLineSel` and `:h TabLine`
function s:get_qualified(tabnr, title) abort
  const winnr = tabpagewinnr(a:tabnr)
  const window_num = '[' .. len(tabpagebuflist(a:tabnr)) .. ']'

  const another_or_focused_buf_label = s:is_stayed_tab(a:tabnr)
    \ ? '%#TabLineSel#[* ' .. (s:get_mod_mark_for_window(winnr) .. window_num .. a:title) .. ' *]'
    \ : '%#TabLine#[' .. (s:get_mod_mark_for_tab(a:tabnr) .. window_num .. a:title) .. ']'
  const terminated = '%' .. a:tabnr .. 'T' .. another_or_focused_buf_label .. '%T%#TabLineFill#'

  return terminated
endfunction

" Do you staying the specified tab?
function s:is_stayed_tab(tabnr) abort
  return a:tabnr is tabpagenr()
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
