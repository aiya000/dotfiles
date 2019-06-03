let b:undo_ftplugin = 'setl ' . join([
  \ 'nolist<',
  \ 'nonumber<',
  \ 'norelativenumber<',
\ ])

setl nolist nonumber norelativenumber

nnoremap <silent><buffer><expr> i denite#do_map('open_filter_buffer')
nnoremap <silent><buffer><expr> a denite#do_map('open_filter_buffer')
nnoremap <buffer><silent><expr> Q denite#do_map('quit')
nnoremap <buffer><silent><expr> <CR> denite#do_map('do_action')
nnoremap <buffer><silent><expr> <C-[> denite#do_map('quit')
nnoremap <buffer><silent><expr> <C-l> denite#do_map('quit')
nnoremap <buffer><silent><expr> <C-j> denite#do_map('do_action')
nnoremap <buffer><silent><expr> <C-i> denite#do_map('toggle_select_down')

call denite#custom#source('buffer', 'matchers', ['matcher_substring'])
call denite#custom#source('file', 'matchers', ['matcher_substring'])
call denite#custom#source('file_rec', 'matchers', ['matcher_substring'])
call denite#custom#source('line', 'matchers', ['matcher_substring'])
call denite#custom#source('tag', 'matchers', ['matcher_substring'])
call denite#custom#source('file_mru', 'matchers', ['matcher_substring', 'matcher_ignore_globs'])
call denite#custom#source('tag', 'converters', ['converter/abbr_word'])
