" Yank posted gist to clipboard
function! vimrc#autocmd#yank_gista_posted_url() abort
  let gistid = g:gista#avars.gistid
  execute printf('Gista browse --yank --gistid=%s', gistid)
  let @+ = @"
endfunction

" Auto set cursor position in the file
function! vimrc#autocmd#visit_past_position()
  let past_posit = line("'\"")
  if past_posit > 0 && past_posit <= line('$')
    execute 'normal! g`"'
  endif
endfunction

function! vimrc#autocmd#lcd_buffer_dir_or_base() abort
  try
    lcd %:p:h
  catch /\(E344\|E472\)/ " the buffer has no file
    execute ':lcd' g:vimrc.path_at_started
  endtry
endfunction
