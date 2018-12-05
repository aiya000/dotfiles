GuiFont RictyDiminished NF:h11

nnoremap <C-+> :<C-u>call <SID>increase_font_size()<CR>
nnoremap <C--> :<C-u>call <SID>decrease_font_size()<CR>

let s:gui_font_size = 14

function! s:increase_font_size() abort
    let s:gui_font_size += 1
    execute 'GuiFont RictyDiminished NF:h' . s:gui_font_size
endfunction

function! s:decrease_font_size() abort
    let s:gui_font_size -= 1
    execute 'GuiFont RictyDiminished NF:h' . s:gui_font_size
endfunction
