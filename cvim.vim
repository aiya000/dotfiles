"-------------------------------------
" This file is the preference for cVim
"-------------------------------------

let mapleader = '['

"let homedirectory = '/home/aiya000'
"let configpath = '~/.dotfiles/cVimrc'
"set localconfig

let vimport = 55355
let barposition = 'bottom'
let hintcharacters = 'yjuiopwertklhgfdsabnmvcxz'
set scalehints


map a addQuickMark
map b :buffer<Space>
map B :bookmarks<Space>
map d closeTab
"map p  :open <C-v>
map u :restore<Space>
map gH :tabnew google<CR>
map gh :open google<CR>
map gf :viewsource!<CR>
map <C-a> incrementURLPath
map <C-x> decrementURLPath
map <C-n> nextTab
map <C-p> previousTab
map <C-f> scrollPageDown
map <C-b> scrollPageUp
map <C-j> <CR>
map <C-c> <Esc>
map <C-l> <Esc>
map <leader>l <C-l>
map <C-k><C-l> :nohlsearch<CR>

"cnoremap <C-p> <S-Up>
"cnoremap <C-n> <S-Down>
"cnoremap <C-b> <Left>
"cnoremap <C-f> <Right>
"cnoremap <C-h> <BS>
"cnoremap <C-d> <Del>
"cnoremap <C-i> <Tab>
"cnoremap <C-l> <Esc>

imap <C-a> beginningOfLine
imap <C-e> endOfLine
imap <C-p> backwardLine
imap <C-n> forwardLine
imap <C-b> backwardChar
imap <C-f> forwardChar
imap <C-h> deleteChar
imap <C-d> deleteForwardChar
imap <C-u> deleteToBeginning
imap <C-k> deleteToEnd
imap <C-w> deleteWord
"imap <C-,> <S-Left>
"imap <C-.> <S-Right>
"imap <C-[> <S-Up>
"imap <C-]> <S-Down>
"imap <C-j> <CR>
imap <C-c> <Esc>
imap <C-l> <Esc>
"imap <C-x> <NOP>
"imap <C-c> <NOP>
imap <C-x><C-a> selectAll
"imap <C-x><C-t> <C-x>
"imap <C-x><C-c> <C-c>
"imap <C-i> <Tab>
imap <C-g> editWithVim


command Reload source ~/.cvimrc
