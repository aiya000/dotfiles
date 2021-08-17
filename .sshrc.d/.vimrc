set
  \ autoindent
  \ backspace=indent,eol,start
  \ breakindent
  \ browsedir=buffer
  \ cindent
  \ cmdheight=2
  \ completeopt-=preview
  \ cursorline
  \ fileencodings=ucs-bom,utf-8,sjis,euc-jp,cp932,iso-2022-jp-3,iso-2022-jp,eucjp-ms,euc-jisx0213,ucs-bom,latin1,default
  \ hidden
  \ history=500
  \ hlsearch
  \ incsearch
  \ laststatus=2
  \ linebreak
  \ list
  \ listchars=tab:»_,trail:_,extends:»,precedes:«,nbsp:%,eol:↲
  \ matchpairs+=<:>,（:）,｛:｝,「:」,＜:＞,『:』
  \ nojoinspaces
  \ noruler
  \ notimeout
  \ nowrap
  \ nowrapscan
  \ number
  \ path=.,,./*
  \ previewheight=40
  \ relativenumber
  \ scrolloff=16
  \ sessionoptions=buffers,curdir
  \ shellslash
  \ suffixes=
  \ expandtab
  \ shiftwidth=2
  \ tabstop=2
  \ textwidth=0
  \ visualbell
  \ wildignorecase
  \ wildmenu

nnoremap <C-k><C-j> :<C-u>w<CR>

inoremap <C-l> <Esc>
inoremap <C-k><C-j> <Esc>:w<CR>
