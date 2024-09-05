scriptencoding utf-8

let s:V = vital#vimrc#new()

let s:List = s:V.import('Data.List')
let s:Msg = s:V.import('Vim.Message')

function vimrc#dein#hook_source#operator_surround() abort
  const basic_symbols = s:List.char_range('!', "'") + ['*', '&', '_', '|', '~', ':', '/'] " Excluding brackets () [] {} and ` for unique mappings ('keys')
  const basic_between = s:List.map(basic_symbols, { char ->
    \ #{ block: [char, char], motionwise: ['char', 'line', 'block'], keys: [char] }
  \ })

  const basic_html_tags = [
    \ #{ block: ['<p>', '</p>'], motionwise: ['char'], keys: ['[p'] },
    \ #{ block: ['<a>', '</a>'], motionwise: ['char'], keys: ['[a'] },
    \ #{ block: ['<div>', '</div>'], motionwise: ['char'], keys: ['[d'] },
    \ #{ block: ['<span>', '</span>'], motionwise: ['char'], keys: ['[s'] },
    \ #{ block: ['<h1>', '</h1>'], motionwise: ['char'], keys: ['[h1'] },
    \ #{ block: ['<h2>', '</h2>'], motionwise: ['char'], keys: ['[h2'] },
    \ #{ block: ['<h3>', '</h3>'], motionwise: ['char'], keys: ['[h3'] },
    \ #{ block: ['<h4>', '</h4>'], motionwise: ['char'], keys: ['[h4'] },
    \ #{ block: ['<h5>', '</h5>'], motionwise: ['char'], keys: ['[h5'] },
    \ #{ block: ['<ol>', '</ol>'], motionwise: ['char'], keys: ['[ol'] },
    \ #{ block: ['<ul>', '</ul>'], motionwise: ['char'], keys: ['[ul'] },
    \ #{ block: ['<li>', '</li>'], motionwise: ['char'], keys: ['[li'] },
  \ ]

  let g:operator#surround#blocks = {
    \ '-': [
      \ #{ block: ['(', ')'], motionwise: ['char', 'line', 'block'], keys: ['(', ')', 'p'] },
      \ #{ block: ['[', ']'], motionwise: ['char', 'line', 'block'], keys: [']', 'k'] },
      \ #{ block: ['{', '}'], motionwise: ['char', 'line', 'block'], keys: ['{', '}', 'P'] },
      \ #{ block: ['<', '>'], motionwise: ['char', 'line', 'block'], keys: ['<', '>', 'K'] },
      \ #{ block: [' ', ' '], motionwise: ['char', 'line', 'block'], keys: ['  '] },
      \ #{ block: ['`', '`'], motionwise: ['char', 'line', 'block'], keys: ['`', 'b'] },
      \ #{ block: ['（', '）'], motionwise: ['char', 'line', 'block'], keys: ['（', ' ）', 'j(', 'j)', 'jp'] },
      \ #{ block: ['｛', '｝'], motionwise: ['char', 'line', 'block'], keys: ['｛', ' ｝', 'j{', 'j}', 'jP'] },
      \ #{ block: ['「', '」'], motionwise: ['char', 'line', 'block'], keys: ['「', ' 」', 'j[', 'j]', 'jk'] },
      \ #{ block: ['〈', '〉'], motionwise: ['char', 'line', 'block'], keys: ['〈', ' 〉', 'jK'] },
      \ #{ block: ['『', '』'], motionwise: ['char', 'line', 'block'], keys: ['『', ' 』', 'j-k'] },
      \ #{ block: ['＜', '＞'], motionwise: ['char', 'line', 'block'], keys: ['『', ' 』', 'j<', 'j>'] },
      \ #{ block: ['**', '**'], motionwise: ['char'], keys: ['B'] },
      \ #{ block: ['~~', '~~'], motionwise: ['char'], keys: ['~'] },
    \ ] + basic_between,
    \ 'review': [
      \ #{ block: ['@<b>{', '}'], motionwise: ['char'], keys: ['B'] },
      \ #{ block: ['@<i>{', '}'], motionwise: ['char'], keys: ['i'] },
      \ #{ block: ['@<u>{', '}'], motionwise: ['char'], keys: ['u'] },
      \ #{ block: ['@<tt>{', '}'], motionwise: ['char'], keys: ['t'] },
      \ #{ block: ['@<idx>{', '}'], motionwise: ['char'], keys: ['x'] },
      \ #{ block: ['@<ruby>{', ', ruby}'], motionwise: ['char'], keys: ['r'] },
      \ #{ block: ['@<code>{', '}'], motionwise: ['char'], keys: ['c'] },
      \ #{ block: ['@<mathcode>{', '}'], motionwise: ['char'], keys: ['m'] },
      \ #{ block: ['@<img>{', '}'], motionwise: ['char'], keys: ['[i'] },
      \ #{ block: ['@<list>{', '}'], motionwise: ['char'], keys: ['[l'] },
    \ ],
    \ 'html': basic_html_tags,
    \ 'vue': basic_html_tags,
    \ 'typescript.tsx': basic_html_tags,
  \ }

  " NOTE: Can operator-surround allow <localleader> by some way?
endfunction

function vimrc#dein#hook_source#emmet() abort
  let g:user_emmet_install_global = 0
  let g:user_emmet_leader_key = '<C-g>'

  augroup vimrc
    autocmd! FileType html,xml,markdown EmmetInstall
  augroup END
endfunction
