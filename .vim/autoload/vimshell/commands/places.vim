let g:vimshell_hereis_file = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vim'))

let s:command = {
\	'name' : 'places',
\	'kind' : 'internal',
\	'description' : 'view define hereis paths - g:vimshell_hereis_file'
\}

function! vimshell#commands#places#define()
	return s:command
endfunction


" --- --- --- "


function! s:command.execute(args, context)
	let l:file = g:vimshell_hereis_file
	if !filereadable(l:file)
		echoerr 'no such places file'
		return
	endif

	for l:place in readfile(l:file)
		echo s:format_alias(l:place)
	endfor
endfunction

function! s:format_alias(place)
	" Convert sequence
	"     alias hoge='cd "/hoge/foo/space space"'
	"     [alias hoge], ['cd "/hoge/foo/space space"']
	"     hoge, ['cd "/hoge/foo/space space"']
	"     hoge, "/hoge/foo/space space"

	let l:alias      = split(a:place, '=')
	let l:alias_name = split(l:alias[0], ' ')[1]

	let l:path       = join(split(l:alias[1], ' ')[1:], ' ')
	let l:path1      = substitute(l:path, "'", '', '')

	return l:alias_name . ":\t" . l:path1
endfunction
