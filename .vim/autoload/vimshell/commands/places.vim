let g:vimshell_hereis_file = get(g:, 'vimshell_hereis_file', expand('~/.vimsh_places.vim'))

let s:command = {
\	'name' : 'places',
\	'kind' : 'internal',
\	'description' : 'view define paths by hereis'
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
		let l:alias      = split(l:place, '=')
		let l:alias_name = split(l:alias[0], ' ')[1]

		let l:path       = split(l:alias[1], ' ')[1]
		let l:path1      = substitute(l:path, "'", '', '')

		echo l:alias_name . ":\t" . l:path1
	endfor
endfunction
