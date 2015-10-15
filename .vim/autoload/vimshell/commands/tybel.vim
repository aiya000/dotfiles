let s:command = {
\	'name'        : 'tybel',
\	'kind'        : 'internal',
\	'description' : 'tybel source.ts - composite tsc and babel'
\}

function! vimshell#commands#tybel#define()
	return s:command
endfunction

" --- --- --- "

function! s:angry(msg)
	call vimshell#interactive#send(printf('echo "!!!>> %s!!!"', a:msg))
endfunction

function s:open_scratch(detail)
	new
	setl buftype=nofile filetype=scratch
	execute 'normal! i' a:detail
endfunction

"@Incomplete("don't escaped from insert mode when execution succeed")
function! s:command.execute(args, context)
	if !executable('tsc')
		call s:angry('tsc command is not found, tybel needs it')
	elseif !executable('babel-node')
		call s:angry('babel-node command is not found, tybel needs it')
	endif
	let l:sources        = join(a:args, ' ')
	let l:tempfile       = tempname()
	let l:tsc_cmd        = printf('tsc --target es6 --out %s %s', l:tempfile, l:sources)
	let l:babel_cmd      = printf('babel-node %s', l:tempfile)
	let l:rm_cmd         = printf('%s %s', (has('win32') ? 'del' : 'rm'), l:tempfile)
	let s:sys_func       = exists('*vimproc#system') ? function('vimproc#system') : function('system')
	let l:scratch_opened = 0

	" Execute TypeScript as EcmaScript6
	for l:cmd in [l:tsc_cmd, l:babel_cmd, l:rm_cmd]
		let l:result = s:sys_func(l:cmd)
		if v:shell_error
			call s:angry(l:result)
			return
		else
			if !l:scratch_opened
				call s:open_scratch(l:result)
				let l:scratch_opened = 1
			else
				execute 'normal! i' l:result
			endif
		endif
	endfor

	" Delete blank line
	if l:scratch_opened
		normal! dd
	endif
endfunction
