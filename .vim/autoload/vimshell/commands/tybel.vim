let s:command = {
\	'name'        : 'tybel',
\	'kind'        : 'internal',
\	'description' : 'tybel (run|compile) source.ts... - composite tsc and babel'
\}

function! vimshell#commands#tybel#define()
	return s:command
endfunction

" --- --- --- "

" Enum TybelMode
let s:TybelMode = {
\	'run'       : 0,
\	'compile'   : 1,
\	'undefined' : -1
\} | lockvar! s:CmdMode

" TybelMode -> (Bool, String)
function! s:commands_available(tybel_mode)
	if !executable('tsc')
		return [0, 'tsc command is not found, tybel needs it']
	elseif (a:tybel_mode is s:TybelMode.run) && !executable('babel-node')
		return [0, 'babel-node command is not found, tybel needs it']
	elseif (a:tybel_mode is s:TybelMode.compile) && !executable('babel')
		return [0, 'babel command is not found, tybel needs it']
	endif
	return [1, 'tybel can use these']
endfunction

" String -> Void
function! s:angry(msg)
	execute 'normal! o' . printf('!!!>> %s!!!', a:msg)
endfunction

" String -> Void
function s:open_scratch(detail)
	new
	setl buftype=nofile filetype=scratch
	execute 'normal! i' a:detail
endfunction

" TybelMode -> [String] -> Void
function! s:execute_cmd(tybel_mode, cmds) abort
	let s:system_func    = exists('*vimproc#system') ? function('vimproc#system')
	\                                                : function('system')
	let l:scratch_opened = 0

	for l:cmd in a:cmds
		let l:result = s:system_func(l:cmd)
		if v:shell_error  "NOTE: Did ignored this ?
			call s:angry(l:result)
			return
		else
			if a:tybel_mode is s:TybelMode.compile
				continue
			endif
			if !l:scratch_opened
				call s:open_scratch(l:result)
				let l:scratch_opened = 1
			else
				execute 'normal! i' l:result
			endif
		endif
	endfor

	" Delete empty line & flatten result
	if l:scratch_opened
		normal! ddgg=G
		stopinsert
	endif
endfunction

function! s:command.execute(args, context)
	let l:tybel_mode = a:args[0] ==# 'run'     ? s:TybelMode.run
	\                : a:args[0] ==# 'compile' ? s:TybelMode.compile
	\                                          : s:TybelMode.undefined
	if l:tybel_mode is s:TybelMode.undefined
		call s:angry(printf('sub command "%s" is unknowned', a:args[0]))
		return
	endif

	let [l:available, l:message] = s:commands_available(l:tybel_mode)
	if !l:available
		call s:angry(l:message)
		return
	endif

	let l:sources        = join(a:args[1:], ' ')
	let l:tempfile       = tempname()
	"FIXME: happend error of 'file already exists'
	let l:tsc_cmd        = printf('tsc --target es6 --out %s %s', l:tempfile, l:sources)
	let l:babel_cmd      = (l:tybel_mode is s:TybelMode.run) ? printf('babel-node %s', l:tempfile)
	\                                                        : printf('babel --out-file a.out.js %s', l:tempfile)
	let l:rm_cmd         = (has('win32') ? 'del ' : 'rm ') . l:tempfile

	" Execute TypeScript as EcmaScript6
	call s:execute_cmd(l:tybel_mode, [l:tsc_cmd, l:babel_cmd, l:rm_cmd])

	" Append empty cmd line
	"TODO: this didn't append line, but replaced current line to empty line
	call vimshell#interactive#send('')
endfunction
