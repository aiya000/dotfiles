let s:save_cpo = &cpo
set cpo&vim

let s:command = {
\	'name' : 'tweet',
\	'kind' : 'internal',
\	'description' : 'tweet {message}... | and referenced to yomi322.'
\}

function! vimshell#commands#tweet#define()
	return s:command
endfunction


" --- --- --- "

function! s:command.execute(args, context)
	if exists(':TweetVimCommandSay')
		execute ':TweetVimCommandSay ' . join(a:args)
	else
		let l:message = 'TweetVim is not available !'
		call vimshell#error_line(a:context.fd, l:message)
	endif
endfunction

" --- --- --- "


let &cpo = s:save_cpo
unlet s:save_cpo
