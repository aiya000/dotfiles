function! nvimrc#open_weechat() abort
	terminal weechat
	set ft=term-weechat
	" See .private
	if exists('g:vimrc["weechat"]["security_passphrase"]')
		put=g:vimrc['weechat']['security_passphrase']
	endif
endfunction


function! nvimrc#open_stack_exec_ghci() abort
	terminal stack exec ghci
	set ft=term-stack-exec-ghci
endfunction


function! nvimrc#open_stack_ghci() abort
	terminal stack ghci
	set ft=term-stack-ghci
endfunction
