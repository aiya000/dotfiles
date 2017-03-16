function! nvimrc#open_weechat() abort
	terminal weechat
	set ft=term-weechat
	" See .private
	if exists('g:vimrc["weechat"]["security_passphrase"]')
		put=g:vimrc['weechat']['security_passphrase']
	endif
endfunction
