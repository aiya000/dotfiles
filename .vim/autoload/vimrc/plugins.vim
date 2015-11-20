" Delete otiose lines
function! vimrc#plugins#weblio_filter(output) " {{{
	let l:lines = split(a:output, "\n")
	return join(l:lines[60 : ], "\n")
endfunction " }}}

" Open tweetvim by private account
function! vimrc#plugins#twitter_private() " {{{
	if !exists('g:vimrc.private["twitter"]["priv_ac"]')
		call vimrc#echo_error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
		return
	endif

	execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
	let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['priv_ac']

	TweetVimHomeTimeline
endfunction " }}}

" Open tweetvim_say by private account
function! vimrc#plugins#tweet_private() " {{{
	if !exists('g:vimrc.private["twitter"]["priv_ac"]')
		call vimrc#echo_error('Not set env variable => g:vimrc.private["twitter"]["priv_ac"]')
		return
	endif

	execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['priv_ac']
	TweetVimSay

	"@Incomplete('wait sync here')
	"execute ':TweetVimSwitchAccount' g:vimrc.private['twitter']['curr_ac']
endfunction " }}}

" Open tweetvim by public account
function! vimrc#plugins#twitter_public() " {{{
	if !exists("g:vimrc.private['twitter']['publ_ac']")
		call vimrc#echo_error("Not set env variable => g:vimrc.private['twitter']['publ_ac']")
		return
	endif

	execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
	let g:vimrc.private['twitter']['curr_ac'] = g:vimrc.private['twitter']['publ_ac']

	TweetVimHomeTimeline
endfunction " }}}

" Open tweetvim_say by public account
function! vimrc#plugins#tweet_public() " {{{
	if !exists('g:vimrc.private["twitter"]["publ_ac"]')
		call vimrc#echo_error('Not set env variable => g:vimrc.private["twitter"]["publ_ac"]')
		return
	endif

	execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['publ_ac']
	TweetVimSay

	"@Incomplete('wait here')
	"execute ':TweetVimSwitchAccount ' g:vimrc.private['twitter']['curr_ac']
endfunction " }}}
