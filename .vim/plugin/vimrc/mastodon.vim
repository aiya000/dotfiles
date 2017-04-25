command! -bar Mstdn       execute 'MastodonHome' g:vimrc['private']['mastodon'].curr_ac[0] g:vimrc['private']['mastodon'].curr_ac[1]
command! -bar MstdnPublic execute 'MastodonHome' g:vimrc['private']['mastodon'].publ_ac[0] g:vimrc['private']['mastodon'].publ_ac[1]
command! -bar Toot        execute 'MastodonSay' g:vimrc['private']['mastodon'].curr_ac[0] g:vimrc['private']['mastodon'].curr_ac[1]
command! -bar TootPublic  execute 'MastodonSay' g:vimrc['private']['mastodon'].publ_ac[0] g:vimrc['private']['mastodon'].publ_ac[1]
