"-- Basic --"
command! -bar Twitter            TweetVimHomeTimeline
command! -bar TwitterTab         tabnew | Twitter
command! -bar Tweet              TweetVimSay

"-- Private Account --"
command! -bar TwitterPrivate     call vimrc#plugins#twitter_private()
command! -bar TwitterPrivateTab  tabnew | TwitterPrivate
command! -bar TweetPrivate       call vimrc#plugins#tweet_private()

"-- Public Account --"
command! -bar TwitterPublic      call vimrc#plugins#twitter_public()
command! -bar TwitterPublicTab   tabnew | TwitterPublic
command! -bar TweetPublic        call vimrc#plugins#tweet_public()
