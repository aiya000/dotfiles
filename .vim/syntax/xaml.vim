runtime syntax/xml.vim
highlight xamlNotice term=bold ctermfg=235 ctermbg=108 guifg=#262626 guibg=#87af87
call matchadd('xamlNotice', 'x:Name')
call matchadd('xamlNotice', 'x:Key')
" Check explicit default value
call matchadd('xamlNotice', '\<0,0,0,0\>')
call matchadd('xamlNotice', 'VerticalAlignment="Stretch"')
call matchadd('xamlNotice', 'HorizontalAlignment="Stretch"')
call matchadd('xamlNotice', 'Text=""')
call matchadd('xamlNotice', 'Content=""')
call matchadd('xamlNotice', 'Margin="0"')
call matchadd('xamlNotice', 'Padding="0"')
