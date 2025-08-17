vim.cmd("execute 'source' (InitLua.neovim_home . '/after/ftplugin/tweetvim_say.vim')")
vim.cmd('resize 5')

vim.cmd('nmap <buffer> <CR> <Plug>(adrone_say_post)')
