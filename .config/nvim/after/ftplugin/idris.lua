vim.cmd("let s:Job = vital#vimrc#new().import('System.Job')")


vim.opt_local.ts = 2
vim.opt_local.sw = 2
vim.opt_local.et = true
vim.opt_local.conceallevel = 0
vim.opt.commentstring = " -- %s"

vim.cmd([[
function! s:idris(cmd) abort " {{{
    let all_output = []
    function! s:aggregate_stdout(_, data, __) abort closure
        call add(all_output, a:data[0])
    endfunction
]])
    vim.cmd([[
    function! s:aggregate_stderr(_, data, __) abort closure
        call add(all_output, a:data[0])
    endfunction
    ]])

    vim.cmd([[
    function! s:see_result(_, __, ___) abort closure
        let lines = join(all_output, "\n")

        if substitute(lines, '\(\n\|\r\)', '', 'g') ==# ''
            echo printf('no result of "%s" is existent', a:cmd)
            return
        elseif len(all_output) <= 2
            echo lines
            return
        endif

        "TODO: Use :pedit
        new | setl noswapfile buftype=nofile filetype=ftplugin-idris
        setl modifiable noreadonly

        1put!=lines
        normal! G"_ddgg

        setl nomodifiable
    endfunction
    ]])

    vim.call("s:Job.start(printf('idris --client "%s"', a:cmd), {")
        vim.cmd("\\ 'on_stdout': function('s:aggregate_stdout'),")
        vim.cmd("\\ 'on_stderr': function('s:aggregate_stdout'),")
        vim.cmd("\\ 'on_exit':   function('s:see_result'),")
    vim.cmd("\\})")
vim.cmd("endfunction \" }}}")

vim.cmd([[
function! s:with_cword(f, cmd) abort " {{{
    let cword = expand('<cword>')
    call a:f(printf('%s %s', a:cmd, cword))
endfunction " }}}
]])

vim.cmd([[
function! s:idris_cword(cmd) abort " {{{
    call s:with_cword(function('s:idris'), a:cmd)
endfunction " }}}
]])

vim.cmd([[
function! s:idris_clnum_word(cmd, word) abort " {{{
    let [_, lnum, __, ___, ____] = getcurpos()
    call s:idris(a:cmd . ' ' . lnum . ' ' . a:word)
endfunction " }}}
]])

vim.keymap.set('n', "R", function() vim.call("<SID>idris(':reload')") end, { buffer = true, silent = true })
vim.keymap.set('n', "T", function() vim.call("<SID>idris_cword(':type')") end, { buffer = true, silent = true })
vim.keymap.set('n', "d", function() vim.call("<SID>idris_cword(':doc')") end, { buffer = true, silent = true })
vim.keymap.set('n', "m", function() vim.call("<SID>idris_cword(':missing')") end, { buffer = true, silent = true })
vim.keymap.set('n', "o", function() vim.cmd("<C-u>vsp<CR>:IdrisRepl -p base -p contrib -p effects <C-r>=expand('%:p')<CR>") end, { buffer = true, silent = true })
vim.keymap.set('n', "p", function() vim.call("<SID>idris_clnum_word(':proofsearch', '<C-r>=expand('<cword>')[1:]<CR>')") end, { buffer = true, silent = true })
