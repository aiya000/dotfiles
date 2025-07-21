
vim.opt.commentstring = " /*%s*/"
vim.opt_local.tabstop = 2
vim.opt_local.shiftwidth = 2
vim.opt_local.expandtab = true
vim.opt.errorformat = "[%t%.%#] %f:%l:%m"

vim.keymap.set('n', "w", function() vim.cmd("<C-u>QuickfixRunSbtCompileWatch") end, { buffer = true, silent = true })
vim.keymap.set('n', "W", function() vim.cmd("<C-u>QuickfixStopSbtCompileWatch") end, { buffer = true, silent = true })

local augroup_FtpluginScala = vim.api.nvim_create_augroup("FtpluginScala", { clear = true })
    " Clear past results for :QuickfixRunSbtCompileWatch
    vim.api.nvim_create_autocmd("BufWritePost", { group = augroup_FtpluginScala, pattern = "*.scala", callback = function() vim.cmd("CClear") end })
