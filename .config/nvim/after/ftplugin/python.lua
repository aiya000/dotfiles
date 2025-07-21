
vim.opt_local.ts = 4
vim.opt_local.sw = 4
vim.opt_local.et = true

local augroup_FtpluginPython = vim.api.nvim_create_augroup("FtpluginPython", { clear = true })
  vim.api.nvim_create_autocmd("BufWritePre", { group = augroup_FtpluginPython, pattern = "*", callback = function() vim.cmd("LspDocumentFormatSync") end })
