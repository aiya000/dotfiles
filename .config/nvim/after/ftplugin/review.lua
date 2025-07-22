vim.opt_local.sw = 4
vim.opt_local.ts = 4
vim.opt_local.et = true
vim.opt_local.spell = false

vim.cmd("vmap <expr> ix textobj#from_regexp#mapexpr('@<[a-z]\\+>{\\zs.*\\ze}')")
vim.cmd("vmap <expr> ax textobj#from_regexp#mapexpr('@<[a-z]\\+>{.*}')")
