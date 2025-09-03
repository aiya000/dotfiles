---なぜか現段階で、sumneko-lua-language-serverがいくつかのフィールドに対して「そのフィールドは知らない」って言ってくるので、自前で定義を追加する

-- TODO: これって正しい？ 何か継承した方がいいクラスがあったりする？
---Neovim's options such as `expandtab`, `number`, `completeopt` and so on
---@class VimOption
---@field append fun(self: VimOption, value: string|string[]): VimOption
---@field remove fun(self: VimOption, value: string|string[]): VimOption
---@field get fun(self: VimOption): unknown

---@class VimOptions : vim.opt
---@field completeopt VimOption
---@field matchpairs VimOption

---@class VimFunctions : vim.fn
---@field getcwd fun(): string
---@field system fun(cli: string): string
---@field filereadable fun(path_to_file: string): boolean
---@field fnameescape fun(path: string): string

---@class vim
---@field list_extend fun(xs: unknown[], ys: unknown[]): unknown[] --TODO: 型引数持たせられない？ lspのバージョンを上げたら、型引数を持たせる
---@field opt VimOptions
---@field fn VimFunctions

-- Attach defined types to `vim` variable
---@type vim
vim = vim
