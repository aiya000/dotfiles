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

---なぜか`fun(x: -1 | 0 | 1)`のように型を書けない。かつaliasを介し、かつaliasでは-1を先頭に持ってこなければうまくいくので、定義する
---@alias Tri 1 | 0 | -1

---@class VimFunctions : vim.fn
---@field getcwd fun(): string
---@field system fun(cli: string): string
---@field filereadable fun(path_to_file: string): boolean
---@field fnameescape fun(path: string): string
---@field getchar fun(expr?: Tri, opts?: table): string
---@field char2nr fun(str: string, utf8?: unknown): integer

---タプル`[row: integer, col: integer]`が今使ってるlspだと認識できないので、代用。
---必ずちょうど2要素を渡すこと。
---@alias Position integer[]

---@class VimApi : vim.api
---@field nvim_win_set_cursor fun(win_id: integer, pos: Position): nil
---@field nvim_buf_delete fun(buf_id: integer, opts: { force: boolean, unload: boolean }): nil

-- TODO: なぜかvim.cmdを認識しないので、手動で従来の`string | table`を加えている
---@alias VimCmd string | table | { colorscheme: fun(name: string): nil }

---@class vim
---@field list_extend fun(xs: unknown[], ys: unknown[]): unknown[] --TODO: 型引数持たせられない？ lspのバージョンを上げたら、型引数を持たせる
---@field opt VimOptions
---@field fn VimFunctions
---@field api VimApi
---@field cmd VimCmd

-- Attach defined types to `vim` variable
---@type vim
vim = vim
