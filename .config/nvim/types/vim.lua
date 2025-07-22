--[[
Neovim API type definitions (generated from official documentation)
Reference: /usr/local/share/nvim/runtime/doc/lua.txt

Usage:
1. Put this file to {project-root}/types/vim.lua -- For example, {project-root} means ~/.config/nvim, a Neovim plugin project, or etc
2. Copy .luarc.json (See https://gist.github.com/aiya000/90e6083c68239bfe6bbf4e89af9cdaa5) to {project-root}/.luarc.json
--]]

---@class vim
---@field api vim.api
---@field fn vim.fn
---@field g table
---@field b table
---@field w table
---@field t table
---@field v table
---@field env table
---@field opt vim.opt
---@field opt_global vim.opt
---@field opt_local vim.opt
---@field o table
---@field go table
---@field bo table
---@field wo table
---@field cmd fun(command: string|table)
---@field defer_fn fun(fn: function, timeout: number): table
---@field schedule fun(fn: function)
---@field deprecate fun(name: string, alternative: string, version: string, plugin?: string, backtrace?: boolean)
---@field inspect fun(obj: any, opts?: table): string
---@field keycode fun(str: string): string
---@field notify fun(msg: string, level?: number, opts?: table)
---@field notify_once fun(msg: string, level?: number, opts?: table)
---@field print fun(...: any)
---@field split fun(s: string, sep: string, opts?: table): string[]
---@field join fun(sep: string, list: string[]): string
---@field trim fun(s: string): string
---@field startswith fun(s: string, prefix: string): boolean
---@field endswith fun(s: string, suffix: string): boolean
---@field tbl_deep_extend fun(behavior: string, ...: table): table
---@field tbl_extend fun(...: table): table
---@field tbl_islist fun(t: table): boolean
---@field tbl_isempty fun(t: table): boolean
---@field tbl_keys fun(t: table): any[]
---@field tbl_values fun(t: table): any[]
---@field system fun(cmd: string[], opts?: table): table
---@field wait fun(time: number, callback?: function, interval?: number, fast_only?: boolean): boolean, number
---@field log vim.log
---@field keymap vim.keymap
---@field fs vim.fs
---@field ui vim.ui
---@field hl vim.hl
---@field filetype vim.filetype
---@field spell vim.spell
---@field diff fun(a: string[], b: string[], opts?: table): table[]
---@field base64 vim.base64
---@field json vim.json
---@field mpack vim.mpack
---@field lsp vim.lsp
---@field diagnostic vim.diagnostic
---@field treesitter vim.treesitter
---@field glob vim.glob
---@field lpeg vim.lpeg
---@field loader vim.loader
---@field health vim.health
---@field NIL userdata
---@field type_idx number
---@field val_idx number
---@field types table
---@field empty_dict fun(): table
---@field iconv fun(str: string, from: string, to: string): string
---@field in_fast_event fun(): boolean
---@field rpcrequest fun(channel: number, method: string, ...: any): any
---@field rpcnotify fun(channel: number, method: string, ...: any)
---@field str_utf_end fun(str: string, index: number): number
---@field str_utf_start fun(str: string, index: number): number
---@field str_utf_pos fun(str: string): number[]
---@field stricmp fun(a: string, b: string): number
---@field ui_attach fun(ns: number, options: table, callback: function)
---@field ui_detach fun(ns: number)
---@field call fun(func: string|function, ...: any): any
---@field paste function|nil

--- Neovim API Functions
---@class vim.api

--- Vim Functions
---@class vim.fn
---@field expand fun(expr: string): string パスやファイル名を展開
---@field executable fun(name: string): number コマンドが実行可能かチェック
---@field line fun(expr: string): number 行番号を取得
---@field col fun(expr: string): number カラム位置を取得
---@field exists fun(expr: string): number 変数や関数の存在チェック
---@field has fun(feature: string): number 機能サポートチェック
---@field printf fun(fmt: string, ...: any): string 文字列フォーマット
---@field reverse fun(list: any[]): any[] リスト逆順
---@field jobstart fun(cmd: string, opts?: table): number ジョブ開始
---@field pumvisible fun(): number ポップアップメニュー表示状態

--- Vim Option Type
---@class vim.Option

--- Option setting (set format)
---@class vim.opt
---@field smarttab boolean|vim.Option
---@field expandtab boolean|vim.Option
---@field shiftwidth number|vim.Option
---@field tabstop number|vim.Option
---@field number boolean|vim.Option
---@field relativenumber boolean|vim.Option
---@field wrap boolean|vim.Option
---@field linebreak boolean|vim.Option
---@field showbreak string|vim.Option
---@field wildignore string[]|vim.Option
---@field listchars table|vim.Option
---@field formatoptions table|vim.Option
---@field shortmess string|vim.Option
---@field whichwrap string|vim.Option

--- Log level constants
---@class vim.log
---@field levels vim.log.levels

---@class vim.log.levels
---@field TRACE number
---@field DEBUG number
---@field INFO number
---@field WARN number
---@field ERROR number
---@field OFF number

--- Keymap management
---@class vim.keymap
---@field set fun(mode: string|string[], lhs: string, rhs: string|function, opts?: table)
---@field del fun(modes: string|string[], lhs: string, opts?: table)

--- File System Operations
---@class vim.fs
---@field abspath fun(path: string): string
---@field basename fun(file: string): string
---@field dirname fun(file: string): string
---@field joinpath fun(...: string): string
---@field normalize fun(path: string, opts?: table): string
---@field find fun(names: string[], opts?: table): string[]
---@field dir fun(path: string, opts?: table): function
---@field parents fun(start: string): function
---@field root fun(source: string, marker: string|string[]): string|nil
---@field rm fun(path: string, opts?: table)

--- UI Operation
---@class vim.ui
---@field input fun(opts: table, on_confirm: function)
---@field select fun(items: any[], opts: table, on_choice: function)
---@field open fun(path: string, opts?: table): boolean, string|nil

--- Highlight Operation
---@class vim.hl
---@field on_yank fun(opts?: table)
---@field range fun(bufnr: number, ns: number, higroup: string, start: number[], finish: number[], opts?: table)
---@field priorities table

--- File Type Management
---@class vim.filetype
---@field add fun(filetypes: table)
---@field match fun(args: table): string|nil
---@field get_option fun(filetype: string, option: string): any

--- Spell Check
---@class vim.spell
---@field check fun(str: string): table[]

--- Base64 encoding
---@class vim.base64
---@field encode fun(str: string): string
---@field decode fun(str: string): string

--- JSON processing
---@class vim.json
---@field encode fun(obj: any, opts?: table): string
---@field decode fun(str: string, opts?: table): any

--- MessagePack Serialization
---@class vim.mpack
---@field encode fun(obj: any): string
---@field decode fun(str: string): any

--- LSP (Language Server Protocol)
---@class vim.lsp
---@field config fun(name: string, opts: table): table
---@field enable fun(name: string, bufnr?: number)
---@field start fun(config: table): number
---@field stop_client fun(client_id: number, force?: boolean)
---@field get_client_by_id fun(client_id: number): table|nil
---@field get_clients fun(opts?: table): table[]
---@field omnifunc fun(find_start: number): any
---@field formatexpr fun(): any
---@field tagfunc fun(pattern: string, flags: string, info: table): table[]
---@field handlers table
---@field buf vim.lsp.buf
---@field completion table

--- LSP buffer operation
---@class vim.lsp.buf
---@field hover fun()
---@field definition fun()
---@field declaration fun()
---@field implementation fun()
---@field references fun()
---@field rename fun()
---@field code_action fun(opts?: table)
---@field format fun(opts?: table)
---@field signature_help fun()
---@field document_symbol fun()
---@field workspace_symbol fun(query?: string)
---@field type_definition fun()

--- Diagnostics
---@class vim.diagnostic
---@field config fun(opts?: table, namespace?: number)
---@field enable fun(bufnr?: number, namespace?: number)
---@field disable fun(bufnr?: number, namespace?: number)
---@field get fun(bufnr?: number, opts?: table): table[]
---@field set fun(namespace: number, bufnr: number, diagnostics: table[], opts?: table)
---@field reset fun(namespace?: number, bufnr?: number)
---@field show fun(namespace?: number, bufnr?: number, diagnostics?: table[], opts?: table)
---@field hide fun(namespace?: number, bufnr?: number)
---@field goto_next fun(opts?: table)
---@field goto_prev fun(opts?: table)
---@field open_float fun(opts?: table, ...: any): table, number
---@field setloclist fun(opts?: table)
---@field setqflist fun(opts?: table)
---@field severity vim.diagnostic.severity
---@field handlers table

--- Diagnostic level
---@class vim.diagnostic.severity
---@field ERROR number
---@field WARN number
---@field INFO number
---@field HINT number

--- Tree-sitter
---@class vim.treesitter
---@field language_version number
---@field minimum_language_version number
---@field start fun(bufnr?: number, lang?: string): boolean
---@field stop fun(bufnr?: number)
---@field get_parser fun(bufnr?: number, lang?: string, opts?: table): table
---@field get_node fun(opts?: table): table|nil
---@field get_node_text fun(node: table, source?: number|string): string
---@field get_node_range fun(node_or_range: table): number, number, number, number
---@field get_captures_at_cursor fun(winnr?: number): table[]
---@field get_captures_at_pos fun(bufnr: number, row: number, col: number): table[]
---@field foldexpr fun(lnum: number): string
---@field language vim.treesitter.language
---@field query vim.treesitter.query

--- Tree-sitter language management
---@class vim.treesitter.language
---@field add fun(lang: string, opts?: table)
---@field register fun(lang: string, opts: table)
---@field get_lang fun(filetype: string): string

--- Tree-sitter query
---@class vim.treesitter.query
---@field get fun(lang: string, query_name: string): table|nil
---@field parse fun(lang: string, query: string): table
---@field list_predicates fun(): string[]
---@field list_directives fun(): string[]
---@field add_predicate fun(name: string, handler: function, opts?: table)
---@field add_directive fun(name: string, handler: function, opts?: table)

--- glob pattern matching
---@class vim.glob
---@field to_lpeg fun(pattern: string): table

--- LPeg pattern matching
---@class vim.lpeg
---@field match fun(pattern: table, subject: string, init?: number, ...: any): any
---@field P fun(value: any): table
---@field R fun(...: string): table
---@field S fun(string: string): table
---@field C fun(patt: table): table
---@field Cc fun(...: any): table
---@field Ct fun(patt: table): table
---@field locale fun(tab?: table): table
---@field setmaxstack fun(max: number)
---@field B fun(pattern: table): table
---@field Cb fun(name: string): table
---@field Carg fun(n: number): table
---@field Cf fun(patt: table, func: function): table
---@field Cg fun(patt: table, name?: string): table
---@field Cmt fun(patt: table, fn: function): table
---@field Cp fun(): table
---@field Cs fun(patt: table): table

--- Module Loader
---@class vim.loader
---@field enable fun(enable?: boolean)
---@field find fun(modname: string, opts?: table): table[]
---@field reset fun(path?: string)

--- Health Check
---@class vim.health
---@field start fun(name: string)
---@field ok fun(msg: string)
---@field error fun(msg: string, ...: any)
---@field warn fun(msg: string, ...: any)
---@field info fun(msg: string)

-- Explicit global vim variable types
---@type vim
vim = vim

--[[
The MIT License (MIT)

Copyright (c) 2025 aiya000

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.
--]]
