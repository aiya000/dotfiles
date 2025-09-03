-- NOTE: Don't use the mark Z, this is reserved by some my functions.

local helper = require('helper')
local git = require('git')
local fn = require('utils.functions')
local s = fn.s

-------------------
-- Global values --
-------------------
-- InitLua global configuration {{{

InitLua = InitLua or {
  loaded = false,
  neovim_home = vim.fn.expand('~/.config/nvim'),
  path_at_started = vim.fn.getcwd(),
  is_wsl = vim.fn.executable('uname') == 1 and vim.fn.system('uname -a'):match('microsoft%-standard'),
  is_unix = vim.fn.has('unix') == 1,
  is_macos = vim.fn.has('macunix') == 1,
  git_root = nil,
  memo_path = '~/.backup/memo.md',
}

-- Delayed to avoid startup slowdown
vim.defer_fn(function()
  git.read_git_root(function(git_root)
    InitLua.git_root = git_root
    io.write(s'vimrc: a git root detected: {git_root}')
  end)
end, 100)

InitLua.open_on_gui =
  InitLua.is_macos and 'open'
  or InitLua.is_wsl and 'wslview'
  or InitLua.is_unix and 'xdg-open'
  or error('init.lua: No method found for opening GUI')

local backupdir = vim.fn.expand('~/.backup/vim-backup')
InitLua.backupdir = backupdir
InitLua.directory = s'{backupdir}/swp'
InitLua.undodir = s'{backupdir}/undo'
InitLua.viewdir = s'{backupdir}/view'
InitLua.sessiondir = s'{backupdir}/session'

-- Please see 'vimrc#bufclose_filetype()'.
InitLua.temporary_buftypes = {
  'aref_web',
  'diff',
  'gin-branch',
  'gin-log',
  'gin-status',
  'gitdiffviewer',
  'gitlogviewer',
  'gitreflogviewer',
  'gitshowviewer',
  'help',
  'man',
  'netrw',
  'dirvish',
  'quickrun',
  'scratch',
  'ddu-ff',
  'ddu-filter',
  'fern',
}

-- }}}
-- Options {{{

vim.g.mapleader = "'"
vim.g.maplocalleader = "'"
-- Use below if you are using JIS keyboard
-- vim.g.mapleader = '['
-- vim.g.maplocalleader = '['

vim.opt.autoindent = true
vim.opt.backspace = { 'indent', 'eol', 'start' }
vim.opt.breakindent = true
vim.opt.cindent = true
vim.opt.cmdheight = 2
vim.opt.cmdwinheight = 20
vim.opt.completeopt:remove('preview')
vim.opt.conceallevel = 1
vim.opt.expandtab = true
vim.opt.fileencodings = {
  'ucs-bom',
  'utf-8',
  'sjis',
  'euc-jp',
  'cp932',
  'iso-2022-jp-3',
  'iso-2022-jp',
  'eucjp-ms',
  'euc-jisx0213',
  'ucs-bom',
  'latin1',
  'default',
}
vim.opt.hidden = true
vim.opt.history = 10000
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.laststatus = 2
vim.opt.linebreak = true
vim.opt.list = true
vim.opt.listchars = { tab = '»_', trail = '_', extends = '»', precedes = '«', nbsp = '%', eol = '↲' }
vim.opt.matchpairs:append({ '<:>', '（:）', '｛:｝', '「:」', '＜:＞', '『:』', '【:】' })
vim.opt.foldenable = false
vim.opt.joinspaces = false
vim.opt.timeout = false
vim.opt.wrap = false
vim.opt.wrapscan = false
vim.opt.number = true
vim.opt.omnifunc = 'v:lua.vim.lsp.omnifunc'
vim.opt.path = { '.', ',', './*' }
vim.opt.previewheight = 40
vim.opt.relativenumber = true
vim.opt.scrolloff = 16
vim.opt.sessionoptions = { 'buffers', 'tabpages', 'localoptions', 'winsize', 'winpos' }
vim.opt.shellslash = true
vim.opt.shiftwidth = 2
vim.opt.suffixes = {}
vim.opt.tabstop = 2
vim.opt.shada = "'400,<50,s10,h"
vim.opt.visualbell = true
vim.opt.wildignorecase = true
vim.opt.wildmenu = true
vim.opt.ambiwidth = 'double'
vim.opt.background = 'dark'
vim.opt.showtabline = 2

-- TODO: なぜか怒られたので一旦コメントアウト
-- vim.opt.ruler = false

-- Fold options
vim.opt.foldcolumn = '1'
vim.opt.foldopen = { 'search', 'jump', 'mark', 'percent', 'insert', 'tag', 'undo' }
vim.opt.foldclose = 'all'
vim.opt.foldmethod = 'marker'
vim.opt.fillchars = { vert = '|', fold = ' ' }

-- Backup options
vim.opt.directory = InitLua.directory
vim.opt.viewdir = InitLua.viewdir
vim.opt.undofile = true
vim.opt.undodir = InitLua.undodir
vim.opt.backup = false

-- Always I use the ignorecase
vim.opt.ignorecase = true
vim.opt.infercase = false

-- I control the IME state by myself
vim.opt.iminsert = 0

-- Open .tex as LaTex
vim.g.tex_flavor = 'latex'

-- Reference tags of ctags
---@diagnostic disable-next-line: undefined-field --なぜか怒られるので無視する
vim.opt.tags:append({
  'tags',
  '.git/tags',
  s'{InitLua.path_at_started}/tags',
  s'{InitLua.path_at_started}/.git/tags',
})

-- }}}
-- Others {{{

-- TODO: Remove this after https://github.com/aiya000/bash-toys/issues/12 fixed
-- Please see https://github.com/aiya000/bash-toys
vim.env.BASH_TOYS_DUSTBOX_DIR = vim.fn.expand('~/.backup/dustbox')

-- }}}

-------------
-- Prepare --
-------------
-- Set encoding {{{

if not InitLua.loaded then
  vim.opt.fileencoding = 'utf-8'
  vim.opt.encoding = 'utf-8'
end

-- }}}
-- Prepare dein.vim {{{

-- TODO: Future migration to lazy.nvim or something. For now, keep using dein.vim

local dein_dir = s'{InitLua.neovim_home}/bundle/repos/github.com/Shougo/dein.vim'
helper.install_dein_if_not_installed(dein_dir)
---@diagnostic disable-next-line: undefined-field --なぜか怒られるので無視する
vim.opt.runtimepath:append(dein_dir)

vim.call('dein#begin', s'{InitLua.neovim_home}/bundle')
vim.call('dein#load_toml', '~/.config/nvim/dein.toml', { lazy = false })
vim.call('dein#load_toml', '~/.config/nvim/dein_lazy.toml', { lazy = true })
vim.call('dein#add', 'Shougo/dein.vim', { rtp = '' })

-- TODO: Generate dein.vim doc by :helptags

-- }}}
-- Prepare backup directories {{{

helper.ensure_directory(InitLua.directory)
helper.ensure_directory(InitLua.undodir)
helper.ensure_directory(InitLua.sessiondir)

-- }}}
-- Read local scripts {{{

local init_private_lua = vim.fn.expand('~/.dotfiles/.private/nvim_init_private.lua')
if vim.fn.filereadable(init_private_lua) == 1 then
  vim.cmd(s'source {init_private_lua}')
end

local init_env_lua = vim.fn.expand('~/.config/nvim/init_env.lua')
if vim.fn.filereadable(init_env_lua) == 1 then
  vim.cmd(s'source {init_env_lua}')
end

-- }}}

require('autocmds')
require('plugins')
require('keymaps')
vim.call('dein#end')

local env_post_vimrc = vim.fn.expand('~/.vimrc_env_post')
if vim.fn.filereadable(env_post_vimrc) == 1 then
  vim.cmd(s'source {env_post_vimrc}')
end

vim.cmd('filetype plugin indent on')
vim.cmd('syntax enable')
InitLua.loaded = true
