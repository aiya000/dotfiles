-- NOTE: Don't use the mark Z, this is reserved by some my functions.

-- Debug: Confirm init.lua is being loaded (remove these lines when satisfied)
-- print("🚀 init.lua is loading...")
-- vim.cmd('echo "✅ init.lua loaded successfully!"')

-- Global values
vim.g.vimrc = vim.g.vimrc or {
  loaded = false,
  vim_home = vim.fn.expand('~/.config/nvim'),
  path_at_started = vim.fn.getcwd(),
  is_wsl = vim.fn.executable('uname') == 1 and vim.fn.system('uname -a'):match('microsoft%-standard'),
  is_unix = vim.fn.has('unix') == 1,
  is_macos = vim.fn.has('macunix') == 1,
  git_root = nil,
  memo_path = '~/.backup/memo.md',
  is_ddc_enabled = nil,
}

-- Call git root setup function (delayed to avoid startup slowdown)
vim.defer_fn(function()
  local vimrc = require('vimrc')
  vimrc.read_git_root_to_set_g_vimrc_async()
end, 100)

vim.g.vimrc.open_on_gui = vim.g.vimrc.is_macos and 'open' or
                         vim.g.vimrc.is_wsl and 'wslview' or
                         vim.g.vimrc.is_unix and 'xdg-open' or
                         'echo "no method for GUI-open"'

local backupdir = vim.fn.expand('$HOME/.backup/vim-backup')
vim.g.vimrc.backupdir = backupdir
vim.g.vimrc.directory = backupdir .. '/swp'
vim.g.vimrc.undodir = backupdir .. '/undo'
vim.g.vimrc.viewdir = backupdir .. '/view'
vim.g.vimrc.sessiondir = backupdir .. '/session'

-- Please see 'vimrc#bufclose_filetype()'.
vim.g.vimrc.temporary_buftypes = {
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
  'stack_build',
  'fern',
}

-- TODO: Remove this after https://github.com/aiya000/bash-toys/issues/12 fixed
-- Please see https://github.com/aiya000/bash-toys
vim.env.BASH_TOYS_DUSTBOX_DIR = vim.fn.expand('$HOME/.backup/dustbox')

local typescript_variants = {
  'typescript',
  'javascript',
  'vue',
  'typescript.tsx',
  'javascript.jsx',
}

-- Set encoding
if not vim.g.vimrc.loaded then
  vim.opt.fileencoding = 'utf-8'
  vim.opt.encoding = 'utf-8'
end

-- Prepare backup directories
local function ensure_directory(dir)
  if vim.fn.isdirectory(dir) == 0 then
    vim.fn.mkdir(dir, 'p', 0x1c0) -- 0700 in octal
    vim.fn.system(string.format('chown -R "%s:%s" "%s"', vim.env.USER or '', vim.env.GROUP or '', dir))
  end
end

ensure_directory(vim.g.vimrc.directory)
ensure_directory(vim.g.vimrc.undodir)
ensure_directory(vim.g.vimrc.sessiondir)

-- Load private configuration
local private_vimrc = vim.fn.expand('$HOME/.dotfiles/.private/.vimrc_private')
if vim.fn.filereadable(private_vimrc) == 1 then
  vim.cmd('source ' .. private_vimrc)
end

local env_vimrc = vim.fn.expand('$HOME/.vimrc_env')
if vim.fn.filereadable(env_vimrc) == 1 then
  vim.cmd('source ' .. env_vimrc)
end

-- Options
vim.opt.autoindent = true
vim.opt.backspace = {'indent', 'eol', 'start'}
vim.opt.breakindent = true
-- vim.opt.browsedir = 'buffer'  -- Not supported in Neovim
vim.opt.cindent = true
vim.opt.cmdheight = 2
vim.opt.cmdwinheight = 20
vim.opt.completeopt:remove('preview')
vim.opt.conceallevel = 1
vim.opt.expandtab = true
vim.opt.fileencodings = {'ucs-bom', 'utf-8', 'sjis', 'euc-jp', 'cp932', 'iso-2022-jp-3', 'iso-2022-jp', 'eucjp-ms', 'euc-jisx0213', 'ucs-bom', 'latin1', 'default'}
vim.opt.helplang = 'en'
vim.opt.hidden = true
vim.opt.history = 1000
vim.opt.hlsearch = true
vim.opt.incsearch = true
vim.opt.laststatus = 2
vim.opt.linebreak = true
vim.opt.list = true
vim.opt.listchars = {tab = '»_', trail = '_', extends = '»', precedes = '«', nbsp = '%', eol = '↲'}
vim.opt.matchpairs:append({'<:>', '（:）', '｛:｝', '「:」', '＜:＞', '『:』', '【:】'})
vim.opt.foldenable = false
vim.opt.joinspaces = false
vim.opt.ruler = false
vim.opt.timeout = false
vim.opt.wrap = false
vim.opt.wrapscan = false
vim.opt.number = true
vim.opt.omnifunc = 'v:lua.vim.lsp.omnifunc'
vim.opt.path = {'.',',', './*'}
vim.opt.previewheight = 40
vim.opt.relativenumber = true
vim.opt.scrolloff = 16
vim.opt.sessionoptions = {'buffers', 'tabpages', 'localoptions', 'winsize', 'winpos'}
vim.opt.shellslash = true
vim.opt.shiftwidth = 2
vim.opt.suffixes = {}
vim.opt.tabstop = 2
-- vim.opt.viminfo = "'400,<50,s10,h"  -- Use shada in Neovim
vim.opt.visualbell = true
vim.opt.wildignorecase = true
vim.opt.wildmenu = true

-- Simple tabline function (temporary replacement for vimrc#tabline#make())
function _G.simple_tabline()
  local line = ''
  local tab_count = vim.fn.tabpagenr('$')
  local current_tab = vim.fn.tabpagenr()
  
  -- Show tab count
  line = line .. '[' .. tab_count .. '] '
  
  -- Show tabs
  for i = 1, tab_count do
    local buflist = vim.fn.tabpagebuflist(i)
    local winnr = vim.fn.tabpagewinnr(i)
    local bufnr = buflist[winnr]
    local bufname = vim.fn.bufname(bufnr)
    local filename = vim.fn.fnamemodify(bufname, ':t')
    
    if filename == '' then
      filename = '[NoName]'
    end
    
    -- Highlight current tab
    if i == current_tab then
      line = line .. '[* ' .. filename .. ' *]'
    else
      line = line .. '[' .. filename .. ']'
    end
  end
  
  -- Show working directory
  local cwd = vim.fn.getcwd()
  line = line .. ' => [PWD=' .. vim.fn.fnamemodify(cwd, ':~') .. ']'
  
  return line
end

-- Tabline - temporary simple version until we convert vimrc#tabline#make()
vim.opt.tabline = '%!v:lua.simple_tabline()'

-- Debug: Options loaded
-- print("⚙️  Basic options configured")

vim.opt.ambiwidth = 'double'
vim.opt.background = 'dark'

-- TODO: Convert colorscheme
-- vim.cmd('colorscheme lucariox')
vim.cmd('colorscheme default')

vim.opt.showtabline = 2

-- Set the fold options  
vim.opt.foldcolumn = '1'
vim.opt.foldopen = {'search', 'jump', 'mark', 'percent', 'insert', 'tag', 'undo'}
vim.opt.foldclose = 'all'
vim.opt.foldmethod = 'marker'
vim.opt.fillchars = {vert = '|', fold = ' '}

-- The backup options
vim.opt.directory = vim.g.vimrc.directory
vim.opt.viewdir = vim.g.vimrc.viewdir
vim.opt.undofile = true
vim.opt.undodir = vim.g.vimrc.undodir
vim.opt.backup = false

-- Always I use the ignorecase
vim.opt.ignorecase = true
vim.opt.infercase = false

-- I control the IME state by myself
vim.opt.iminsert = 0

-- Open .tex as LaTex
vim.g.tex_flavor = 'latex'

-- Reference tags of ctags
vim.opt.tags:append({
  'tags',
  '.git/tags',
  vim.g.vimrc.path_at_started .. '/tags',
  vim.g.vimrc.path_at_started .. '/.git/tags',
})

vim.g.mapleader = '['
vim.g.maplocalleader = '['

-- TODO: Future migration to lazy.nvim
-- For now, we'll keep the existing dein.vim setup until the basic conversion is complete

-- Prepare dein.vim (keeping original logic for compatibility)
local dein_dirname = vim.g.vimrc.vim_home .. '/bundle/repos/github.com/Shougo/dein.vim'
vim.opt.runtimepath:append(dein_dirname)

-- Try to initialize dein.vim
local dein_ok = pcall(function()
  vim.cmd('call dein#begin("' .. vim.fn.expand('$HOME/.vim/bundle') .. '")')
end)

if not dein_ok then
  -- If dein.vim is not found, we'll continue without plugins for now
  print("Note: dein.vim not found. Continuing without plugin manager.")
end

-- Load configuration modules
-- Plugin config will be loaded later when we migrate to lazy.nvim
-- require('plugins.config')
require('keymaps')
require('autocmds')

-- Load post-configuration (disabled until dein.vim is properly set up)
-- local env_post_vimrc = vim.fn.expand('$HOME/.vimrc_env_post')
-- if vim.fn.filereadable(env_post_vimrc) == 1 then
--   vim.cmd('source ' .. env_post_vimrc)
-- end

-- Enable filetype plugin and syntax
vim.cmd('filetype plugin indent on')
vim.cmd('syntax enable')

vim.g.vimrc.loaded = true

-- Debug: Confirm init.lua completed loading
-- print("🎉 init.lua loading completed!")
-- vim.cmd('echo "🎯 All configurations loaded successfully!"')
