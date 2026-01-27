# dotfiles

Personal dotfiles collection for various development tools and environments.

## 📊 Repository Insights

![GitHub commit activity](https://img.shields.io/github/commit-activity/y/aiya000/dotfiles)
![GitHub last commit](https://img.shields.io/github/last-commit/aiya000/dotfiles)
![GitHub repo size](https://img.shields.io/github/repo-size/aiya000/dotfiles)
![GitHub language count](https://img.shields.io/github/languages/count/aiya000/dotfiles)

## 🚀 Setup

This repository is primarily for personal use. To use these dotfiles:

1. Clone this repository to `~/.dotfiles`:

```shell-session
$ git clone https://github.com/aiya000/dotfiles ~/.dotfiles
$ cd ~/.dotfiles
```

2. Create symbolic links, copy, or recursively copy files as appropriate for each configuration:
   - Use `ln -s` for files that should be linked (most dotfiles)
   - Use `cp` for files that need to be copied (e.g., templates)
   - Use `cp -r` for directories that need to be recursively copied

Example:
```shell-session
$ ln -s ~/.dotfiles/.vimrc ~/.vimrc
$ ln -s ~/.dotfiles/.zshrc ~/.zshrc
$ cp -r ~/.dotfiles/.config/nvim ~/.config/
```

## 📦 Included Configurations

### [Neovim](.config/nvim/)

- **Location**: `.config/nvim/`
- **Main file**: `init.lua`
- **Features**:
    - Full Lua configuration
    - Plugin management with lazy.nvim
    - Custom keymappings and autocommands
    - LSP, completion, and snippet configurations

### [Vim](.vim/)

- **Location**: `.vim/`, `.vimrc`, `.gvimrc`
- **Main file**: `.vimrc`
- **Features**:
    - Plugin management with dein.vim
    - Custom keymappings
    - Filetype-specific configurations

### [tmux](.tmux/)

- **Location**: `.tmux/`, `.tmux.conf`
- **Main file**: `.tmux.conf`
- **Features**:
    - Custom key bindings
    - Plugin configurations

### [Claude Code](.claude_global/)

- **Location**: `.claude_global/`
- **Main files**: `.claude_global/CLAUDE.md` (symlink to `AGENTS.global.md`), `.claude_global/settings.json`
- **Features**:
    - Under files and directories of this directory, intended to be linked to `~/.claude/*`
    - Custom instructions for Claude Code AI assistant
    - Coding style, character settings, and project-specific rules
    - Global and project-specific configurations
- **Note**:
    - For `.claude_global/` setup instructions, please refer to [.claude_global/README.md](.claude_global/README.md)
    - `.claude/` is just configuration for this repository (not for global use)

### [AGENTS.global.md](AGENTS.global.md)

- **Location**: `AGENTS.global.md`
- **Main files**: `AGENTS.global.md`
- **Features**:
    - Intended to be linked to `~/AGENTS.md`
    - Custom instructions for multiple AI assistants including **copilot-cli**
- **Note**: For `AGENTS.global.md` setup instructions, please refer to [AGENTS.global.README.md](AGENTS.global.README.md)

### [copilot-cli](AGENTS.global.md)

- **Location**: `AGENTS.global.md`

### [gemini-cli](GEMINI.global.md)

- **Location**: `GEMINI.global.md` (symlink to `AGENTS.global.md`), 
- **Note**:
    - For `GEMINI.global.md` setup instructions, please refer to [GEMINI.global.README.md](GEMINI.global.README.md) (symlink to `AGENTS.global.README.md`)

### [Shell (zsh/bash)](.sh_generic/)

- **Location**: `.sh_generic/`
- **Main file**: `.sh_generic/aliases.sh`
- **Features**:
    - Various aliases, functions, and configurations

### [Zsh](.zsh/)

- **Location**: `.zsh/`, `.zshrc`, `.zshenv`, `.zprofile`
- **Main file**: `.zshrc`
- **Features**:
    - Custom prompt configuration
    - Plugin management
    - Custom keymappings (`.zshrc.keymap`)
    - Completion configurations

## 🛠️ Other Tools

This repository also includes configurations for:

- **Git**: `.gitconfig`, `.gitignore_global`, `.gitmessage`
- **ctags**: `.ctags.d/`
- **Docker**: `.docker/`, `docker/`
- **Stack (Haskell)**: `.stack/`
- **Custom scripts**: `bin/`, `shell-session-toys/`

## 📝 License

This project is licensed under the terms specified in the [LICENSE](./LICENSE) file.

## 🔗 Links

- [GitHub Repository](https://github.com/aiya000/dotfiles)
