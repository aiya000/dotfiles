# dotfiles

Personal dotfiles collection for various development tools and environments.

## 📊 Repository Insights

![GitHub commit activity](https://img.shields.io/github/commit-activity/y/aiya000/dotfiles)
![GitHub last commit](https://img.shields.io/github/last-commit/aiya000/dotfiles)
![GitHub repo size](https://img.shields.io/github/repo-size/aiya000/dotfiles)
![GitHub language count](https://img.shields.io/github/languages/count/aiya000/dotfiles)
![Top language](https://img.shields.io/github/languages/top/aiya000/dotfiles)

## 🚀 Setup

This repository is primarily for personal use. To use these dotfiles:

1. Clone this repository to `~/.dotfiles`:
   ```bash
   git clone https://github.com/aiya000/dotfiles ~/.dotfiles
   cd ~/.dotfiles
   ```

2. Create symbolic links, copy, or recursively copy files as appropriate for each configuration:
   - Use `ln -s` for files that should be linked (most dotfiles)
   - Use `cp` for files that need to be copied (e.g., templates)
   - Use `cp -r` for directories that need to be recursively copied

   Example:
   ```bash
   ln -s ~/.dotfiles/.vimrc ~/.vimrc
   ln -s ~/.dotfiles/.zshrc ~/.zshrc
   cp -r ~/.dotfiles/.config/nvim ~/.config/
   ```

Alternatively, you can use the `bin/dot-link.sh` script, though it is designed for the author's personal workflow and may not suit all use cases.

## 📦 Included Configurations

### [Neovim](.config/nvim/)

- **Location**: `.config/nvim/`
- **Main file**: `init.lua`
- **Features**:
    - Full Lua configuration
    - Plugin management with lazy.nvim
    - Custom keymappings and autocommands
    - LSP, completion, and snippet configurations
- **Note**: For project-specific instructions, see [AGENTS.md](.config/nvim/AGENTS.md)

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

### [Claude Code](.claude/)

- **Location**: `.claude/`, `.claude_global/`
- **Main files**: `CLAUDE.md`, `.claude_global/settings.json`
- **Features**:
    - Custom instructions for Claude Code AI assistant
    - Coding style, character settings, and project-specific rules
    - Global and project-specific configurations
- **Note**: For `.claude_global` setup instructions, please refer to [.claude_global/README.md](.claude_global/README.md)

### [gemini-cli](GEMINI.md)

- **Location**: `GEMINI.md`
- **Main file**: `GEMINI.md`
- **Features**:
    - Custom instructions for gemini-cli AI assistant
    - Global configuration shared across projects
    - Character settings and conversation rules
    - Memory file management for session tracking

### [copilot-cli](.copilot/)

- **Location**: `.copilot/`
- **Main file**: `copilot-instructions.md` (symlink to `ai-custom-instructions.md`)
- **Features**:
    - Custom instructions for GitHub Copilot CLI
    - Shared configuration with other AI assistants
    - Notification settings and workflow customizations
- **Note**: For setup instructions, see [.copilot/README.md](.copilot/README.md)

### [Shell (sh/bash)](.sh_generic/)

- **Location**: `.sh_generic/`, `.bashrc`, `.bash_profile`
- **Main file**: `.bashrc`
- **Features**:
    - Generic shell configurations
    - Aliases and environment variables

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
- **Custom scripts**: `bin/`, `bash-toys/`

## 📝 License

This project is licensed under the terms specified in the LICENSE file.

## 🔗 Links

- [GitHub Repository](https://github.com/aiya000/dotfiles)
