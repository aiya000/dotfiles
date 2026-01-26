# dotfiles

aiya000の個人用dotfilesリポジトリです。Neovim、Vim、tmux、zsh、その他多数のツールの設定ファイルを管理しているのです♪

## 📊 リポジトリ統計

- 358個のLuaファイル（Neovim設定）
- 215個のVimscriptファイル（Vim設定）
- 長年にわたり改善を重ねてきた設定ファイル群

## 🚀 セットアップ方法

このdotfilesをインストールするには、以下のコマンドを実行してください：

```bash
$ git clone https://github.com/aiya000/dotfiles ~/.dotfiles
$ cd ~/.dotfiles
$ bin/dot-link.sh
```

`dot-link.sh`スクリプトは、ホームディレクトリ以下の既存のdotfilesを削除し、このリポジトリのファイルへのシンボリックリンクを作成します。

⚠️ **警告**: このスクリプトは既存のdotfilesを**強制的に上書き**します。実行前にバックアップを取ることをお勧めします。

## 📦 含まれる設定

### Neovim

- **場所**: `.config/nvim/`
- **主要ファイル**: `init.lua`
- **特徴**:
    - Lua設定への完全移行
    - 豊富なプラグイン設定（lazy.nvim使用）
    - カスタムキーマッピングと自動コマンド
    - LSP、補完、スニペット設定

### Vim

- **場所**: `.vim/`, `.vimrc`, `.gvimrc`
- **主要ファイル**: `.vimrc`
- **特徴**:
    - プラグイン管理（dein.vim使用）
    - カスタムキーマッピング
    - ファイルタイプ別設定

### tmux

- **場所**: `.tmux/`, `.tmux.conf`
- **主要ファイル**: `.tmux.conf`
- **特徴**:
    - カスタムキーバインディング
    - プラグイン設定

### Claude Code

- **場所**: `.claude/`, `.claude_global/`
- **主要ファイル**: `CLAUDE.md`, `.claude_global/settings.json`
- **特徴**:
    - AIアシスタント用のカスタム指示
    - グローバル設定とプロジェクト固有設定

### Shell (sh/bash)

- **場所**: `.sh_generic/`, `.bashrc`, `.bash_profile`
- **主要ファイル**: `.bashrc`
- **特徴**:
    - 汎用シェル設定
    - エイリアスと環境変数

### Zsh

- **場所**: `.zsh/`, `.zshrc`, `.zshenv`, `.zprofile`
- **主要ファイル**: `.zshrc`
- **特徴**:
    - カスタムプロンプト設定
    - プラグイン管理
    - キーマッピング（`.zshrc.keymap`）
    - 補完設定

## 🛠️ その他のツール

このリポジトリには、以下のような他のツールの設定も含まれています：

- **Git**: `.gitconfig`, `.gitignore_global`, `.gitmessage`
- **ctags**: `.ctags.d/`
- **Docker**: `.docker/`, `docker/`
- **Stack (Haskell)**: `.stack/`
- **カスタムスクリプト**: `bin/`, `bash-toys/`

## 📝 ライセンス

このプロジェクトはLICENSEファイルに基づいてライセンスされています。

## 🔗 リンク

- [GitHub リポジトリ](https://github.com/aiya000/dotfiles)
