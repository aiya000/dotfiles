#!/usr/bin/env bash
# WSL Home Directory Backup Script
# Creates a tar.gz archive of the home directory while preserving symlinks

# このスクリプトは：
# - シンボリックリンクをそのまま保存するのです（-hオプションを使わないので、リンク先を辿らないのです）
# - .cacheやnode_modulesなどの不要なディレクトリを除外するのです
# - タイムスタンプ付きのファイル名で/tmpに保存するのです
#
# 使い方は：
# ~/backup-home.sh
#
# って実行するだけなのです♪
#
# もし除外したいディレクトリを追加したい場合は、--exclude行を増やせばいいのです！
# あと、バックアップ先を変えたい場合は、BACKUP_DIR="/tmp"の部分を変更してほしいのです！

set -euo pipefail

BACKUP_NAME="wsl-home-backup-$(date +%Y%m%d-%H%M%S).tar.gz"  # Use .tar.xz for better compression (but backing up will be slower)
BACKUP_DIR="$HOME/tmp"
if [[ ! -d $BACKUP_DIR ]] ; then
  mkdir "$BACKUP_DIR"
fi
BACKUP_PATH="$BACKUP_DIR/$BACKUP_NAME"

echo "Starting backup of home directory..."
echo "Backup will be saved to: $BACKUP_PATH"
cd ~

# Create backup
# - Preserves symlinks (doesn't follow them)
# - Excludes common cache/temp directories
# - Excludes existing backup files
tar --exclude="./$(basename "$BACKUP_DIR")" \
    --exclude='.backup' \
    --exclude='.local/share/Trash' \
    --exclude='.cache' \
    --exclude='.npm' \
    --exclude='.yarn' \
    --exclude='.mozilla/firefox/*/cache2' \
    --exclude='.rustup' \
    --exclude='.nvm' \
    --exclude='.gradle' \
    --exclude='Android' \
    --exclude='**/node_modules' \
    -czvf "$BACKUP_PATH" .

echo "Backup completed successfully!"
echo "Backup size: $(du -h "$BACKUP_PATH" | cut -f1)"
echo "Location: $BACKUP_PATH"
