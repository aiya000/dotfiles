#!/usr/bin/env bash
# mac-backup.sh - ホームディレクトリをクラウドアップロード用にアーカイブする
# 旧 Mac（バックアップ）
# mac-backup.sh
# → ~/mac-backup-20260325_XXXXXX.tar.xz

set -euo pipefail

TIMESTAMP=$(date +%Y%m%d_%H%M%S)
OUTPUT="${HOME}/mac-backup-${TIMESTAMP}.tar.xz"
HOME_PARENT=$(dirname "$HOME")
HOME_NAME=$(basename "$HOME")

echo "╔══════════════════════════════════════════════╗"
echo "║           Mac Backup Script                  ║"
echo "╚══════════════════════════════════════════════╝"
echo ""
echo "  出力先 : $OUTPUT"
echo ""
echo "  【除外されるディレクトリ（再生成可能な大容量データ）】"
echo "    ~/Library/Caches/            (Homebrew キャッシュ等)"
echo "    ~/Library/Developer/Xcode/DerivedData/"
echo "    ~/Library/Developer/CoreSimulator/"
echo "    ~/Library/Developer/Xcode/iOS DeviceSupport/"
echo "    ~/Library/Developer/Xcode/watchOS DeviceSupport/"
echo "    ~/Library/Developer/Xcode/tvOS DeviceSupport/"
echo "    ~/.cache/"
echo "    ~/.Trash/"
echo "    ~/mac-backup-*.tar.xz        (過去のバックアップ自身)"
echo "    */node_modules/"
echo "    */.git/objects/   (git履歴は除外、インデックスは保持)"
echo ""
echo "  【バックアップ前に以下のアプリを終了してください】"
echo "    Chrome / Slack / Outlook / iTerm2 / Raycast / DBeaver"
echo ""

read -r -p "  準備ができたら Enter を押してください（Ctrl+C で中止）: "

# アプリが起動中なら警告
APPS=(
  "Google Chrome"
  "Slack"
  "Microsoft Outlook"
  "iTerm2"
  "Raycast"
  "DBeaver"
  "OpenDeck"
)
RUNNING=()
for app in "${APPS[@]}"; do
  if pgrep -xq "$app" 2>/dev/null || osascript -e "tell application \"System Events\" to (name of processes) contains \"$app\"" 2>/dev/null | grep -q true; then
    RUNNING+=("$app")
  fi
done

if [ ${#RUNNING[@]} -gt 0 ]; then
  echo ""
  echo "  ⚠️  以下のアプリが起動中です:"
  for app in "${RUNNING[@]}"; do
    echo "    - $app"
  done
  echo ""
  read -r -p "  このまま続けますか？ [y/N]: " confirm
  [[ "${confirm:-N}" =~ ^[Yy]$ ]] || { echo "中止しました"; exit 1; }
fi

echo ""
echo "  ==> アーカイブ作成中... (ファイル数によっては数十分かかります)"
echo ""

tar -cJf "$OUTPUT" \
  --exclude="${HOME}/Library/Caches" \
  --exclude="${HOME}/Library/Developer/Xcode/DerivedData" \
  --exclude="${HOME}/Library/Developer/CoreSimulator" \
  --exclude="${HOME}/Library/Developer/Xcode/iOS DeviceSupport" \
  --exclude="${HOME}/Library/Developer/Xcode/watchOS DeviceSupport" \
  --exclude="${HOME}/Library/Developer/Xcode/tvOS DeviceSupport" \
  --exclude="${HOME}/.cache" \
  --exclude="${HOME}/.Trash" \
  --exclude="${HOME}/mac-backup-*.tar.xz" \
  --exclude="*/node_modules" \
  --exclude="*/.git/objects" \
  -C "$HOME_PARENT" \
  "$HOME_NAME" \
  2>&1 | grep -v "^tar: Removing" || true

echo ""
SIZE=$(du -sh "$OUTPUT" | cut -f1)
echo "  ✅ 完了！"
echo "     ファイル : $OUTPUT"
echo "     サイズ   : $SIZE"
echo ""
echo "  クラウドストレージにアップロードしてください"
