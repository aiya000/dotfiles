#!/usr/bin/env bash
# mac-restore.sh - mac-backup.sh で作成したアーカイブを新しい Mac に展開する
# mac-restore.sh mac-backup-20260325_XXXXXX.tar.xz
set -euo pipefail

ARCHIVE="${1:-}"

echo "╔══════════════════════════════════════════════╗"
echo "║           Mac Restore Script                 ║"
echo "╚══════════════════════════════════════════════╝"
echo ""

if [ -z "$ARCHIVE" ]; then
  echo "  使い方: mac-restore.sh <backup.tar.xz>"
  exit 1
fi

if [ ! -f "$ARCHIVE" ]; then
  echo "  ❌ ファイルが見つかりません: $ARCHIVE"
  exit 1
fi

echo "  アーカイブ : $ARCHIVE"
echo "  展開先     : $HOME"
echo ""
echo "  【展開前に以下のアプリを終了してください】"
echo "    Chrome / Slack / Outlook / iTerm2 / Raycast / DBeaver"
echo ""
echo "  ⚠️  既存ファイルは上書きされます"
read -r -p "  準備ができたら Enter を押してください（Ctrl+C で中止）: "

HOME_PARENT=$(dirname "$HOME")

echo ""
echo "  ==> 展開中... (しばらくかかります)"
tar -xJf "$ARCHIVE" \
  -C "$HOME_PARENT" \
  2>&1 | grep -v "^tar: Removing" || true

echo "  ==> パーミッション修正中..."

# SSH
if [ -d "$HOME/.ssh" ]; then
  chmod 700 "$HOME/.ssh"
  find "$HOME/.ssh" -type f | while read -r f; do
    case "$f" in
      *.pub) chmod 644 "$f" ;;
      *)     chmod 600 "$f" ;;
    esac
  done
  echo "     ✓ ~/.ssh"
fi

# GPG
if [ -d "$HOME/.gnupg" ]; then
  chmod 700 "$HOME/.gnupg"
  find "$HOME/.gnupg" -type f -exec chmod 600 {} \;
  echo "     ✓ ~/.gnupg"
fi

echo ""
echo "  ==> macOS 設定を再読み込み中..."
# cfprefsd / Dock / Finder を再起動して設定を反映
pkill -x cfprefsd 2>/dev/null || true
pkill -x Dock     2>/dev/null || true
pkill -x Finder   2>/dev/null || true
echo "     ✓ Dock / Finder / cfprefsd を再起動しました"

echo ""
echo "  ✅ 展開完了！"
echo ""
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo "  📋 リストア後のチェックリスト"
echo "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━"
echo ""
echo "  【必須】"
echo "  □ dotfiles のセットアップ実行 (例: cd ~/.dotfiles && make)"
echo "  □ Homebrew の再インストール & brew bundle (アプリは別途必要)"
echo "  □ Xcode を起動してライセンスに同意"
echo "  □ SSH: ssh-add ~/.ssh/id_ed25519 (or id_rsa)"
echo ""
echo "  【アプリ再ログインが必要】"
echo "  □ Slack        - ワークスペースへの再ログイン"
echo "  □ Microsoft Outlook - Microsoft 365 アカウント再認証"
echo "  □ Chrome       - Google アカウントへのログイン (タブは復元されるはず)"
echo ""
echo "  【確認推奨】"
echo "  □ Raycast      - 設定が復元されているか確認"
echo "  □ iTerm2       - プロファイルが復元されているか確認"
echo "  □ DBeaver      - 接続設定が復元されているか確認"
echo "  □ OpenDeck     - デッキ設定が復元されているか確認"
echo "  □ Google 日本語入力 - ユーザー辞書が復元されているか確認"
echo "  □ WorkSpaces   - プロジェクト設定が復元されているか確認"
echo ""
echo "  【macOS 設定】"
echo "  □ Dock レイアウト・Finder 設定は再起動後に反映されます"
echo "  □ Wi-Fi パスワード・Touch ID は手動で再設定が必要です"
echo "  □ システム設定 > プライバシーとセキュリティ で各アプリの権限を再付与"
echo ""
