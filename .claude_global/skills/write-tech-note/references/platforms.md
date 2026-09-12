# プラットフォーム（Zenn / Qiita）の判定と、違いの一覧

この記事をどちらに出すかは、**カレントディレクトリから判定する**。
判定した結果が、置き場所・ファイル形式・記法・タイトル長・自己レビュー項目のすべてを決める。
一度決めたら記事の中で混ぜない（style-guide §7「プラットフォーム記法」）。

- - -

## 1. カレントディレクトリから判定する

手順:

1. `git rev-parse --show-toplevel` でカレントの git root を取る（git 管理下でなければカレントディレクトリそのもの）
2. その root を下の表の**目印**と照合する
3. どれにも当たらなければ「記事リポジトリの外」。§1-2 へ

| 目印 | プラットフォーム | 役割 |
|---|---|---|
| `articles/` と `books/` と `how-to-write-zenn-articles.md` がある（`~/Repository/zenn-old`、remote `aiya000/zenn-draft`） | Zenn | 下書き。**新しい Zenn 記事はここに書く** |
| `package.json` の依存に `zenn-cli` がある（`~/Repository/zenn-articles`、remote `aiya000/zenn-articles`） | Zenn | 公開用。自動デプロイされる。ユーザーが明示しない限り、ここには書かない |
| トピック名のディレクトリ（`Neovim/`, `TypeScript/`, `Haskell/` …）が並び、README が qiita を名乗る（`~/Repository/qiita-draft`、remote `aiya000/qiita-draft`） | Qiita | 下書き。**新しい Qiita 記事はここに書く** |

- パスではなく**目印**で判定する。ユーザーが clone し直したり、worktree の中にいることがある
- 記事リポジトリの中にいるなら、プラットフォームは決まっている。ユーザーに聞かない
- ただしユーザーが明示的に別のプラットフォームを指定したら、そちらが勝つ。そのときの出力先は相手のリポジトリで、カレントディレクトリには書かない

### 1-2. 記事リポジトリの外にいるとき

自作ツールの記事を、そのツールのリポジトリから書き始めるのが一番多い形。
このとき**カレントディレクトリは「題材」であって「プラットフォーム」ではない**。

- 題材（`git log`、README、コード、実行例）は、そのままカレントディレクトリから取る
- プラットフォームは決まらないので、`AskUserQuestion` で Zenn / Qiita を聞く。既定を勝手に決めない
- 聞く前に、SKILL.md Workflow 5（既存記事の探索）を先に済ませる。同じ題材の記事が片方にすでにあるなら、それを選択肢の説明に添える（「Qiita に luarrow の下書きがあります」）
- 決まったら、§1 の表の該当リポジトリを出力先にする

- - -

## 2. 置き場所とファイル形式

| | Zenn（zenn-old） | Qiita（qiita-draft） |
|---|---|---|
| 置き場所 | `articles/<タイトル>.md` | `<トピック>/<タイトル>.md` |
| 画像などの資産があるとき | `articles/<タイトル>/main.md` | `<トピック>/<タイトル>/main.md` |
| トピックのディレクトリ | なし | `ls` して既存のものから選ぶ。どれにも当てはまらなければ新設してよい |
| 先頭 | Zenn frontmatter。`# ` の H1 は置かない（`title` と二重になる） | `# タイトル` の H1。frontmatter は置かない |
| タイトルの長さ | **70文字まで**（全角も半角も1文字）。`printf '%s' '<title>' \| wc -m` で確認する | 制限なし |
| 文中の改行 | 文の途中の `[。！？]` のあとは行末に半角スペース2つ（ハードブレイク）。文の終わりは改行のみ（zenn-old の `CLAUDE.md`） | ハードブレイクは使わない（既存記事は1本も使っていない） |

Zenn frontmatter の形（zenn-old の `AGENTS.md` より）:

```yaml
---
title: "..."
emoji: "🐕"
type: "tech"
topics: [...]
published: false
---
```

- 古い Qiita 記事には `content.md` や、トピック直下の `.md` がある。**新しく作るときは上の形に揃える**（2025年以降の記事はすべて `<トピック>/<タイトル>/main.md`）
- タイトルはユーザーが出したものをそのまま使う。長い【】付きのタイトルは意図的なので「改善」しない。Zenn で70文字を超えたときだけ、削る案を出してユーザーに決めてもらう

- - -

## 3. 記法の対応表

| 用途 | Zenn | Qiita |
|---|---|---|
| 前提・注意・「余談ですが」・実行例の撮り方の申告 | `:::message` | `:::note info` |
| 警告 | `:::message alert` | `:::note warn` |
| 折りたたみ | `:::details タイトル` | `<details><summary>タイトル</summary><div>` … `</div></details>` |
| 絵文字 | Unicode をそのまま書く | Unicode でも、ショートコード（`:tada:` `:sparkles:` `:point_down:` `:dog2:` `:thinking:` `:sob:`）でもよい |
| 良い例・悪い例のマーク | 🙅 / 🙆、❌ / ⭕ | 上に加えて `:x:` / `:o:` |
| リンクのカード化 | URL を裸で1行に置く | 未確認。裸 URL に頼らず `[タイトル](URL)` で書く |

- 追記は記事の**最上部**に置く。Zenn は `:::details 追記: ...`、Qiita は `<details>` で同じことをする
- 水平線 `- - -`、脚注 `[^kebab-case]`、ファイル名付きコードフェンス、`shell-session` は両方で同じ（style-guide §1-4, §1-5, §2-1）
- SKILL.md 要件4 の「実行例をサンドボックスで撮った」という申告は、**そのプラットフォームのメッセージボックス**で書く（Zenn なら `:::message`、Qiita なら `:::note info`）

- - -

## 4. リポジトリ固有の指示ファイル

書き始める前に、出力先リポジトリの指示ファイルを読む。カレントディレクトリのものではない。

- **Zenn（zenn-old）**: `AGENTS.md`（70文字制限、frontmatter）、`CLAUDE.md`（ハードブレイク、`:` は半角）、`how-to-write-zenn-articles.md`
- **Qiita（qiita-draft）**: 指示ファイルはない（2026-09-12 時点）。`define-markdown-format` と、このスキルのルールだけが効く。zenn-old のハードブレイク規則を持ち込まない

- - -

## 5. 自己レビュー（プラットフォーム別）

SKILL.md の自己レビューチェックリストに加えて、判定したプラットフォームの側だけを走らせる。

**Zenn**:

- frontmatter がある: 1行目が `---` で、`title` `emoji` `type` `topics` `published` が揃っている
- H1 がない: `rg -n '^# ' <file>` が空
- タイトルが70文字以内: `printf '%s' '<title>' | wc -m`
- Qiita 記法が混ざっていない: `rg -n ':::note|<details>|:[a-z_0-9+-]+:' <file>` が空（コードブロックの中の `:` を除く）

**Qiita**:

- H1 がある: 1行目が `# タイトル`
- frontmatter がない: 1行目が `---` ではない
- Zenn 記法が混ざっていない: `rg -n ':::message|:::details' <file>` が空
- ハードブレイクが混ざっていない: `rg -n '  $' <file>` が空
