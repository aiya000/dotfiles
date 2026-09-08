---
name: sync-issue-title-and-detail-to-this-pr
description: PRのタイトルと概要を親Issueへ同期し、親Issueが未リンクなら承認後に新規作成してPRへリンクする。ユーザーがPRとIssueのタイトル・本文を揃える、親Issueを作る、PR内容をIssueへ反映するよう依頼したときに使用する。
compatibility: Requires GitHub CLI (`gh`), authenticated GitHub access, network access, and permission to edit pull requests and create or edit issues in the target repository.
---

# PRのタイトルと概要を親Issueへ同期

## 目的

PRを正として、親Issueのタイトルと概要を同期する。

- 親Issueが未リンクの場合は、PRのタイトルと概要からIssueを新規作成し、PRへリンクする
- 親Issueが存在する場合は、Issueのタイトルと本文だけをPRに合わせて更新する

GitHubへの書き込み前に、必ず同期内容をユーザーへ提示して明示的な承認を得る。

## 入力

- PR番号またはURL。省略時は現在のブランチに紐づくPRを使う
- 対象リポジトリ。省略時は現在のリポジトリを使う

## 手順

### 1. PR情報を取得する

1. 対象リポジトリと `gh auth status` を確認する
2. PR番号が指定されていない場合は、`gh pr view --json number -q .number` で現在のブランチに紐づくPR番号を取得する
3. `gh pr view <PR> --json number,title,body,url` でPRのタイトル、本文、URLを取得する
4. PRを特定できない場合は、ユーザーにPR番号またはURLを確認する

### 2. 親Issueを特定する

PR本文から、次のパターンを大文字小文字を区別せず検索する。

- `Close #N`、`Closes #N`、`Closed #N`
- `Fix #N`、`Fixes #N`、`Fixed #N`
- `Resolve #N`、`Resolves #N`、`Resolved #N`
- `Issue: #N`、`Issue: Closes #N`、`- Issue: #N`

1. 複数のIssue番号が見つかった場合は、更新対象をユーザーへ確認する
2. 1件見つかった場合は、`gh issue view <番号> --json number,title,body,url,labels,assignees` で現在のIssueを取得する
3. 見つからない場合は「親Issueを新規作成する」へ進む

### 3. 親Issueを新規作成する

1. `.github/ISSUE_TEMPLATE/default.md` と適用される `AGENTS.md` を読む
2. 次の内容を組み立てる
   - Issueタイトル: PRタイトルをそのまま使用
   - 「何に貢献するか」: PR本文から該当項目を判断。判断できない場合はユーザーへ確認
   - 「本文」: PR本文の `## 変更の目的と背景` セクション。存在しない場合はPR本文冒頭の概要
   - 現行規約で必須の手動動作確認手順などがあれば、その形式も満たす
3. 新しいIssueのタイトルと本文をユーザーへ全文提示する
4. PR本文へ追加する `Closes #<新Issue番号>` の位置と、既存本文を保持することを説明する
5. ユーザーの明示的な承認を待つ。承認前にIssue作成やPR更新をしない
6. 承認後、Markdown本文を `--body-file` で渡して `gh issue create` を実行する
7. 新Issue番号を取得し、PR本文の `### 関連するIssue・タスク` に `Closes #<新Issue番号>` を追記する
8. セクションが存在しない場合は、既存本文を削除せず適切な位置に関連Issueセクションを追加する
9. PR本文も `--body-file` で安全に渡し、`gh pr edit <PR>` で更新する
10. IssueとPRを再取得し、作成内容とリンクを検証する
11. 新規作成フローを完了し、既存Issueの更新フローは実行しない

### 4. 既存Issueへの同期内容を組み立てる

1. 新タイトルはPRタイトルをそのまま使用する
2. PR本文から、次の優先順で概要を抽出する
   1. `## 変更の目的と背景` セクションの内容
   2. PR本文冒頭から最初の `##` 見出しまでのテキスト
   3. どちらもない場合は、PR本文全体の先頭200文字から作る事実に基づく要約
3. 既存Issue本文に「何に貢献するか」などのチェックリストがある場合は、その内容とチェック状態を保持する
4. チェックリスト以降の本文部分をPR概要で置き換える
5. チェックリストがない場合はPR概要だけで本文を構成する
6. PR本文全体をそのままIssueへコピーしない

### 5. 差分を提示して承認を得る

次をユーザーへ提示する。

- 対象PRとIssueのURL
- タイトル: `[旧]` から `[新]` への差分
- 組み立てた新しいIssue本文の全文
- 変更しないラベル、アサイニー、その他の属性

ユーザーの明示的な承認を待つ。承認前にGitHubを更新しない。ユーザーが内容を修正した場合は、修正版を再提示して承認を得る。

### 6. 既存Issueを更新して検証する

1. 承認後、`gh issue edit <番号> --title <新タイトル>` でタイトルを更新する
2. Markdown本文はshell引数へ直接展開せず、`--body-file` で渡して本文を更新する
3. `gh issue view <番号> --json number,title,body,url,labels,assignees` で再取得する
4. タイトルと本文が承認内容に一致し、ラベルとアサイニーが変更されていないことを確認する

## 仕様

- Issueのタイトルと本文だけを更新する
- ラベル、アサイニー、milestone、projectなどは変更しない
- 新規作成、既存Issue更新、PR本文更新の前に必ずユーザー承認を得る
- PR本文全体ではなく、概要部分だけをIssue本文へ反映する
- Issue新規作成時は現行のIssueテンプレートとリポジトリ規約に従う
- 新規Issueはclosing keywordでPRへリンクする

## 完了報告

- 更新または作成したIssueの番号、タイトル、URL
- 新規作成時は更新したPRのURL
- 実際に変更した属性
- 変更しなかった属性
- 再取得による検証結果
