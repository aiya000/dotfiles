---
name: link-this-pr-to-issue
description: PRをIssueへclosing keywordでリンクし、GitHubのDevelopment欄（closingIssuesReferences）に相互リンクを確立する。ユーザーがPRとIssueをDevelopmentで紐づける、PRをIssueにリンクする、マージでIssueが自動クローズされるようにするよう依頼したときに使用する。
compatibility: Requires GitHub CLI (`gh`), authenticated GitHub access, network access, and permission to edit pull requests in the target repository.
---

# PRをIssueへDevelopmentでリンク

## 目的

PR本文へclosing keyword（`Closes #N` 等）を記載し、GitHubの「Development」欄にPRとIssueの相互リンクを確立する。これにより、PRがマージされたときに対象Issueが自動でクローズされる。

GitHubへの書き込み前に、必ず変更内容をユーザーへ提示して明示的な承認を得る。

## 入力

- PR番号またはURL。`#3797` と `3797` の両形式を受け付ける。省略時は現在のブランチに紐づくPRを使う
- Issue番号またはURL。省略時はPR本文から推測する（「2. 対象Issueを特定する」参照）
- 対象リポジトリ。省略時は現在のリポジトリを使う

## 手順

### 1. PRを特定する

1. 対象リポジトリと `gh auth status` を確認する
2. PR番号が指定されていない場合は、`gh pr view --json number -q .number` で現在のブランチに紐づくPR番号を取得する
3. `gh pr view <PR> --json number,title,body,url` でPRのタイトル、本文、URLを取得する
4. PRを特定できない場合は、ユーザーにPR番号またはURLを確認する

### 2. 対象Issueを特定する

Issue番号が明示されている場合はそれを使う。省略された場合は、PR本文から次のパターンを大文字小文字を区別せず検索する。

- `Close #N`、`Closes #N`、`Closed #N`
- `Fix #N`、`Fixes #N`、`Fixed #N`
- `Resolve #N`、`Resolves #N`、`Resolved #N`
- `Issue: #N`、`- Issue: #N`、`関連 Issue: #N` などの参照

1. 1件見つかった場合は、それを対象Issueの候補とする
2. 複数のIssue番号が見つかった場合は、どれをリンク対象にするかユーザーへ確認する
3. 1件も見つからない場合は、ユーザーにIssue番号を確認する
4. 対象番号がIssueであること（PRでないこと）を `gh api repos/{owner}/{repo}/issues/<番号> --jq '.pull_request == null'` で確認する

### 3. 既存のリンク状態を確認する

GraphQLでPRの `closingIssuesReferences` を取得し、対象Issueが既にリンク済みかを判定する。

```graphql
query($owner:String!,$repo:String!,$num:Int!){
  repository(owner:$owner,name:$repo){
    pullRequest(number:$num){
      closingIssuesReferences(first:50){ nodes{ number title state } }
    }
  }
}
```

- 対象Issueが既に `closingIssuesReferences` に含まれる場合は、リンク済みと報告して終了する（冪等。本文を書き換えない）
- 含まれない場合は次へ進む

### 4. PR本文の変更案を組み立てる

対象Issueに対する closing keyword 行を、PR本文へ追加または既存参照から変換する。

1. closing keyword は原則 `Closes #N` を使う。ただしIssueをクローズすべきでない場合（親Issueなど複数PRで解決する場合）は、ユーザーに意図を確認し、closing keyword ではなく通常参照（`Related to #N`）に留める判断を提示する
2. PR本文に既に対象Issueへの非closing参照（例: `- Issue: #N`）がある場合は、その行を `- Closes #N` に置換する
3. 参照行がない場合は、`### 関連するIssue・タスク` セクションへ `- Closes #N` を追記する
4. 当該セクションが存在しない場合は、既存本文を一切削除せず、本文末尾などの適切な位置に関連Issueの記述を追加する
5. PR本文の他のセクションは変更しない

### 5. 差分を提示して承認を得る

次をユーザーへ提示する。

- 対象PRとIssueの番号・タイトル・URL
- PR本文の変更箇所（追加行または置換前後の行）だけを抜粋した差分
- リンクによりPRマージ時にIssueが自動クローズされること（closing keyword を使う場合）

ユーザーの明示的な承認を待つ。承認前にPRを更新しない。ユーザーが内容を修正した場合は、修正版を再提示して承認を得る。

### 6. PR本文を更新する

1. 更新後のPR本文全文を一時ファイルへ書き出す（shell引数へ直接展開しない）
2. `gh pr edit <PR> --body-file <ファイル>` で本文を更新する
3. 既存本文の他セクションが保持されていることを前提とする（丸ごと置換しない）

### 7. リンクを検証する

1. 手順3と同じGraphQLで `closingIssuesReferences` を再取得する
2. 対象Issueが `closingIssuesReferences` に含まれることを確認する
3. 含まれない場合は、追加操作を繰り返さず、期待値・実際値・使ったkeywordをユーザーへ報告する（closing keyword が本文の正しい位置・形式で記載されているかを見直す）

## 仕様

- 変更するのはPR本文のみ。Issue本文・ラベル・アサイニー・milestone・projectは変更しない
- closing keyword の既定は `Closes #N`。Issueをクローズすべきでない場合はユーザー確認のうえ通常参照に留める
- 既にリンク済みの場合は何もしない（冪等）
- PR本文は既存セクションを保持し、関連Issue箇所だけを追加・変換する
- GitHubへの書き込み前に必ずユーザー承認を得る
- 存在しないIssueやPRへのリンクを作らない

## 禁止事項

- Issue本文・ラベル・アサイニー等、PR本文以外の変更
- ユーザー承認前のPR更新
- PR本文の他セクションの削除・改変
- Issueをクローズすべきか不明なまま closing keyword を付与すること

## 完了報告

- 対象PRとIssueの番号・タイトル・URL
- PR本文に追加または変換した closing keyword 行
- `closingIssuesReferences` の再取得による検証結果
- 既にリンク済みで変更不要だった場合はその旨
