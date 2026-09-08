---
name: apply-pr-review
description: PRレビューの全指摘を確認し、妥当な指摘を修正・検証・コミットして各GitHub Conversationへ返信する。ユーザーがPRレビューへの対応、レビュー指摘の一括修正、Conversationへの対応報告を依頼したときに使用する。
compatibility: Requires Git, GitHub CLI (`gh`), authenticated GitHub access, network access, and permission to push to the pull request branch and reply to review conversations.
---

# PRレビュー指摘の適用

## 目的

対象PRのレビュー指摘を漏れなく評価し、妥当な指摘を修正して、指摘ごとのConversationへ対応内容を日本語で返信する。

## 対象PRの決定

1. ユーザーがPR番号またはURLを指定した場合は、そのPRを対象にする
2. 指定がない場合は、現在のブランチに紐づくPRと会話の文脈から対象を特定する
3. 対象を特定できない場合は、PR番号またはURLをユーザーに確認し、回答があるまで作業を止める
4. 推測はできるが不明瞭な場合は、作業前に対象PRのURLを示して宣言する
5. ユーザーが明示的に除外したレビューだけを対象外とし、それ以外のレビューをすべて対象にする

## 手順

### 1. 前提とPR情報を確認する

1. Gitリポジトリ、現在のブランチ、remote、Git identityを確認する
2. `gh auth status` でGitHub認証を確認する
3. `gh pr view <PR> --json number,title,body,url,headRefName,baseRefName,reviewDecision,reviews,comments` でPR情報を取得する
4. GitHub GraphQL APIを使い、inline review threadを含む全Conversation、コメント、返信、resolve状態、対象ファイル、行番号、node IDを取得する
5. `gh pr view --comments` だけでinline review threadを取得できたと判断しない
6. 対象ブランチが現在のブランチと異なる場合は、勝手に別ブランチへ変更せずユーザーへ確認する

### 2. 全指摘を分類する

各指摘について、関連コードとリポジトリ規約を読み、次のいずれかに分類する。

- 対応する: 技術的に妥当で、修正を入れてよい
- 対応しない: 誤認、要件違反、セキュリティ低下、破壊的変更など、入れてはいけない要因がある
- 確認待ち: 要件やレビュワーの意図が不明で、安全に判断できない
- 対応済み: 現在のPR内容ですでに解消されている

同じ原因に見える指摘でも、Conversationごとに状態と返信先を管理する。

### 3. 妥当な指摘を修正する

1. 指摘ごとに単一目的の修正を行う
2. 変更対象に適用される `AGENTS.md` とプロジェクト規約を守る
3. 指摘に必要な範囲を超える無関係な変更を混ぜない
4. 変更後、対象テスト、lint、型チェック、ビルドなど最も関連する検証を実行する
5. 検証が失敗した場合は原因を修正し、成功するまで再検証する。実行できない検証は理由を記録する
6. 指摘ごとに変更ファイルだけをstageし、単一目的の新規コミットを作る
7. `--no-verify` を使用しない。hook失敗後は修正を再stageし、`--amend` ではなく新しいコミットを作る
8. 複数のConversationを1コミットへまとめない。ただし同一原因を同時に直さなければコードが成立せず、各Conversationへの説明も明確にできる場合は例外とする
9. PRのheadブランチへ通常のpushを行う。force push、履歴改変、main/master/developへの直接pushはしない

### 4. 入れてはいけない指摘を保留する

指摘を入れてはいけない要因がある場合は、コードを変更しない。

1. Kiroの現在の会話で、対象指摘と対応できない技術的理由をユーザーへ報告する
2. 該当Conversationにも、対応しない理由と必要な判断を日本語で返信する
3. その指摘はユーザーが次のアクションを命じるまで保留する
4. 他の独立した指摘は、安全に続行できる場合に限り処理を続ける

### 5. Conversationへ返信する

修正コミットをpushした後、対応した各Conversationへ直接返信する。PR全体のコメントで代用しない。

返信には次を含める。

- 対応結果
- 修正内容の短い説明
- 修正ファイル名と行番号
- コミットSHA
- コミットURL
- 実行した検証と結果

対応済みまたは対応不要と判断したConversationにも、その根拠を返信する。API呼び出し後はConversationを再取得し、返信が正しいthreadへ投稿されたことを確認する。

### 6. Resolveを制御する

- Conversationは既定ではResolveしない
- ユーザーが事前または事後に明示的にResolveも依頼した場合だけ、対象threadをResolveする
- Resolve後はthreadの状態を再取得して確認する

## 完了報告

対象Conversationごとに、次を表で報告する。

- Conversationまたは指摘の識別情報
- 対応済み、対応しない、確認待ち、対応済みだった、のいずれか
- コミットSHAとURL
- 検証結果
- Conversationへの返信結果
- Resolve状態

未対応や未検証があれば、理由とユーザーに必要な次の判断を明記する。
