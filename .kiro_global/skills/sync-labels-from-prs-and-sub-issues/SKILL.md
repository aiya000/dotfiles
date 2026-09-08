---
name: sync-labels-from-prs-and-sub-issues
description: Issueに紐づくPRとSub Issuesの変更ファイルパスから領域ラベルを算出し、承認後にPR・Sub Issues・親Issueのラベルを付与または除去して同期する。ユーザーが関連PRからIssueラベルを集約する、領域ラベルを同期する、ラベル根拠を再計算するよう依頼したときに使用する。
compatibility: Requires GitHub CLI (`gh`), authenticated GitHub access, network access, GraphQL API access, and permission to edit pull request and issue labels in the target repository.
---

# PRとSub Issuesから領域ラベルを同期

## 目的

Issueに紐づくPRとSub Issuesの変更ファイルパスから領域ラベルを算出し、対象PR、Sub Issues、親Issueへ同期する。根拠がなくなった領域ラベルは除去する。

算出元は常にPRの変更ファイルパスとし、Issue本文から領域を推測しない。ラベルの書き込み前に差分を提示し、必ずユーザーの明示的な承認を得る。

## 入力

- Issue番号またはPR番号。`#2685` と `2685` の両形式を受け付ける
- 省略時は現在のブランチに紐づくPRを対象とし、その親Issueを辿る
- 対象リポジトリ。省略時は現在のリポジトリを使う

## 管理対象の領域ラベル

| ラベル                        | 変更パス条件                                                                                                                                                                                    |
| ----------------------------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| `native-app`                  | `frontend/ios/**`、`frontend/android/**`、`*.swift`、`*.kt`、`*.java`、`*.plist`、`*.pbxproj`、`*.gradle`、`frontend/capacitor.config.ts`、`frontend/resources/**`                              |
| `frontend`                    | `frontend/**`。ただし `frontend/ios/**`、`frontend/android/**`、`frontend/k8s/**` を除く                                                                                                        |
| `edge-node`                   | `backend/edge_node/**`                                                                                                                                                                          |
| `account-node`                | `backend/account_node/**`                                                                                                                                                                       |
| `edge-node` と `account-node` | `backend/common/**`、`backend/config/**`、`backend/main.py`、`backend/migrate.py`、`backend/Dockerfile*`、`backend/pyproject.toml`、`backend/uv.lock`、`backend/scripts/**`、`backend/tests/**` |
| `k8s`                         | `frontend/k8s/**`、`backend/*/k8s/**`、`**/kustomization.yaml`、`**/values.yaml`                                                                                                                |
| `monthly-review`              | `knowledge/devin-workflow/reports/**`。`developer-experience` も併せて付与                                                                                                                      |
| `developer-experience`        | `.devin/**`、`.agents/**`、`**/AGENTS.md`、`knowledge/**`                                                                                                                                       |
| `other-implementation`        | `.github/**`                                                                                                                                                                                    |
| `documentation`               | `docs/**`、ルートおよび各ディレクトリの `README.md`                                                                                                                                             |

パスは上から順に評価し、マッチしたラベルをすべて付与する。`native-app` と `frontend` は排他ではない。

## 手順

### 1. 対象を特定する

1. 対象リポジトリと `gh auth status` を確認する
2. 番号が省略された場合は、`gh pr view --json number -q .number` で現在のブランチに紐づくPRを取得する
3. 指定番号がIssueかPRかを `gh api repos/{owner}/{repo}/issues/<番号> --jq '.pull_request != null'` で判定する
4. PR起点の場合は、PR本文のclosing keywordまたは `Issue: #N` から親Issueを特定する
5. 複数の親Issue候補がある場合は、どれを親として扱うかユーザーへ確認する
6. 親Issueが見つからない場合はPR単体を対象にして続行し、最終報告で「親Issue未リンク」と明記する

### 2. 関連PRとSub Issuesを収集する

親Issueと各Sub Issueについて、GitHub GraphQL APIから次を取得する。

- `closedByPullRequestsReferences`
- `subIssues`
- `timelineItems` の `CROSS_REFERENCED_EVENT` に含まれるPR

元のIssueからSub Issuesを2階層まで再帰的に辿る。同じIssueやPRを重複処理しない。

各connectionでは `pageInfo { hasNextPage endCursor }` を取得し、続きがある限りpaginationする。固定件数の先頭ページだけで全件と判断しない。権限やAPI制約で全ページを取得できない場合は、ラベル差分を適用せず不完全な対象範囲をユーザーへ報告する。

収集したPRのうち、`OPEN` と `MERGED` だけをラベル算出の根拠に使う。マージされず `CLOSED` になったPRは除外する。

### 3. 変更ファイルからラベルを算出する

1. 各PRについて、`gh api repos/{owner}/{repo}/pulls/<番号>/files --paginate --jq '.[].filename'` で全変更ファイルを取得する
2. 「管理対象の領域ラベル」のマッピングを各パスへ適用する
3. 各PRのラベルは、そのPR単体の変更パスから算出した集合とする
4. 各Sub Issueのラベルは、そのSub Issueに直接紐づく有効なPRと、配下のSub Issuesから算出した和集合とする
5. 親Issueのラベルは、自身に紐づく有効なPRと、全Sub IssuesおよびそのPRから算出した和集合とする
6. ラベルごとに根拠となったPR番号と変更パスを記録する

### 4. 現在値とラベルの存在を確認する

1. `gh label list --limit 1000` などでリポジトリに存在するラベルを取得する
2. 算出された領域ラベルが存在しない場合は、ラベルを作成せずユーザーへ報告する
3. IssueとPRの現在のラベルをそれぞれ再取得する
4. 差分計算は管理対象の領域ラベルだけに限定する
5. `bug`、`enhancement`、`chat`、`release`、`devin`、`preview`、`type-*`、その他の手動運用ラベルは差分から除外し、完全に保持する

### 5. 差分を提示して承認を得る

対象ごとに、追加と除去を表で提示する。

```text
| 対象 | 追加 | 除去 |
|---|---|---|
| Issue #2685 | native-app | - |
| PR #2690 | native-app | frontend |
```

併せて次を示す。

- 親Issue、Sub Issues、PRの関連関係
- 各ラベルの根拠となったPR番号と変更パス
- 除外した `CLOSED` PR
- 親Issue未リンクや取得不完全などの警告
- `.github/labeler.yml` または正規のラベル同期ルールとの不一致

ユーザーの明示的な承認を待つ。承認前にラベルを変更しない。

### 6. 承認された差分を適用する

1. Issueには `gh issue edit <番号> --add-label <labels> --remove-label <labels>` を使用する
2. PRには `gh pr edit <番号> --add-label <labels> --remove-label <labels>` を使用する
3. 追加または除去が空なら、該当オプションを指定しない
4. 承認後に対象の関連関係や変更ファイルが変化していた場合は適用を止め、再計算して差分の再承認を得る
5. 存在しないラベルを新規作成しない

### 7. 適用結果を検証する

1. 各IssueとPRのラベルをGitHubから再取得する
2. 管理対象ラベルが算出結果と完全に一致することを確認する
3. 手動運用ラベルが変更されていないことを確認する
4. 不一致があれば勝手に追加操作を繰り返さず、期待値、実際値、失敗した操作を報告する

## 禁止事項

- リポジトリに存在しないラベルの新規作成
- 手動運用ラベルの付与または除去
- Issue本文だけを根拠にした領域ラベルの推測
- マージされずクローズされたPRを根拠に含めること
- `org-node`、`directory-node`、`flutter` の付与
- ユーザー承認前のラベル変更
- `.github/labeler.yml` の自動修正。食い違いは報告し、修正は別タスクとして扱う

## 完了報告

対象Issue、Sub Issues、PRごとに次を表で報告する。

- 最終的な領域ラベル
- 追加したラベル
- 除去したラベル
- 根拠となったPR番号と変更パス
- 手動運用ラベルが保持されたこと
- 再取得による検証結果

`.github/labeler.yml` や正規ルールとの差異があれば、変更せずに差異を明記する。
