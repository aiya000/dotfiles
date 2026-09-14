---
name: git-resolve-for-origin-develop
description: 対象ブランチを origin/develop へ rebase し、発生したコンフリクトを解消して検証まで行う。ユーザーが develop への追従、origin/develop への rebase、rebase コンフリクトの解消を依頼したときに使用する。
compatibility: Requires Git, GitHub CLI (`gh`) with authenticated GitHub access, network access to the `origin` remote, and a configured Git identity.
---

# origin/develop への rebase とコンフリクト解消

## 目的

対象ブランチを最新の `origin/develop` へ rebase し、コンフリクトを安全に解消して、検証が通る状態までローカルで仕上げる。

## 呼び出し形式

```
/git-resolve-for-origin-develop [<Issue URL または Issue 番号>]
```

- 引数は省略可能
- 引数は「どのブランチを rebase するか」を決めるためだけに使う
- 引数があっても、Issue 本文の内容をコードへ実装することはしない

## 対象ブランチの決定

次の A → B → X → C の順にフォールバックする。
先の手段で対象が一意に決まった時点で、以降の手段は試さない。
決定後は、必ず対象ブランチと決定根拠（A/B/X/C）を宣言してから作業を始める。

### A. Issue に紐づく PR の head ブランチ

引数で Issue が指定された場合に試す。

1. Issue を確認する

    ```shell
    gh issue view <issue-url-or-number> --json number,title,url,state
    ```

2. Issue に紐づく PR を取得する

    ```shell
    gh api graphql -f query='
    query($owner:String!,$repo:String!,$number:Int!){
      repository(owner:$owner,name:$repo){
        issue(number:$number){
          timelineItems(itemTypes:[CROSS_REFERENCED_EVENT,CONNECTED_EVENT],first:100){
            nodes{
              __typename
              ... on CrossReferencedEvent{ source{ ... on PullRequest{ number headRefName state isDraft url } } }
              ... on ConnectedEvent{ subject{ ... on PullRequest{ number headRefName state isDraft url } } }
            }
          }
        }
      }
    }' -F owner=<owner> -F repo=<repo> -F number=<issue-number>
    ```

    取得できない場合の補助として次も使う

    ```shell
    gh pr list --state open --search "<issue-number>" --json number,title,headRefName,url
    ```

3. open な PR が 1 件に絞れたら、その `headRefName` を対象ブランチにする
4. open な PR が複数ある場合は A を成立させず C へ進む
5. 対象ブランチが現在のブランチと異なる場合も、A で一意に決まったなら切り替えて進める。ただし作業ツリーがクリーンでないときは切り替えず C へ進む

### B. 現在のブランチ

A が成立しなかった場合に試す。

1. `git branch --show-current` で現在のブランチを取得する
2. 現在のブランチが `develop` / `main` / `master` の場合は B を成立させず X へ進む
3. detached HEAD の場合は B を成立させず C へ進む
4. 現在のブランチに紐づく open PR があるか確認する

    ```shell
    gh pr view --json number,title,headRefName,baseRefName,url
    ```

5. PR があり、その head が現在のブランチなら、現在のブランチを対象にする
6. PR がない場合、そのブランチが作業ブランチだと判断できるなら現在のブランチを対象にしてよい。判断できないときは X へ進む

### X. 推論

A も B も成立しなかった場合に、根拠を挙げて推論する。

- ブランチ名に Issue 番号が含まれる（例: `feature/3207-xxx`、`fix/issue-3207`）
- ブランチの独自コミットのメッセージに Issue 番号や Issue タイトルと一致する記述がある

    ```shell
    git log --oneline origin/develop..HEAD
    ```

- 直近のブランチ操作履歴から作業ブランチが判別できる

    ```shell
    git reflog show --date=relative | head -30
    ```

- 現在の会話の文脈から対象が判別できる

推論で対象を 1 つに絞れた場合は、**推論であることと根拠を明示**してから進める。
根拠が弱く複数候補が残る場合は C へ進む。

### C. ユーザーへ確認して止まる

A・B・X のいずれでも対象を一意に決められない場合、または食い違いを検出した場合は、作業を止めてユーザーへ確認する。

- 候補ブランチとそれぞれの根拠を提示する
- 何が判断できなかったかを明示する
- 回答があるまで `fetch` 以外の操作をしない

## 手順

### 1. 前提を確認する

1. Git identity を確認する（`git-verify-identity` の手順に従う）
2. リポジトリルートと状態を確認する

    ```shell
    git rev-parse --show-toplevel
    git status --porcelain=v1 --branch
    ```

3. rebase / merge / cherry-pick が進行中の場合は、新しい rebase を始めず、まず進行中の操作をユーザーへ報告して指示を仰ぐ
4. 作業ツリーがクリーンでない場合は、**勝手に stash / commit / 破棄をしない**。変更内容を提示してユーザーへ確認する
5. `origin` remote の存在を確認する

    ```shell
    git remote get-url origin
    ```

### 2. 対象ブランチへ切り替える

1. 「対象ブランチの決定」で決めたブランチが現在のブランチと同じなら何もしない
2. 異なる場合は、切り替え先を宣言してから切り替える

    ```shell
    git switch <target-branch>
    ```

3. ローカルに存在しない場合のみ、リモート追跡ブランチから作成する

    ```shell
    git switch --track origin/<target-branch>
    ```

### 3. origin/develop を取得する

```shell
git fetch origin develop
```

- `git fetch origin/develop` は不正な指定なので使わない（`origin` と `develop` は空白区切り）
- 取得後の到達点を記録する

    ```shell
    git rev-parse --short origin/develop
    git rev-parse --short HEAD
    ```

- `git merge-base --is-ancestor origin/develop HEAD` が成立する場合は、すでに追従済みなので rebase せずその旨を報告して終了する

### 4. rebase する

```shell
git rebase origin/develop
```

- エディタが開く操作は非対話で実行する（必要なら `GIT_EDITOR=true` を付与する）
- `-i` / `--interactive` は使わない
- コンフリクトなく完了した場合はステップ 6 へ進む

### 5. コンフリクトを解消する

コンフリクトが出た場合、1 コンフリクトずつ次を繰り返す。

1. 状況を把握する

    ```shell
    git status --porcelain=v1
    git diff --name-only --diff-filter=U
    git log -1 --format='%h %s' REBASE_HEAD
    ```

2. 双方の意図を理解する

    ```shell
    git log --oneline origin/develop..REBASE_HEAD
    git log --oneline -5 origin/develop -- <conflicted-file>
    ```

3. 解消の原則を守る

    - **両方の変更意図を保持する**。片方を安易に捨てない
    - conflict marker を残さない。解消後に必ず確認する

        ```shell
        rg -n '^(<<<<<<<|=======|>>>>>>>)' <conflicted-file>
        ```

    - `git checkout --ours` / `--theirs` を無条件に使わない。片側採用が正しいと説明できる場合にのみ使う
    - lock ファイルや生成物（`pnpm-lock.yaml`、`eslint.seatbelt.tsv` など）は手で混ぜず、再生成して解決する
    - 変更対象に適用される `AGENTS.md` とプロジェクト規約に従う
    - コンフリクト解消に必要な範囲を超えたリファクタや整理を混ぜない
    - どちらを採るかで挙動・要件が変わる、意図が読み取れない、そもそも設計判断が必要な場合は**解消せず停止**し、該当箇所と判断が必要な点をユーザーへ提示する

4. 解消できたファイルを stage して続行する

    ```shell
    git add <resolved-file>
    GIT_EDITOR=true git rebase --continue
    ```

5. 次のコンフリクトが出たら 1 へ戻る

禁止事項

- `git rebase --skip` は使わない（コミットが消えるため）
- `git rebase --abort` はユーザーの承認を得たときだけ実行する
- `git rerere` の自動適用結果をそのまま信用せず、内容を確認する

### 6. 検証する

rebase 完了後、変更範囲に最も関連する検証を実行する。

- frontend の変更を含む場合

    ```shell
    cd frontend
    pnpm run lint
    pnpm run type-check
    pnpm run test
    ```

- コンフリクト解消でコードを触った場合は、該当箇所のテストも実行する
- 失敗した場合は原因を修正し、成功するまで再検証する。修正は `git commit --amend` ではなく、対象コミットへの修正が必要かユーザーへ確認する
- 実行できない検証は、理由を明記する

### 7. push は既定でしない

- 既定では push せず、ローカルの rebase 完了状態で止める
- ユーザーが明示的に push を依頼したときだけ実行する

    ```shell
    git push --force-with-lease
    ```

- `git push --force`（lease なし）は使わない
- `develop` / `main` / `master` へ直接 push しない

## 完了報告

次の内容を表で報告する。

- 対象ブランチと決定根拠（A / B / X / C）
- rebase 前の HEAD SHA と rebase 後の HEAD SHA
- 追従した `origin/develop` の SHA
- コンフリクトが発生したファイルと、それぞれの解消方針
- 実行した検証とその結果
- push の状態（未実行 / 実行済み）

停止した場合は、停止理由とユーザーに必要な次の判断を明記する。

## 禁止事項

- `git config` の変更、`git -c user.name=` / `git -c user.email=` による identity 注入
- `--no-verify` による hook のスキップ
- `git push --force`、`git reset --hard`、`git clean -f` の無断実行
- 対話モードを要求するコマンド（`git rebase -i` など）
