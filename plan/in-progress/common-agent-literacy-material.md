---
plan_id: common-agent-literacy-material
status: in-progress
created: 2026-09-04
updated: 2026-09-10
priority: high
next_actor: user
next_action: "Windows 実機で herdr + codex の疎通確認と初回認証画面 (credential.png) の撮影 (外部待ち b)"
---

# 共通資料 - git とエージェント利用のリテラシー

## メタ情報

- **状態**: in-progress (`git.md` / `agent.md` とも執筆済み, main に land, 2026-09-10 に `open: true` 化. 残りは Windows 実機確認)
- **作成日**: 2026-09-04
- **対象**: `lectures/common/git.md` (新規), `lectures/common/agent.md` (新規),
  `lectures/common/setup.md` (既存, 前方参照の解決), `lectures/common/llm.md` (骨子 → 上記 2 本へ吸収して削除)
- **親計画**: [dsp-course-material.md](dsp-course-material.md) - 第 1〜2 回で扱う共通資料
- **吸収した計画**: `plan/proposed/slds-special-coding-agent-material.md` (学生向けコーディングエージェントの使い方 教材)
- **切り出した計画**: skill の公開 (grill-me 等) は別 plan. 本計画では「導入を推奨する」と書くにとどめる

## 概要

データサイエンス実践の第 1〜2 回で扱い, 他講義からも参照する共通資料.
**課題のコードを LLM に書かせ, それを読んで直せるようになる**ことを目的とし,
そのための最低限の操作と前提知識を扱う.

`lectures/common/llm.md` として起こした骨子は, 分量が 1 ファイルに収まらないため
**`git.md` と `agent.md` の 2 本に分割**する (2026-09-04 決定).

## 到達目標

学生が資料を読み終えた時点で,

1. エージェントに書かせたコードを読み, どこが何をしているか説明でき, 直せる
2. 自分の repo に作業を記録し, 変更を diff で確認できる
3. 何を public に置いてよく, 何を置いてはいけないか判断できる
4. 分からないまま承認せず, 理解せずに次へ進まない - その具体的な手順を持っている

4 は最終回の発表で効く. **やったかどうかは評価しないが, やっていないと質疑に答えられない**
という位置づけで提示する (2026-09-04 決定).

## 学生の環境 (2026-09-04 確定)

| 要素 | 選定 | 備考 |
|---|---|---|
| ターミナル | **herdr** (Agent multiplexer, Apache-2.0, v0.8.2) | macOS / Linux / Windows 対応. macOS/Linux は `curl -fsSL https://herdr.dev/install.sh \| sh`, Windows は `irm https://herdr.dev/install.ps1 \| iex` |
| エージェント | **codex** (OpenAI Codex CLI) | 学生が ChatGPT に馴染んでいること, 単価が安いことが選定理由 |
| 課金 | **ChatGPT Plus $20/月 を 3 ヶ月** | 学生負担. 3 ヶ月で約 9,000 円 |
| skill | `~/.agents/skills/` または `.agents/skills/` の `SKILL.md` | codex は 2025-12 から対応 |
| repo | **学生が各自の GitHub アカウントで作る** | 教員側でテンプレートや Classroom は用意しない |

- 2026-04-02 から Codex はトークンベースのクレジット課金. Plus は GPT-5.6 で 5 時間あたり
  Sol 15-90 / Terra 20-110 / Luna 50-280 (2026-09 時点の公開情報).
- `plan/proposed/free-cli-agent-for-univ-course.md` の 2026-06 時点の結論
  (Gemini API 無料キー + Aider / Cline) は**差し替わった**. 同 plan は記事化ブリーフとして残す.

## 章構成

### `common/git.md` - バージョン管理と GitHub

エージェントの変更を確認し, 作業を記録し, 公開範囲を判断するための最低限.

1. なぜエージェントと git を一緒に使うのか (変更が diff で見える, 戻せる)
2. GitHub とは何か - ホスティングサービスであって git そのものではない
3. **git を入れる** - Windows は winget, macOS は Homebrew. `user.name` / `user.email` の設定
   (public リポジトリではメールアドレスが読めることに注意)
4. **GitHub のアカウントとリポジトリを作る** - Sign up, 二段階認証 (2023 年から push 利用者に必須),
   New repository (private / README / Python の .gitignore), Code から HTTPS の URL を取って clone.
   認証はブラウザ経由 (パスワード認証は 2021 年に廃止)
3. **public と private の違い** - 何を public に置いてはいけないか
   - API キー, 認証トークン (補足 B で配布する X の共有トークンが直接該当する)
   - 個人情報を含むデータ, 提供を受けたデータ
4. 最低限の操作 - `clone` / `status` / `diff` / `add` / `commit` / `push`
5. **エージェントが使うものを理解する程度の branch と worktree**
   - 操作を覚えるのではなく, herdr / codex が何をしているかを読めるようにする

### `common/agent.md` - コーディングエージェントの利用

1. **ハーネスとは何か** - エージェントが動くよう設計された環境 (ツール・情報形式・フィードバック・足場).
   「言語モデルにとってインターフェースが認知アーキテクチャ」
   - 素材: `Research/audit-harness/slides/2026-08-31-audit-harness-overview/00-common-a.md` の冒頭 3 節.
     **private repo なのでリンクせず, 学部生向けに書き直す**
2. 環境構築 - codex の導入と `codex login` (ChatGPT アカウント), herdr の導入, 動作確認
3. 操作の最小セット - 対話の開始と終了, `/clear` (文脈を切る), 差分の確認と承認
4. `AGENTS.md` - repo の決まりごとをエージェントに読ませる
   - **最低限のものを事例として配布し, 以後は各自が改善していく形にする** (2026-09-04 決定)
   - 現行の研究用 repo の規約からいくつか採る (文献の本文確認必須化など)
5. skill - 繰り返す手順を `SKILL.md` として切り出す. 導入を推奨する skill の紹介
6. **リテラシー** - 分からないまま承認しない / 理解せず次に進まない
   - 現行 `common/python1.md:537-539` に断片があるので, **正本をこちらへ移し, python1 側は参照に置き換える**
7. **AI の開発環境と実行環境** (モデルカリキュラム **3-10 ☆**)
   - 学習する環境と推論する環境の違い, 手元と Colab と API の使い分け
8. 発展 (任意課題) - LLM をただ使うのでなく, LLM で学習する方法
   - 自分用の repo に学んだことを md / HTML で書き出して残す
   - 分岐して詰める手順 (grill-me 型), 分からないまま承認しない具体的手順
   - **成績評価の対象にしない. ただし最終回の質疑で効くことを明示する**

## 回への割付

第 1〜2 回のまま (2026-09-04 決定). 同じ回に Ch1 (本資料の読み方) と
Ch2 (分析設計, **1-2 ☆**) も入る.

| 回 | 内容 |
|---|---|
| 1 | Ch1 科目説明 / `setup.md` VSCode と CLI / `git.md` (インストール, GitHub 登録, Collaborator 招待, clone, 7 コマンド, 公開範囲の判断) |
| 2 | `agent.md` 1-7 (ハーネス, herdr と codex の導入, 最小の操作, AGENTS.md, skill, リテラシー, 開発環境と実行環境) / Ch2 分析設計 |

2026-09-06 に `git.md` を第 1 回へ前倒し (ユーザ裁定. 執筆後の実測で第 2 回が git.md 689 行 +
agent.md 127 行 + Ch2 となり 90 分に収まらなかったため). codex を動かす前に git が入っている
順序にもなる. `agent.md` 8 (発展) は資料に置いて自習へ回す.

## 既存資料との関係

| 資料 | 対応 |
|---|---|
| `common/setup.md:53` | 「AI利用法に関しては後ほど扱いますが」という**着地先のない前方参照**. `agent.md` へのリンクにする |
| `common/setup.md:122-230` | CLI の基本操作は既存. `git.md` / `agent.md` で重複させず参照する |
| `common/python1.md:537-539` | 生成 AI 利用の方針の断片. 正本を `agent.md` へ移し参照に置き換える |
| `lectures/fp/fp1.md:639` | AI コーディング支援への言及. `agent.md` へリンクできる |
| `lectures/dsp/dsp_b1.md:49` | 教員が契約した X の認証トークンを配布する. `git.md` 3 の public/private の実例として直結 |
| `lectures/dsp/dsp8.md` | LLM の仕組み (Transformer, 注意機構, 自己教師あり学習) はこちらの担当. `agent.md` では数理を扱わない |

## 導入コマンド (2026-09-04 調査)

学生の導入を **winget (Windows) / Homebrew (macOS)** に寄せる. GUI インストーラは
選択肢の多い画面が続き, 間違えると後で分かりにくい不具合になるため使わない.

| 対象 | Windows | macOS | 置き場所 |
|---|---|---|---|
| git | `winget install -e --id Git.Git` | `brew install git` | `git.md` |
| GitHub CLI | `winget install -e --id GitHub.cli` | `brew install gh` | `git.md` (認証を `gh auth login` で済ませる) |
| codex | `winget install -e --id OpenAI.Codex` | `brew install codex` | `agent.md` |
| herdr | **winget 不可**. 公式 PowerShell インストーラ | 公式 sh インストーラ | `agent.md` |

- winget は Windows 11 に標準搭載. Windows 10 の古い版は Microsoft Store の
  「アプリ インストーラー」(App Installer) を入れさせる (2026-09-04 決定).
- codex の winget パッケージ `OpenAI.Codex` は **OpenAI, Inc. 発行の公式** (Apache-2.0).
- herdr の winget にある `hdosys.herdr-win` は **第三者による非公式ディストリビューション**なので使わない.
- Windows のエンドポイントセキュリティが herdr の PowerShell ワンライナーを止めることがある.
  公式に `install.cmd` の代替手順がある.

## スクリーンショット (2026-09-04 撮影, 収録済み)

`images/common/git/` に 8 枚. 計 1.8 MB. 元は 3800px 幅だったので幅 440-1600px に縮小した.

| ファイル | 内容 |
|---|---|
| `top.png` | GitHub トップページ (Sign up の位置) |
| `signup.png` | アカウント作成フォーム |
| `menu.png` | 右上アイコンのメニュー (Settings の位置) |
| `settings-auth.png` | Settings > Password and authentication |
| `2fa.png` | Two-factor authentication (Enabled の状態) |
| `new-repo-menu.png` | `+` メニューの New repository |
| `new-repo.png` | 作成の入力画面 (Private / README On / .gitignore Python) |
| `code-button.png` | Code ボタンから HTTPS の URL |

**撮影で判明した本文の誤り** (2026-09-04 に修正済み). New repository の UI が
書いていたものと違っていた.

| 誤って書いていた記述 | 実際 |
|---|---|
| Public / Private を選ぶ | **Choose visibility** のドロップダウン |
| Add a README file にチェック | **Add README** のトグル (On/Off) |
| (構造への言及なし) | **1 General / 2 Configuration** の 2 段構成 |

スクリーンショットを撮らずに出していたら, 誤った手順のまま配ることになっていた.

`new-repo.png` のリポジトリ名は動作確認時の `aaaaa` のままだが, 本文で
「図では動作確認のため `aaaaa` と入れている」と補って使う (2026-09-04 決定).

**未収録**: 初回 `clone` / `push` のブラウザ認証画面 (`credential.png`).
手元の macOS は資格情報が保存済みでこの画面が出ないため撮れない.
Windows 実機での herdr + codex 疎通確認のときに Git Credential Manager の画面を
1 枚撮って追加する (2026-09-04 決定). 本文は文章のみで完結させてある.

## git.md の cross-check (2026-09-04)

codex (GPT-5.6, リポジトリを直接読ませた) と Cursor-Fable (全文を貼った) の 2 者に
独立レビューを取り, 指摘を Claude が検証して裁定した.

### 反映した技術的な誤り

| 指摘 | 検証 | 対応 |
|---|---|---|
| **macOS の HTTPS 認証はブラウザを開かない** | system の `credential.helper` が `osxkeychain` であることを実機で確認. ターミナルで Username/Password を聞かれ, password には personal access token が要る | `gh auth login` を導入手順に入れ, GitHub CLI を Windows/macOS 両方でインストールさせる形へ. 「パスワードを求められたらそれは別のもの」という誤誘導の一文を削除 |
| `git restore <file>` は index から戻す | 公式仕様どおり | `--source=HEAD` を明示. 取り消せないこと, 追跡外のファイルは消えないことを warn で追記 |
| **git は空ディレクトリを記録しない** | 仕様 | Exercise GIT-1 が成立していなかった. `notes/` `src/` に実ファイルを置く手順へ. §ディレクトリの構成 にも note を追加 |
| **GitHub の Python テンプレートは `.env` を含む** | `github/gitignore` の `Python.gitignore` を取得して確認 (`.env` `.envrc` `.venv` `__pycache__` `.ipynb_checkpoints` を含む) | 「Python を選んでも `.env` は除外されない」は誤り. 足りないのは `data/` と表計算ファイルだけ, と訂正 |
| `git diff` は untracked も staged も表示しない | 仕様 | 「承認前に diff を読む」が新規ファイルに効かない致命的な穴. `git status` で一覧を見て新規ファイルは開く, `git diff --staged` で記録直前に確認する, の 2 つを追加 |
| 合流図が octopus merge になっていた | 3 親の同時マージは通常のマージではなく, コンフリクトで停止せず失敗する | 2 段階の合流 (M1 → M2) の図へ描き直し |
| `.gitignore` の `*` は `/` に一致しない | 仕様 | パターンの読み方を独立した小節にして 3 規則を明記 |
| 「GitHub を使うには git が必要」 | GitHub の Web UI だけでも編集できる | 「エージェントは手元のファイルを書き換えるので手元の git を使う」という限定へ |
| 2FA 必須化の対象 | GitHub が必須化したのは選定された contributor で, 全 push 利用者ではない. 期限後は締め出しでなくアクセス時の設定誘導 | 「乗っ取り防止のために最初に設定する」という根拠へ置き換え |
| `git add .` は cwd 以下 | 仕様 | 明記 |
| クラウド同期の機構説明が過剰 | 単一端末では順不同アップロードで壊れるわけではない. 実害は `.git/index.lock` の競合, 重複ファイル, タイムスタンプ, オンデマンドの退避 | 機構を 4 つの具体的な症状へ書き直し |
| winget 不在 = 古い Windows 10 と断定 | Windows 11 でも登録未完了で認識されないことがある. Windows 10 は 1809 以降が条件 | 切り分けを訂正 |
| macOS の Homebrew が前提未解決 | `setup.md:287` は「brew に関しては自分で調べてみましょう」だけ | brew.sh の日本語ページへリンク |
| `uv.lock` は uv プロジェクトに限る | `pyproject.toml` がある場合の話 | 条件を明記 |
| `AGENTS.md` に秘密を書ける | 記録される以上当然 | 鍵やアカウント名を書けない旨を追記 |
| `git status` の例に `analysis_result.csv` | 自分の `.gitignore` で `*.csv` を除外しているので出ない | 例を `notes/2026-04-15-groupby.md` へ変更 |
| e-Stat は「再配布に制限がない」ではない | 出典表示が条件 | Exercise の回答を「利用規約を確認してから判断」へ |
| `.claude/` `.cursor/` の分類が誤りかつ学生に無関係 | 学生は codex しか使わない. `.claude/settings.json` 等は共有前提 | `.gitignore` の例から削除 |
| codex はリポジトリ内に何も作らない | codex は trusted project で `.codex/` を読む | 当該記述を削除 |

### 反映した安全上の欠落

- **`.ipynb` は出力をファイル本体に埋め込む**. `data/` と `*.csv` を除外しても notebook を
  記録すればデータが出ていく. 最も起きやすい漏洩経路なので warn を追加.
- **private は公開範囲を狭めるだけ**. 提供データや個人情報を GitHub へ送ってよい根拠にはならない.
  独立した小節を追加.
- **`data/` と `.env` のバックアップが消える**. 同期から外し GitHub からも外すと, どこにも残らない.
  原本をリポジトリ外に保管する指示を追加.
- **`git commit` を `-m` 無しで実行すると vim が開く**. `core.editor` の設定と, 開いてしまった
  ときの `Esc` `:q!` を追加.
- **Windows は git インストール後に PowerShell を開き直す必要がある**. 追加.
- **`git log`** を教えていなかった. 「結果を出した版の同定」を動機に挙げながら履歴を見る手段が
  無かったので追加 (コマンドは 6 つから 7 つへ).

### 反映した文体の指摘

見出しを対象を指す句へ (`git を入れる` → `git のインストール` 等) / 太字を論理の要所に限る /
`してしまう` の演出を事実の断定へ / 演習の回答例を敬体へ / 「触れた」等の禁止語を削除 /
「3 つ」と 4 節の不一致を解消.

### 未反映 (ユーザ判断が要る)

1. **作業モデルの不整合**. §なぜ… は「エージェントが自分のディレクトリを触る」前提で
   commit と diff を教えるが, §worktree では herdr がエージェントごとに別 worktree を作ると書いている.
   後者なら学生の main worktree で `git diff` を打っても何も出ない. エージェントの変更を
   main へ取り込む方法 (`git merge` か herdr の操作) も書いていない.
   どちらの作業モデルで教えるかを `agent.md` と揃える必要がある.
2. **分量**. 90 分に対して git 導入 + GitHub 登録 + 2FA + 認証 + `.gitignore` + 7 コマンド +
   branch/worktree + 演習 3 本. Cursor は §branch/worktree を `agent.md` へ移すことを推奨.
3. **共通資料が特定講義に依存**. X の認証トークンの例を一般化したが, 教員がリポジトリを
   どう見るか (Collaborator への招待手順) は未記述.
4. **`<details data-pass>` はページソースにパスワードが平文で入る**ので保護になっていない.
   基盤側の問題で本資料の範囲外だが, 回答例を隠す設計の前提として認識が要る.

## 未確定 (台帳)

| 問い | 親 | 状態 | 分岐先 | 外へ出した物 | 戻る条件 | 結論 |
|---|---|---|---|---|---|---|
| `AGENTS.md` を学生は書くのか読むのか | agent.md 4 | resolved | - | - | - | 最低限を事例として配布し以後は各自が改善. 研究用 repo の規約から採る (2026-09-04) |
| 学生 repo の中身の見本を配るか | 学生が各自で作る | resolved | - | - | - | 資料に最小構成を載せる. `git.md` の §リポジトリの構成 に記載済み (2026-09-04) |
| 配布する `AGENTS.md` に何を書くか | AGENTS.md の扱い | open | - | - | 研究用 repo の規約から採る項目の選定 | - |
| Windows で herdr + codex が動くか | 学生の環境 | branched | 実機確認 | - | 学生の Windows 機 1 台で疎通 | - |
| skill をどこに公開するか (原典 `mattpocock/skills` MIT の表示を含む) | agent.md 5 | branched | 別 plan | 未起票 | 別 plan の起票と結論 | - |
| 第 2 回が重い問題 | 回への割付 | resolved | - | - | - | branch/worktree 節を agent.md の発展節へ移し, `git.md` を第 1 回へ前倒し (2026-09-06). 第 1 回 = setup + git, 第 2 回 = agent 1-7 + Ch2 |
| エージェントの作業モデル (main を触るか worktree を分けるか) | git.md と agent.md の整合 | resolved | - | - | - | 1 ディレクトリ (main) で codex を動かし, 差分は `git diff` と `/diff` で読む. worktree は agent.md の発展節のみ (2026-09-06) |
| 教員が学生の private repo をどう見るか | 学生が各自で作る | resolved | - | - | - | 学生が教員を Collaborator に招待する. git.md の GitHub 節に小節を追加 (2026-09-06) |
| GitHub 手順のスクリーンショット | git.md の GitHub 節 | resolved | - | - | - | 8 枚を 2026-09-04 に撮影して収録. 撮影の過程で New repository の UI 記述の誤りが 3 件見つかり修正 |
| 初回認証画面の図 (`credential.png`) | git.md の clone 節 | branched | Windows 実機確認 | - | Windows で Git Credential Manager の画面を撮影 | - |

**先送りの理由**: Windows 検証は外部待ち (実機と学生) のため (b). skill 公開は
教材執筆とは独立した作業で別 plan が妥当なため (d).

## 却下した案

| 案 | 却下理由 | 判定日 |
|---|---|---|
| Gemini API 無料キー + Aider / Cline | 学生が ChatGPT に馴染んでおり codex の方が単価も安い | 2026-09-04 |
| `llm.md` 1 本にまとめる | 新規執筆 8 項目が 1 ファイルに収まらない | 2026-09-04 |
| Python から LLM の API を叩く方法を教える | それは LLM が実装するので, 環境構築と操作とリテラシーに集中する | 2026-09-04 |
| GitHub Classroom / テンプレート repo の配布 | 学生が各自で作る方針. ただし資料には最小構成の見本を載せる | 2026-09-04 |
| `AGENTS.md` を読めるだけでよいとする | 事例を配って各自が改善する形にした | 2026-09-04 |
| 任意課題を加点対象にする | 最終発表の質疑で効くので, やったかどうかは評価しない | 2026-09-04 |

## 変更履歴

- 2026-09-10: ユーザ指示により `git.md` / `agent.md` を DSP 全体に先行して `open: true` 化.
  `stack exec main -- build` が成功し, 生成 HTML に本文が出ることを確認. `setup.md` には winget / Homebrew
  を講義の標準的なソフトウェア管理手段とする説明を加え, VSCode と `tree` の導入手順も統一.
- 2026-09-04: grill-me で設計を確定して作成. `llm.md` の骨子を `git.md` / `agent.md` へ分割.
  `plan/proposed/slds-special-coding-agent-material.md` を吸収.
- 2026-09-04: `common/git.md` を執筆. `common/agent.md` を骨子として起こし, `common/llm.md` を削除.
  `setup.md:53` の着地先のない前方参照, `python1.md:537` の生成 AI 方針, `fp1.md:639` の
  AI 支援への言及から `agent.md` への導線を張った.
- 2026-09-04: `git.md` に インストール と GitHub のアカウント/リポジトリ作成 の節を追加.
  `agent.md` の環境構築に codex と herdr の導入コマンドを記載. 導入は winget / Homebrew に
  寄せ, herdr だけ公式インストーラを使う (winget の該当パッケージは非公式のため).
- 2026-09-04: GitHub 手順のスクリーンショット 8 枚を撮影して `git.md` に収録.
  撮影により New repository の UI 記述の誤り 3 件 (visibility はドロップダウン,
  README はトグル, 2 段構成) が判明し修正. 初回認証画面のみ Windows 確認時へ持ち越し.
- 2026-09-04: `git.md` を codex + Cursor-Fable で cross-check. 技術的な誤り 17 件と
  安全上の欠落 6 件を反映して全面改訂. macOS の HTTPS 認証がブラウザを開かない
  (`credential.helper` = `osxkeychain`) ことを実機で確認し, `gh auth login` を導入手順へ追加.
  空ディレクトリが記録されないため Exercise GIT-1 が成立していなかったのを修正.
  未反映の 4 件 (作業モデルの不整合, 分量, 教員のアクセス, details の保護) を台帳へ.
- 2026-09-06: 未確定 3 件をユーザ裁定. 作業モデル = 1 ディレクトリで codex (worktree は発展のみ) /
  git.md の branch・worktree 節を agent.md 発展へ移す / 教員は Collaborator 招待.
  `agent.md` の本文を codex に下書きさせる (brief = 章構成 1〜8 + 演習 AGENT-1〜3 + git.md・python1.md の小改訂).
- 2026-09-06: `agent.md` の本文を執筆 (codex 下書き → Claude 裁定, commit `92edeb3`). 8 節 + Exercise AGENT-1〜3.
  git.md の branch/worktree 節を agent.md の発展へ移し, Collaborator 招待の小節を追加. python1.md の
  生成 AI 段落を agent.md への参照に集約. prose-lint は agent.md / git.md とも指摘 0.
  分量実測: 第 1 回 = agent.md 1〜3 節 222 行, 第 2 回 = git.md 689 行 + agent.md 4〜7 節 127 行 + Ch2.
- 2026-09-06: 割付を確定. `git.md` を第 1 回へ前倒しし, 第 2 回を `agent.md` 1-7 + Ch2 に (ユーザ裁定).
- 2026-09-06: `agent.md` のレビューは作者 (codex) と別ベンダの Claude-Fable が全文で行い, 作者≠reviewer を充足.
  Cursor-Fable は主戦と同一モデルになるため追加しない. 事実の根拠は codex / herdr の公式文書
  (2026-09-06 取得) で, brief に載せた一覧以外のコマンドは本文に無いことを codex の報告と grep で確認.
