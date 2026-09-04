---
plan_id: common-agent-literacy-material
status: in-progress
created: 2026-09-04
updated: 2026-09-04
priority: high
next_actor: agent
next_action: "common/agent.md の執筆. 1 ハーネス → 2 環境構築 → 3 操作の最小セット の順 (第1回で使う範囲から)"
---

# 共通資料 - git とエージェント利用のリテラシー

## メタ情報

- **状態**: in-progress (`git.md` 執筆済み / `agent.md` は骨子のみ)
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
2. 環境構築 - herdr のインストール, codex のログイン (ChatGPT アカウント), 動作確認
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
| 1 | Ch1 科目説明 / `setup.md` VSCode と CLI / `agent.md` 1-3 (ハーネスとは何か, herdr と codex の導入, 最小の操作) |
| 2 | `git.md` 全体 / `agent.md` 4-7 (AGENTS.md, skill, リテラシー, 開発環境と実行環境) / Ch2 分析設計 |

第 2 回が重い. `agent.md` 8 (任意課題) と `git.md` 5 (branch / worktree) は
資料に置いて自習へ回す前提で組んでいる.

## 既存資料との関係

| 資料 | 対応 |
|---|---|
| `common/setup.md:53` | 「AI利用法に関しては後ほど扱いますが」という**着地先のない前方参照**. `agent.md` へのリンクにする |
| `common/setup.md:122-230` | CLI の基本操作は既存. `git.md` / `agent.md` で重複させず参照する |
| `common/python1.md:537-539` | 生成 AI 利用の方針の断片. 正本を `agent.md` へ移し参照に置き換える |
| `lectures/fp/fp1.md:639` | AI コーディング支援への言及. `agent.md` へリンクできる |
| `lectures/dsp/dsp_b1.md:49` | 教員が契約した X の認証トークンを配布する. `git.md` 3 の public/private の実例として直結 |
| `lectures/dsp/dsp8.md` | LLM の仕組み (Transformer, 注意機構, 自己教師あり学習) はこちらの担当. `agent.md` では数理を扱わない |

## 未確定 (台帳)

| 問い | 親 | 状態 | 分岐先 | 外へ出した物 | 戻る条件 | 結論 |
|---|---|---|---|---|---|---|
| `AGENTS.md` を学生は書くのか読むのか | agent.md 4 | resolved | - | - | - | 最低限を事例として配布し以後は各自が改善. 研究用 repo の規約から採る (2026-09-04) |
| 学生 repo の中身の見本を配るか | 学生が各自で作る | resolved | - | - | - | 資料に最小構成を載せる. `git.md` の §リポジトリの構成 に記載済み (2026-09-04) |
| 配布する `AGENTS.md` に何を書くか | AGENTS.md の扱い | open | - | - | 研究用 repo の規約から採る項目の選定 | - |
| Windows で herdr + codex が動くか | 学生の環境 | branched | 実機確認 | - | 学生の Windows 機 1 台で疎通 | - |
| skill をどこに公開するか (原典 `mattpocock/skills` MIT の表示を含む) | agent.md 5 | branched | 別 plan | 未起票 | 別 plan の起票と結論 | - |
| 第 2 回が重い問題 | 回への割付 | open | - | - | `agent.md` 執筆後の分量実測 | - |

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

- 2026-09-04: grill-me で設計を確定して作成. `llm.md` の骨子を `git.md` / `agent.md` へ分割.
  `plan/proposed/slds-special-coding-agent-material.md` を吸収.
- 2026-09-04: `common/git.md` を執筆. `common/agent.md` を骨子として起こし, `common/llm.md` を削除.
  `setup.md:53` の着地先のない前方参照, `python1.md:537` の生成 AI 方針, `fp1.md:639` の
  AI 支援への言及から `agent.md` への導線を張った.
