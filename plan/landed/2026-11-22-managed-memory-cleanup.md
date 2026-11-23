---
plan_id: managed-memory-cleanup-2026-11-22
status: landed
created: 2026-11-22
updated: 2026-11-22
priority: low
next_actor: none
next_action: "なし (3 項目とも本人の回答どおりに反映し, receipt を書いた)"
---

# assistant からの memory 整理の依頼 (2026-11-22)

## メタ情報

- **状態**: landed (2026-11-22)
- **作成日**: 2026-11-22
- **出所**: 引き継ぎの依頼 `assistant-2026-11-22T07-21-45-556106-managed-memory-の整理` (substantive)

## 概要

assistant の監査が, この repo の agent の memory に 3 件の不整合を挙げた. substantive の依頼なので, 承認の前には memory と receipt を書き換えない.

## 動機

memory は以後の session の手順に使われる. 古い進行状態や, 食い違う手順が残ると誤った作業につながる.

## 設計

### 1. 公開時の検査の確認待ち

- 対象: `project_dsp_parallel_orchestration.md`, `plan/landed/2026-10-28-publish-broken-ref-check.md`
- 依頼の推奨: memory から進行状態を外し, 計画の next_action に集約する.
- 確かめた事実: 検査の導入 (1f4b592) の後に Publish の commit が 4 件ある (b945c31, 53ca494, d2be5e1, 38be243). どの時点の `publish.sh` にも `set -e` と検査があり, 4 件の `docs/` に現在の検査を掛け直すと全件 exit 0 だった. 当時に script 経由で実行した log は見ていない.
- 案 (推奨): 本人が「4 回の公開は `publish.sh` で行い, 検査で止まらなかった」と認めれば, memory の進行状態の 1 文を消し, 計画の確認待ちを閉じる. 掛け直しは補助の根拠で, agent だけでは確認済みにしない.
- 代案: 依頼どおり memory の 1 文だけを消し, 計画の確認待ちは残す.

### 2. 再 build と watch の競合

- 対象: `gotcha_docs_reset_needs_rebuild.md`
- 確かめた事実: 「publish.sh は rebuild を使うので公開には影響しない」とある. もう一方の memory には, watch の常駐中は rebuild が失敗しうるとある. 競合そのものは再現していない.
- 案 (推奨): 該当の文を「watch が動いていなければ公開時の rebuild で復元される. 競合するときは watch を止めてから publish.sh を走らせる」に直す.

### 3. 抽象概念を導入する順序

- 対象: `feedback_lecture_math_haskell_parallel.md` (2026-07-09), `feedback_fp_lecture_design_principles.md` (2026-07-13)
- 確かめた事実: 前者は数学の定義を Haskell より先か並べて書くとする. 後者は新しい抽象概念を「具体 2 例 → 一般定義 → Haskell の機構」の順で導入するとする. 後者の原則 4 は本人の言葉の 3 点には入らず「観察から追加, ユーザに提示済み」と記録されている. 元の session の transcript は無く, 本人がこの順序まで承認したかは確認できない.
- 2 つの記録は対立しない (どちらも一般定義が Haskell より先). 増える制約は「具体例を先に」だけ.
- 聞くこと: この順序を既定として承認するか.
- 案 (推奨): 本人が順序を承認すれば, 前者に「新しい抽象概念は具体 2 例 → 一般定義 → Haskell の機構の順. 既習の概念は並べて示してよい」を足す.

## ロードマップ

1. 3 項目を 1 問ずつ, 同じ AUQ で聞く.
2. 承認分だけを反映する.
3. receipt に項目ごとの採否を書き, この計画を landed へ移す.

## コスト見積もり

memory 3 file と計画 1 file の数行.

## 未解決事項 / リスク

- なし (3 項目とも本人が回答).
- 講義, code, 公開物は変えない.

## 関連ファイル

上の各項目の「対象」.

## 変更履歴

- 2026-11-22: 依頼を受領し, 根拠を確かめて起票した.
- 2026-11-22: 本人の回答. 1 = 通ったので閉じる, 2 = 直す, 3 = 既定にしない. 1 と 2 は反映した. 3 は, 回答の後に `writing/local/lecture-design.md:16` に同じ既定があると分かった (質問の前の調査漏れ). 質問に書いた適用範囲の外なので, 確認してから反映する.
- 2026-11-22: 3 の最終の回答「具体例の数を2つにするという限定は必要ない」. memory と `writing/local/lecture-design.md` から数の限定だけを外した. 順序は残し, 07-09 の memory への追記はしない. landed へ移した.
