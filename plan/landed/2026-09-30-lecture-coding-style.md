---
plan_id: lecture-coding-style
status: landed
created: 2026-09-30
updated: 2026-09-30
priority: medium
next_actor: none
next_action: ""
---

# 講義コードへの coding-style 導入

## メタ情報

- **状態**: landed (2026-09-30 承認・実装. 行幅は実測で 90 桁, coding/ 新設と lock, CLAUDE.md へ案内を追記, fp-examples の stack test 通過)
- **由来**: handoff `assistant-2026-09-27T17-07-59-305479-講義コードに-haskell-と-python-の-coding-style-を` (2026-09-27 本人裁定)
- **class**: substantive → 2026-09-30 承認済み

## 目的

講義資料としてのコード (教材 md 内のコード例, 例題 dir) に Haskell と Python の執筆規約を入れる.
説明規則は講義向けに緩める (Haddock / docstring と main への集約は短い例では不要, コメントは日本語可,
Haskell は型 signature を書く, Python の型ヒントは授業の段階に合わせる). 見た目の規則
(空行, 行幅, 命名, Haskell の縦揃え, Python の Ruff format) と実害の防止は当てる.

## 対象

- 対象: `lectures/` の教材 md 内の Haskell / Python コード例, `fp-examples/`, `slds_code/`.
- 対象外: `_scratch/`, `docs/` (生成物), `src/` (Hakyll のサイト生成コード; 2026-09-30 本人裁定で profile に含めない).
- 既存の教材コードは変更しない (新規・変更した例だけに当てる).

## 作業

1. `coding/coding-style.yaml` を作る (参照型; 形式は ExchangeAlgebra の同名ファイルに倣う).
   profile は 2 つ: `haskell-coding-style` (scope `haskell-lecture`) と `python-coding-style` (scope `python-lecture`).
2. `coding/local/` に両 seed の Repo-local slot を実態で埋める (対象 path, 行幅は現状の実測で決める).
3. `writing_lock.py --kind coding` の bump で lock を作り, 同じ commit に含める.
4. `CLAUDE.md` の fp / slds 講義編集ルールに 1 行足す: 教材 md のコード例か例題 dir のコードを追加・変更する前に
   `writing_lock.py --kind coding paths --profile <haskell-coding-style|python-coding-style>` が返す file を Read し,
   変更した例だけに当てる.
   Python の convention は category research にしか配信されず, md のコード例は path 規則でも発火しないので,
   この案内が唯一の読み込み経路になる.

## 受け入れ基準

- `writing_lock.py --kind coding paths --profile haskell-coding-style` / `python-coding-style` が pin 済み seed と local slot を返す.
- `CLAUDE.md` に読み込みの案内がある.
- `lectures/`, `fp-examples/`, `slds_code/` の既存コードに差分が無い.
- `cd fp-examples && stack test` が通る.

## リスク

- 行幅を実測で決めるため, 既存例との乖離が大きい場合は local slot で例外として記録する (既存コードは触らない).
- 隔離 worktree は不要 (コードではなく設定・文書の追加のみ; main で可).
