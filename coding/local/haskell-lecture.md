---
id: haskell-blog-haskell-lecture
scope: repo
reason: haskell-coding-style seed (scope haskell-lecture) に対する本 repo 固有の値 (§7 の repo-local slot). 規則本文は seed を lock 経由で読む.
since: 2026-09-30
---

# Haskell コーディング規則: haskell-blog の講義コード

## §7 slot の値

- **講義 scope の対象**: `lectures/` の教材 md に載せる Haskell コード例 (主に `lectures/fp/`) と,
  `fp-examples/` の検証用 test と補助 source. 対象外は `_scratch/`, `docs/` (生成物), `archive/`,
  `src/` (Hakyll のサイト生成コード; 2026-09-30 本人裁定で対象に含めない).
- **build / 検査コマンド**: `cd fp-examples && stack test`.
- **規則 2-3 の行幅上限**: 90 桁 (全角は 2 桁で数える). 2026-09-30 の実測で fp-examples の test は
  99 パーセンタイルが 88 桁, 教材 md のコード例は 95 パーセンタイルが 82 桁だった.
- **規則 3 の並びの閾値**: 3 (seed の既定).
- **規則 5 の例外を適用する dir**: なし.
- **凍結 kernel の所在**: なし.
- **規則 9 の記述量予算, 規則 38 の行数 trigger**: 講義 scope では当てない.

## 運用

- 教材 md のコード例か `fp-examples/` のコードを追加・変更するときだけ読み, 変更した例だけに当てる.
  既存の例の一括整形はしない.
- 教材の例と `fp-examples/test/Fp<N>/*Spec.hs` の対応を崩さない (`CLAUDE.md` の fp 講義編集ルール).
