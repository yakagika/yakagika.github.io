---
id: haskell-blog-python-lecture
scope: repo
reason: python-coding-style seed (scope python-lecture) に対する本 repo 固有の値 (repo-local slot). 規則本文は seed を lock 経由で読む.
since: 2026-09-30
---

# Python コーディング規則: haskell-blog の講義コード

## slot の値

- **講義 scope の対象**: `lectures/` の教材 md に載せる Python コード例 (`lectures/slds/`, `lectures/dsp/`,
  `lectures/common/python1-5.md` など) と, `slds_code/` の Python source. 対象外は `_scratch/`, `docs/`
  (生成物), `archive/`, `slds_data/`, `slds_papers/`.
- **行幅**: 90 桁 (全角は 2 桁で数える). 2026-09-30 の実測で教材 md の Python コード例は
  99 パーセンタイルが 87 桁, `slds_code/` は 95 パーセンタイルが 82 桁だった.
  この repo には `pyproject.toml` が無く Ruff の設定を置かないので, 整形するときは
  `ruff format --line-length 90` を明示する.
- **docstring の言語**: 日本語可 (講義 scope の緩和に従う).
- **型ヒント**: 授業の段階に合わせる (seed の講義 scope に従う).
- **共通可視化 module の場所**: なし.
- **Ruff の選択規則**: なし (advisory で必要なときだけ F / B の不具合系).

## 運用

- 教材 md のコード例か `slds_code/` のコードを追加・変更するときだけ読み, 変更した例だけに当てる.
  既存の例の一括整形はしない.
