---
title: exchangealgebra を Hackage に登録
description: 自作の Haskell ライブラリ exchangealgebra を Hackage に公開しました. 簿記を代数的に扱い, 経済シミュレーションの最小単位として使うためのライブラリです. チュートリアルは関連論文の正式公表後に書く予定です.
tags:
    - haskell
    - hackage
    - exchangealgebra
    - accounting
    - simulation
featured: true
date: 2026-05-26
tableOfContents: true
---

# はじめに

自作の Haskell ライブラリ [`exchangealgebra`](https://hackage.haskell.org/package/exchangealgebra) を Hackage に登録しました. この投稿はその告知のみで, チュートリアル的な使い方は関連論文を正式公表してから別記事として書く予定です.

ソースコードと README は GitHub にあります.

- Hackage: <https://hackage.haskell.org/package/exchangealgebra>
- GitHub: <https://github.com/yakagika/ExchangeAlgebra>

# Exchange Algebra とは

`exchangealgebra` は出口 弘 先生が提唱した [Exchange Algebra](https://www.springer.com/gp/book/9784431209850) を Haskell で実装したライブラリです. 簿記の仕訳をスケール付きの基底代数の要素として扱うことで, 仕訳・締め・振替・シミュレーションを関数合成と射影で書けるようにします.

- 書籍: <https://www.springer.com/gp/book/9784431209850>
- 原典論文 (PDF): <https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/82987/1/0809-7.pdf>

理論の詳細はチュートリアル記事で改めて扱うので, ここではモジュール構成の概観だけ書いておきます.

# モジュール構成

公開モジュールは大きく 3 層に分かれています.

**Algebra 層 (基盤):**

- `ExchangeAlgebra.Algebra` — 中核の代数 (`Alg` 型, 加算 `.+`, hat `.^`, bar `.-`, 射影 `proj`)
- `ExchangeAlgebra.Algebra.Base` — 基底クラス (`BaseClass`, `HatBaseClass`, `ExBaseClass`)
- `ExchangeAlgebra.Algebra.Base.Element` — `Element` 型クラス (wildcard 対応の基底要素)
- `ExchangeAlgebra.Algebra.Transfer` — 振替の書換 (`TransTable`, `(.->)`, `transfer`, `finalStockTransfer`)

**Journal 層 (メタデータ付き仕訳):**

- `ExchangeAlgebra.Journal` — `Journal n v b` 型 (Note を持つ仕訳), `sigmaOn`, `filterByAxis`, `projWithNote` など
- `ExchangeAlgebra.Journal.Transfer` — Journal 向けの振替 API

**Simulate / IO 層:**

- `ExchangeAlgebra.Simulate` — `StateSpace`, `Updatable`, `runSimulation`, ディスクへの spill, 波及効果 (`rippleEffect`, `leontiefInverse`)
- `ExchangeAlgebra.Simulate.Visualize` — Chart/Cairo ベースの PNG 描画 (使い分けは README 参照)
- `ExchangeAlgebra.Write` — CSV 出力と spill 復元

詳細とインポートの典型パターンは [README](https://github.com/yakagika/ExchangeAlgebra#readme) を参照してください.

# インストール

Stack の `extra-deps` に固定します.

~~~ yaml
# stack.yaml
extra-deps:
  - exchangealgebra-0.4.0.0
~~~

~~~ yaml
# package.yaml
dependencies:
  - exchangealgebra
~~~

GHC 9.10 (Stackage `lts-24.4` でテスト), Cabal 3.0 以降が必要です. `Chart` / `Chart-cairo` を経由して cairo / pango / freetype が必要なので, macOS では `brew install cairo pango` が要ります.

未リリースのリビジョンを使いたい場合は Git の特定コミットを `extra-deps` に指す方法もあります (詳細は README).

# サンプルの動かし方

`examples/` ディレクトリ以下に動くサンプルが揃っています. リポジトリ全体を clone してもよく, [degit](https://github.com/Rich-Harris/degit) で `examples/` だけ抜き出すこともできます.

~~~ bash
git clone https://github.com/yakagika/ExchangeAlgebra.git
cd ExchangeAlgebra
stack build
stack exec -- sim1        # 100 期シミュレーション
stack exec -- ebex1       # 入門用の簿記例
stack exec -- ripple      # 10 エージェントの波及効果
stack exec -- cge         # CGE モデル
~~~

サンプルのカタログと前提 (Python プロット用の uv 等) は [examples/README](https://github.com/yakagika/ExchangeAlgebra/blob/master/examples/README.md) にまとめてあります.

# このライブラリを使った研究

本ライブラリはプレプリント [*Accounting State Space as the Minimal Unit for Economic Agent-Based Modeling: Advancing Ripple Effect Analysis in Real-Time Economy*](https://doi.org/10.21203/rs.3.rs-8485050/v1) (Akagi, 2026, Research Square preprint v1) で会計計算をベースとした経済ABMを提案しています. 引用情報はリポジトリの `CITATION.cff` から GitHub の "Cite this repository" 経由で取れます.

# 今後

- **チュートリアル記事**: 上記プレプリントを正式公表したタイミングで, Exchange Algebra の代数構造と仕訳・振替・シミュレーションの組み立て方を解説する別記事を予定.
- **Haddock**: <https://htmlpreview.github.io/?https://raw.githubusercontent.com/yakagika/ExchangeAlgebra/master/haddock/index.html>

# ライセンス

MIT と Open World License (OWL) のデュアルライセンスです. 詳細はリポジトリの `LICENSE` を参照してください.
