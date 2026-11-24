---
title: exchangealgebra の manual を公開しました
description: Haskell ライブラリ exchangealgebra の利用者向け manual を公開しました. 英語版と和訳があり, Hackage の 0.6.1.1 を説明しています. 収録している範囲と, 初めて読むときの入口を紹介します.
tags:
    - haskell
    - exchangealgebra
    - accounting
featured: false
category: research
date: 2026-11-24
tableOfContents: false
---

Haskell ライブラリ [`exchangealgebra`](https://hackage.haskell.org/package/exchangealgebra) の利用者向けの manual を公開しました.

- URL: <https://yakagika.github.io/ExchangeAlgebra/>
- 言語: 英語版と和訳があり, 頁の右上で切り替えられます.
- 版: Hackage の 0.6.1.1 を説明しています. manual に載せた program はすべてこの版で compile し, 出力が頁の記載と一致することを確かめています.

manual が扱うのは, ライブラリの 4 つの層 (代数, 仕訳帳, 会計, 入出力) です. 交換代数の定義と公理から始め, 1 件の取引 (普通預金から現金を引き出す取引) の仕訳を記録する program を組み立てます. その後は振替と仕訳帳, 複数の会計主体, 勘定科目の属性, 試算表から決算整理と財務諸表, 連結までの簿記の手続き, CSV からの読み込みと帳簿の書き出しへ進みます. シミュレーションの層と, リポジトリに同梱した examples の解説はまだ載せていません.

初めて使う場合は「引き出しを記録する」の章から読むのが早いです. この章では 1 つの program で, 要素, 基底, 冗長代数, 交換代数の順に構成要素を組み立て, 記録した仕訳を借方と貸方に分けて両者の一致を確かめます. 続く章は, この program の部品を差し替える方法を説明します. 関数ごとの説明は [Hackage の Haddock](https://hackage.haskell.org/package/exchangealgebra/docs/ExchangeAlgebra.html) にあります.

ライブラリの公開の経緯と版ごとの変更は, 以前の記事 [exchangealgebra を Hackage に登録](2026-05-26-exchangealgebra-on-hackage.html) と [exchangealgebra 0.5.0.0 と 0.4 系からの変更点](2026-09-04-exchangealgebra-0-5-0-0.html) に書きました.
