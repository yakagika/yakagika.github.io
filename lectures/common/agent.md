---
title: 共通資料 コーディングエージェントの利用
description: 資料
tags:
    - programming
    - lecture
featured: false
date: 2026-09-04
open: false
tableOfContents: true
previousChapter: git.html
---

本資料は複数の講義で共通に使う資料です. コーディングエージェントに課題のプログラムを書かせ, その結果を読んで直せるようになるために必要な環境と操作, および判断の仕方を扱います.

前提として[共通資料 プログラミング用の設定](setup.html)と[共通資料 バージョン管理とGitHub](git.html)を先に読んでください.

**大規模言語モデルの仕組みはここでは扱いません.** Transformer, 注意機構, 自己教師あり学習といった内部の話は[第8章 ニューラルネットワークから生成AIへ](dsp8.html)で扱います.

# ハーネス: エージェントが動く環境

(執筆中)

# 環境構築

この講義では **codex** (エージェント本体) と **herdr** (エージェントを動かすターミナル) の 2 つを入れます. git と GitHub CLI は[共通資料 バージョン管理とGitHub](git.html)で先に入れておいてください.

## codex を入れる

codex は OpenAI が配布しているコーディングエージェントです.

Windows は [共通資料 バージョン管理とGitHub](git.html)で使った winget で入ります.

~~~ powershell
winget install -e --id OpenAI.Codex
~~~

macOS は Homebrew で入ります.

~~~ bash
brew install codex
~~~

入ったことを確認します.

~~~ bash
codex --version
~~~

## codex にログインする

codex は自前の課金を持たず, ChatGPT のアカウントを使います.

~~~ bash
codex login
~~~

ブラウザが開くので, ChatGPT にログインします.

::: warn

**この講義では ChatGPT Plus (月 20 ドル) を 3 ヶ月間契約してもらいます.** 無料プランでも codex は動きますが, 数回のやりとりで上限に達します. 契約の時期は講義中に案内します.

:::

## herdr を入れる

herdr は, エージェントを動かすためのターミナルです. 複数のエージェントを別々の作業場所で同時に動かせます ([共通資料 バージョン管理とGitHub](git.html) の worktree の節を参照).

Windows は PowerShell で次を実行します.

~~~ powershell
powershell -ExecutionPolicy Bypass -c "irm https://herdr.dev/install.ps1 | iex"
~~~

macOS と Linux は次を実行します.

~~~ bash
curl -fsSL https://herdr.dev/install.sh | sh
~~~

::: note

**herdr は winget では入りません.** winget を検索すると `hdosys.herdr-win` という項目が出てきますが, これは herdr の開発元ではない第三者が配布しているものです. 上のコマンドを使ってください.

Windows のセキュリティ製品が上のコマンドを止めることがあります. その場合は [herdr.dev](https://herdr.dev/docs/install/) から `install.cmd` をダウンロードして実行する手順が用意されています.

:::

入ったことを確認します.

~~~ bash
herdr --version
~~~

# 操作の最小セット

(執筆中)

# AGENTS.md: リポジトリの決まりごとを読ませる

(執筆中)

# skill: 繰り返す手順を切り出す

(執筆中)

# 分からないまま承認しない

(執筆中)

# AIの開発環境と実行環境

(執筆中)

# 発展: LLMで学習する

(執筆中)
