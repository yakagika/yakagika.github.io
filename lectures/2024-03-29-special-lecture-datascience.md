---
title: 特別講義(データサイエンス)
description: 資料
tags: datascience, lecture, statistics, python
featured: true
tableOfContents: true
---


(執筆準備中)

# 第2回 Pythonの基礎

第2回では,Pythonの基礎の基礎を学習します.
本講義はプログラミング自体の学習を対象としているわけではないので,
講義内で必要な技能を紹介するのみに留めます.

基本的には,公式の[Pythonチュートリアル](https://docs.python.org/ja/3/tutorial/index.html)の内容で必要十分ですので,そちらにそって学習を進めます.
より詳しい内容を勉強したい場合には,プログラミングの講義を履修するか,
[千葉商科大学IEEESB](http://cucieee.stars.ne.jp/index.html)
などに参加するのが良いかと思います.

今回は取り敢えず,以下の作業を自分で行いながら,Pythonの体験をしてみましょう.

## REPLを使ってみよう

プログラムの対話環境全般をREPL (Read Eval Print Loop)と呼びます. 長いプログラム(スクリプト)を書かなくても対話的にプログラムを実行することができます.

Pythonには複数のREPLがあり,iPython, jupyternotebook, Google Colaboratoryなどがあります.ここではとりあえず,iPythonを利用します. 他のものについても,後ほど出てきます.

CLI(PowerShellやTerminal)を開いて,`python`(Mac の人はPython3)とだけ打ってEnter Keyを入力するとPythonのREPLが立ち上がります.

~~~ sh
~/Desktop
python3
Python 3.10.11 (v3.10.11:7d4cc5aa85, Apr  4 2023, 19:05:19) [Clang 13.0.0 (clang-1300.0.29.30)] on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>>
~~~

プログラムを書いてEnter Keyを押すとその行のプログラムが実行されます.

~~~ sh
>>> 1 + 1
2
~~~


終了するには,`exit()`と入力します.

~~~ sh
~/Desktop 1m 7s
Python 3.10.11 (v3.10.11:7d4cc5aa85, Apr  4 2023, 19:05:19) [Clang 13.0.0 (clang-1300.0.29.30)] on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>> 1 + 1
2
>>> exit()
~/Desktop 1m 7s
~~~

## コメントアウト

REPL上でもスクリプトでも,\#を先頭につけると,その行はコメントとして扱われます.

ただのコメントであり,プログラムとしては何も実行されません.

~~~ sh
>>> # これはコメント何も起きない
>>>
~~~

## Pythonを電卓として使う

REPLの動きに慣れるために,簡単な計算をREPL上で行いましょう.
簡単な計算に用いる記号は以下のとおりです.


|  計算      | 記号 |
| ------     | ---- |
| 足し算     | `+`  |
| 引き算     | `-`  |
| 掛け算     | `*`  |
| 割り算     | `/`  |
| 整数除算   | `//` |
| 累乗       | `**` |
| 剰余(余り) | `%`  |

実際の計算は以下のようになるはずです.

~~~ sh
>>> # 計算は普通の電卓と同じような感覚で使えます.
>>> # 足し算
>>> 1 + 1
2
>>> # 引き算
>>> 10 - 5
5
>>> # 掛け算
>>> 2 * 3
6
>>> # 割り算
>>> 100 / 5
20.0
>>> # 整数除算
>>> 6 / 4
>>> 1.0
>>> # カッコも使えます
>>> (50 - 5 * 6) / 4
5.0
>>> 50 - 5 * 6 / 4
42.5
>>> # 剰余(余り)
>>> 17 % 3
2
>>> 5 * 3 + 2
17
>>> # 累乗
>>> 2 ** 2
4
>>> 2 ** 3
8
~~~

### 練習問題

以下の計算をREPLを使って自分でしてみましょう.
Pythonの計算になれることが目的ですので,どのように計算したかを説明できるようにしましょう.

- 飴が40個あります.7人で同じ数ずつ分けると1人分は何個で何個あまりますか?

- 底辺5cm,高さ4cmの三角形の面積はいくつですか?

- 一片が5cmの正立方体の体積はいくつですか?

## 代入


特別講義(データサイエンス)の授業資料などを書いてく予定です.

yakagika