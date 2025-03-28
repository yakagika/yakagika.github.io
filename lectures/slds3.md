---
title: 特別講義DS Ch3 Pythonことはじめ
description: 資料
tags:
    - datascience
    - statistics
    - python
featured: true
date: 2024-03-29
tableOfContents: true
previousChapter: slds2.html
nextChapter: slds4.html
---

# Pythonことはじめ

この章では,Pythonの基礎の基礎を学習します.
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

CLI(PowerShellやTerminal)を開いて,`python`とだけ打ってEnter Keyを入力するとPythonのREPLが立ち上がります.

~~~ sh
~/Desktop
python
Python 3.10.11 (v3.10.11:7d4cc5aa85, Apr  4 2023, 19:05:19) [Clang 13.0.0 (clang-1300.0.29.30)] on darwin
Type "help", "copyright", "credits" or "license" for more information.
>>>
~~~

プログラムを書いてEnter Keyを押すとその行のプログラムが実行されます.

~~~ python
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

~~~ python
>>> # これはコメント何も起きない
>>>
~~~

## データ型

第1回の検査に関する説明で軽く触れましたが, プログラミングで扱うデータはコンピュータのメモリ上に数値の羅列として存在します. それらの数値に人間が解釈可能な意味を与えたものをデータ型といいます.

Pythonで最初から準備されているデータ型(**組み込み型**)には以下のようなものがあります.
詳細はこのあと順番に見ていきますので, こんなものがあるということだけ,頭に入れておきましょう.

::: note
Pythonのデータ型(一部)

- 数値型:数値を表す
    - 整数(`int`)
    - 浮動小数点数(`float`)
    - 複素数(`complex`)

- 文字列型(`str`):文字を表す

- リスト(`list`):いくつかのデータをまとめたもの

- タプル(`tuple`):データの組み合わせ

- 辞書型(`dict`):`key`と`value`からなる辞書を表す

- 真偽値(`bool`):正しいか正しくないかなどを表す
:::



### 数値型(Pythonを電卓として使う)

REPLの動きに慣れるために,電卓で行うような簡単な計算をREPL上で行いましょう.
電卓で行う計算なので,ここで扱うデータ型は数値型になります.
Pythonにおける数値型には整数を表す`int`,小数を表す`float`,複素数を表す`complex`があります. 本資料では,その内`int`と`float`について扱います.

基本的には`1`や`100`のような整数を書けば`int`型として認識され,`1.0`や`100.3`のように小数点をつけると`float`型として認識されます.


~~~ python
>>> 1
1
>>> 1.0
1.0
~~~

Pythonでは`type()`の丸括弧の中にデータを記述することで特定のデータのデータ型を確認することができます.

~~~ python
>>> type(1)
<class 'int'>
>>> type(1.0)
<class 'float'>
~~~

整数や後述の文字など変換可能なデータ型から,`int`型へ変換するには`int()`の中に,そのデータを書きます. `float`型に変換するには,`float()`を使います.`float`から`int`への変換では,小数点以下が切り捨てられます.


~~~ python
>>> int(4.9)
4
>>> float(2)
2.0
>>> float("2")
2.0
~~~

Pythonでは複数の数値型が混ざった計算に対応しています. 基本的に,特定の計算で複数の数値型が混じっている場合には, `int`型は`float`型に,`float`型は`complex`型に自動で拡張されるので本講義の範囲ではそれほど意識する必要はありません.

簡単な計算に用いる記号は以下のとおりです.

|  計算      | 記号      |
| ------     | ----      |
| 足し算     | `+`       |
| 引き算     | `-`       |
| 掛け算     | `*`       |
| 割り算     | `/`       |
| 整数除算   | `//`      |
| 剰余(余り) | `%`       |
| 累乗       | `**`      |
| 絶対値     | `abs()`   |
| 整数変換   | `int()`   |
| 小数変換   | `float()` |

実際の計算は以下のようになるはずです.

計算は普通の電卓と同じような感覚で使えます.

~~~ python
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
~~~

計算には順序があり丸括弧 `()`で囲うことで計算する順番を変えることができます.

~~~ python
>>> (50 - 5 * 6) / 4
5.0
>>> 50 - 5 * 6 / 4
42.5
~~~


マイナスの数は数値の前に`-`をつけます

~~~ python
>>> 10 + -5
5
~~~

割り算の商と余りは`//`や,`%`で計算できます.

~~~ python
>>> # 整数除算(余りを表示しない)
>>> 6 // 4
>>> 1.0
>>> # 剰余(余り)
>>> 17 % 3
2
>>> 5 * 3 + 2
17
~~~

同じ数字をX回掛けたものを累乗といい,`**`といいます. `2 ** 3 = 2 * 2 * 2`

~~~ python
>>> # 累乗
>>> 2 ** 2
4
>>> 2 ** 3
8
~~~


`abs()` の中に数値を入れることで絶対値が計算できます

~~~ python
>>> abs(4)
4
>>> abs(-4)
4
~~~



::: note

**演習**

以下の計算をREPLを使って自分でしてみましょう.
Pythonの計算になれることが目的ですので,どのように計算したかを説明できるようにしましょう.

- 飴が40個あります.7人で同じ数ずつ分けると1人分は何個で何個あまりますか?

- 底辺5cm,高さ4cmの三角形の面積はいくつですか?

- 2の8乗はいくつですか?

- 累乗と掛け算の計算順序を丸括弧を使った計算で確かめてください.

:::

## 変数と代入



値に名前をつけることを`代入`といい,名前のついた値を`変数`といいます. `=` の左側に付けたい名前,右側に値を書きます.

プログラムでは,データはコンピュータの記憶領域(メモリ)に格納されています. メモリは,1バイト(8bit, 1bit は0/1の値)ごとにアドレスという連番がついています.


![memory null](/images/memory-null.png)

~~~ python
>>> x = 10
~~~

変数を宣言するということは,このアドレスにデータを割り当てることを意味します.
`x = 10`という変数が4バイト利用するとしたら以下のようにアドレス 201 から204 に 10という数字(int)を割り当てるイメージです.

![memory 10](/images/memory-10.png)

`x`のアドレスは`id(x)`で確認できます.

~~~ python
>>> id(x)
4438639120
~~~

`y = x` と変数に別の名前を宣言する場合は,同じ場所が参照されます.

~~~ python
>>> y = x
>>> id(x)
4438639120
>>> id(y)
4438639120
~~~

![memory 10](/images/memory-10y.png)

`x=15`と再度代入する(再代入)と,新しくメモリが確保されます.
その際に,`x`をコピーしていた`y`の値も変わることに注意しましょう.

~~~ python
>>> x = 15
>>> id(x)
4438639280
>>> id(y)
4438639280
~~~

![memory 15](/images/memory-15.png)

数値の場合には`y`に再代入しても,`x`の値は変わりません.

~~~ python
>>> x = 10
>>> y = x
>>> id(x)
4438639120
>>> id(y)
4438639120
>>> y = 15
>>> y
15
>>> x
10
>>> id(x)
4438639120
>>> id(y)
4438639280
~~~

この挙動は,後に出てくる配列では,異なるので注意が必要です.


::: note
変数に名前をつける際には,以下の点に注意しましょう.

- 小文字で始まる英数字を使う

- 複数の単語を使うときは`_`(アンダーバー)でつなげる

- 英語で名前をつける
    - `nagasa` ではなく `length`
    - `namae` ではなく `name`

- 長くなっても良いので他の人が見たときに意味がわかる名前をつける
    - 三角形の高さを表したいとして
        - `x`や`h`などの一文字よりも
        - `height`のほうが良い
        - 他にも高さを表す変数が登場するなら
        `triangle_height`のほうがより分かりやすい

~~~ python
>>> # 長方形の面積を求める
>>> width = 20
>>> height = 5
>>> area = width * height
>>> area
900
>>> #定義されていないものはエラーがでます
>>> space
Traceback (most recent call last):
 File "<stdin>", line 1, in <module>
NameError: name 'space' is not defined
~~~
:::

::: note
**演習**

- 変数を利用して以下の猫型ロボットのBMIを計算してください
    - BMI = 体重(kg)÷身長(m)の2乗
    - 猫型ロボットの身長 129.3cm
    - 猫型ロボットの体重 129.3kg
:::

### 代入演算子

代入は`=`でつなげる以外にもいくつかのパターンがあります.
まずは,普通に変数に数値を代入してみます.

~~~ python
>>> x = 1
>>> x
1
>>> x = 2
>>> x
2
~~~

このように具体的な値を`=`の右側に記入するのは直感的に分かりやすいのですが,pythonのプログラムを見ていると,左右に同じ変数名が登場する場合があります.

~~~ python
>>> x = 1
>>> x = x + 1
>>> x
2
~~~

これは,**右側に登場する**`x`は過去の`x`を表しており,**左側に登場する**`x`は,過去の`x`を利用して作られた新しい`x`であると解釈しましょう.

上の例では,`x=1`という過去の変数を使って, `x(=1) + 1`という新しい`x`を作っています.
これは**自己代入**と呼ばれ,数学や関数型言語における**再帰**とは異なり,手続き型言語独特の記法なので注意しましょう.

足し算`+`を使った自己代入は省略して, `x += 1`のように書けます. これは `x = x + 1`の省略形で**複合代入演算子**といいます.

同様に,引き算`-=`,掛け算 `*=`, 割り算 `/=`などの代入演算子も存在します.

~~~ python
>>> x = 10
>>> x -= 5
>>> x
5
>>> x *= 5
>>> x
25
>>> x /= 5
>>> x
5
~~~

## 文字列型

ここまでは,数値のみを扱ってきましたが,Pythonには数値以外にもいくつものデータ型が存在します.
次に, 文字を表す`文字列型(str)`の利用法について見ていきましょう.

文字列型は,文字を`""`(ダブルクオーテーション),あるいは`''`(シングルクォーテーション)で囲みます.

~~~ python
>>> "イヌ"
'イヌ'
>>> 'ネコ'
'ネコ'
>>> type('ネコ')
<class 'str'>
~~~

三連引用符`"""`で囲むことで複数行書くことができます.

~~~ python
>>> """ あ
... い
... う"""
'あ\nい\nう'
~~~

`\n`は改行を表しています.

~~~ python
>>> print('あ\nい\nう')
あ
い
う
~~~

### 文字列の演算

文字列は`+`で連結,`*`で反復させることができます.

~~~ python
>>> name = '太郎'
>>> '私は' + name  + 'です!'
'私は太郎です!'
>>> name * 3
'太郎太郎太郎'
~~~


変数を文字列の中で使いたいときには,上の例のように`+`で連結することもできますが,変数が文字列型ではないときには,`str()`を利用して文字列型に変換してから,結合する必要があります.

~~~ python
>>> cat_num = 10
>>> type(cat_num)
<class 'int'>
>>> #そのまま文字列と結合するとエラーが出る
>>> '私はネコを' + cat_num + '匹飼っています.'
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: can only concatenate str (not "int") to str
>>> #str()を利用して文字列に変換する
>>> '私はネコを' + str(cat_num) + '匹飼っています.'
'私はネコを10匹飼っています.'
~~~

また,変数の値を文字列の中で利用する場合には`str()`や`+`を利用して結合する以外にも`f'文字列'`という記法を利用することができます.文字列の前に`f`と書くと,文字列内の`{変数名}`の部分が変数の値に変更されます.

~~~ python
>>> f'私はネコを{cat_num}匹飼っています.'
'私はネコを10匹飼っています.'
~~~

`{変数名}`の部分には式を入れることも可能です.

~~~ python
>>> f'私はネコを{cat_num*10}匹飼っています.'
'私はネコを100匹飼っています.'
~~~

文字列が代入された変数の後ろに`[]`をつけて,番号を`[]`の中に入れると,指定した番号番目の文字が取得できます. このような`[]`で指定する数字を`index`といいます.

`Python`という文字に対して,indexは以下のように振られています.
`-`をつけて後ろから数えることもできます.

|文字列    |  P    |  y    |  t    |  h    |  o    |  n    |
| :---:    | :---: | :---: | :---: | :---: | :---: | :---: |
|前から    |  0    |  1    |  2    |  3    |  4    |  5    |
|後ろから  |  -6   |  -5   |  -4   |  -3   |  -2   |  -1   |

Pythonの文字列は,最初の文字を`0`番目と数えるので注意しましょう.

~~~ python
>>> word = 'Python'
>>> word[0]
'P'
>>> word[5]
'n'
>> word[-6]
'p'
~~~

インデックスの`[]`の中で,`[Start:End]`のようにはじめと終わりのインデックスを指定することで,文字列の1文字ではなく,部分的な文字列を取得することもできます. これを**スライス**といいます.

終わりは,一つ手前までになるので注意しましょう.

~~~ python
>>> word[0:2]
'Py'
>>> word[2:5]
'tho'
~~~

はじめか終わりのインデックスを省略すると,**以前/以降の全て**という意味になります.

~~~ python
>>> word[:4] #0から4まで
'Pyth'
>>> word[2:] #2から最後まで
'thon'
~~~



::: note
**演習**

- 演習1

`'abcdefg'` から `'cde'`をスライスで抜き出してください.

- 演習2

`x = 'abcdefg'`と定義して, xに操作を加えて`'abfg'`を作ってください.

- 演習3

`x = 'abcdefg'`と定義して, xに操作を加えて`'bbbeee'`を作ってください.

:::

## リスト

複数の値をまとめるデータ型の一種に**リスト型**があります. コンマで区切って角括弧の中に複数の値を書くことで,ひとまとまりのデータを作れます.
リストの中身一つ一つを**要素**といいます.

~~~ python
>>> squares = [1,4,9,16,25]
>>> squares
[1,4,9,16,25]
~~~

リストは文字列と同じ様に,インデックスやスライスで要素を取得できます.

~~~ python
>>> squares[0]
1
>>> squares[-1]
25
>>> squares[-3:]
[9,16,25]
~~~

### リストの演算

リストも`+`で連結,`*`で反復させることができます.

~~~ py
>>> [1,2,3] + [4,5,6]
[1,2,3,4,5,6]
>>> [1] * 3
[1,1,1]
>>> [1,2] * 3
[1,2,1,2,1,2]
~~~

リストはインデックスやスライスで指定した要素に値を再代入して変更することができます.

~~~ py
>>> animals = ['cat','dog','bird']
>>> animals[1] = 'mouse'
>>> animals
['cat','mouse','bird']
>>> animals[1:] = ['fish','pig']
>>> animals
['cat','fish','pig']
~~~

::: warn

リストは,数値などとは**変数に別の名前をつけたときの挙動が異なる**ので注意が必要です.

数値や文字列は,別の名前をつけた変数に再代入した場合もとの変数は,変更されません.

~~~ py
>>> cat = 'cat'
>>> cute_cat = cat
>>> cute_cat = 'cute_cat'
>>> cat
'cat'
>>> cute_cat
'cute_cat'
~~~

`cute_cat`を変更しても`cat`は変わりません. 代入の説明箇所で見たように,数値などの変数の場合は,`cute_cat`には`cat`の値が渡されており,値が同じ場合には同じアドレスを参照するが,値が変更された場合には,
新しいメモリが確保されます.

しかし,リストは別名の変数を変更すると元の変数の値も変更されます.

~~~ py
>>> animals = ['cat','fish','pig']
>>> species = animals
>>> species[0] = 'horse'
>>> animals
['horse', 'fish', 'pig']
>>> species
['horse', 'fish', 'pig']
~~~

`animal`の別名`species`を変更すると`animal`も変更されています. これは,配列などは新しい名称の変数に,値ではなくアドレスを渡していることによります.どちらの変数もずっと同じアドレスを参照しつづけるため,片方が変化すると同じアドレスを参照しているもう片方の値も変わります.

~~~ py
>>> id(animals)
4441753024
>>> id(species)
4441753024
>>> species[0] = 'cat'
>>> id(species)
4441753024
~~~

しかし,インデックスやスライスによる要素の変更ではなく,全体を再代入した場合には新しいアドレスが割り当てられます.

~~~ py
>>> species = ['a','b','c']
>>> id(species)
4441834752
~~~

同じ値を持つが異なる場所を参照するリストを作りたい場合には,`copy()`を利用します.

~~~ py
>>> animals = ['cat','fish','pig']
>>> species = animals.copy()
>>> id(animals)
4441764160
>>> id(species)
4441798208
>>> species[0] = 'dog'
>>> animals
['cat', 'fish', 'pig']
>>> species
['dog', 'fish', 'pig']
~~~

このような挙動は今後出てくる`pandas`などの配列でも同様なので,注意が必要です.

:::

`append()`というメソッドを使って,リストの末尾に要素を追加することができます.

~~~ py
>>> animals
['cat','fish','pig']
>>> animals.append('dog')
>>> animals
['cat','fish','pig','dog']
~~~

リストの長さ(要素数)を知りたい場合には,`len()`関数を利用します.

~~~ py
>>> animals
['cat','fish','pig','dog']
>>> len(animals)
4
~~~

::: warn
`メソッド`,`関数`という言葉が説明無しに突然でてきました.
これらの違いについて理解するには段階が必要なため,後に説明します.
ここでは,変数などの後ろに`変数.f()`の形で`.`を利用してつけるものを`メソッド`,単独で`f()`のように利用するものを`関数`ということだけ覚えておきましょう.
なお,いずれも`()`の中に値や変数を書いたり書かなかったりしますが,その意味についても後ほど扱います.
:::



リストはリストも要素にすることができます. このようなリストを**多重リスト**と呼びます.

~~~ py
>>> x = [1,2,3]
>>> y = [4,5,6]
>>> z = [x,y]
>>> z
[[1,2,3],[4,5,6]]
>>> z[1]
[4,5,6]
>>> z[0][1]
2
~~~

::: note
**演習**

`xs = [[1,2,3],[4,5,6],[7,8,9]]` というリストを作り,以下の操作を行ってください.

- `xs`の長さを求める

- スライスを使って以下を抽出する
    - `[[4,5,6],[7,8,9]]`
    - `[[1,2,3]]`
    - `[[7,8,9]]`
    - `[8,9]`

- `[4,5,6]`を`[-4,-5,-6]`に更新する

- `1` を `-1`に,`9`を`-9`にする

- `[7,8,-9]`のあとに,`[10,11,12]`を追加する

:::

## タプル

Pythonにはリスト以外にも複数のデータ型の組み合わせを表すデータ型が存在します. タプルは,データの組を表すデータ型であり,`()`の中に`,`で区切ってデータを入れることでリストのようにデータを格納することができます.
タプルも複数のデータをまとめることができます.

しかし, リストのように扱うことは推奨されません.
(後に扱う関数などで返り値を複数返したいときなど)基本的に2,3個のデータの組を扱いたい場合に利用して,3個以上のデータを扱い場合にはリストなどを使うようにしましょう.

~~~ py
>>> name_and_age = ('Taro',10)
>>> name_and_age
('Taro', 10)
~~~

タプルの値の取り出しには,同じ形のタプルに変数を格納することで値を取り出す`パターンマッチ`が良く利用されます.

~~~ py
>>> (name,age) = name_and_age
>>> name
'Taro'
>>> age
10
~~~


インデックスによる値の取得も可能です.

~~~ py
>>> name_and_age[0]
'Taro'
>>> name_and_age[1]
10
~~~


ただし,インデックスを利用した要素の変更はできません.

~~~ py
>>> name_and_age[0] = 'Hanako'
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
TypeError: 'tuple' object does not support item assignment
~~~

## 辞書型

名前とその意味, 商品と在庫数,日本語名と英語名など,特定のデータに対応する別のデータの組み合わせを沢山扱いたい場合には,`辞書型`(dict)が利用されます. 辞書型は`key`と`value`と呼ばれるデータの組み合わせからなります.

辞書型のデータは,`{key1:value1,key2:value2,...}`のように,`key`と`value`の組み合わせを`:`で表して,`{}`にコンマで区切るかたちで作成します.

例えば, 学生と学生の出席回数の組み合わせを表すデータは以下のように作成されます.

~~~ py
>>> attendance = {'Taro':10,'Hanako':12,'Kenta':9,'Shizuka':10}
>>> attendance
{'Taro': 10, 'Hanako': 12, 'Kenta': 9, 'Shizuka': 10}
~~~

`dict()`に`key`と`value`のタプルのリストを渡すことで生成する事もできます.

~~~ py
>>> attendance = dict([('Taro',10),('Hanako',12),('Kenta',9)])
>>> attendance
{'Taro': 10, 'Hanako': 12, 'Kenta': 9}
~~~

特定の`key`でそれに対応する`value`を呼び出すには,`key`をインデックスとして`[]`を利用します.

~~~ py
>>> attendance['Taro']
10
>>> attendance['Shizuka']
10
~~~

新しい`key:value`を加える場合にも,`value`を変更する場合にも,インデックスによる代入が利用できます.

~~~ py
>>> attendance['Taro'] = 11
>>> attendance['Taro']
11
>>> attendance['Shinzi'] = 12
>>> attendance['Shinzi']
12
~~~

`key`を削除するには`pop(消したいkey)`メソッドを利用します.

~~~ py
>>> attendance.pop('Hanako')
>>> attendance
{'Taro': 11, 'Kenta': 9, 'Shizuka': 10, 'Shinzi': 12}
~~~

辞書型のデータから,`key`のみ,`value`のみを抜き出すには,`keys()`,`values()`メソッドを利用します.
リストとして取得したい場合には,`list()`関数で囲みます.

~~~ py
>>> attendance.keys()
dict_keys(['Taro', 'Kenta', 'Shizuka', 'Shinzi'])
>>> attendance.values()
dict_values([11, 9, 10, 12])
>>> list(attendance.values())
[11, 9, 10, 12]
~~~

これらの処理はリストを利用しても可能ですが,辞書型のほうが計算が速いため辞書型を利用するようにしましょう. プログラミングにおいては用途に応じて適切なデータ型を選択することが重要です.

::: warn
同じような処理が可能なデータ型でも,その処理を実行するのにコンピュータが必要な計算の数や,使用するメモリの量,速度などに違いがあります.

この資料では扱いませんが,大規模なデータを扱う場合や,大量の計算を行うプログラムを書く際には,`計算量`を考慮して`アルゴリズムとデータ構造`を適切に設計する必要があります. 興味のある方は,調べてみましょう.
:::

::: note

**演習**

- 5種類の果物の日本語名と英語名を変換する辞書を作成し,実際に機能する様子を紹介してください.

- 上で作成した辞書にもう一つ果物を追加してください.


:::


## 論理演算

それが正しいか間違っているか判別できる文を**命題**といいます. 命題の結果を表すものとして真(正しい),偽(間違っている)という値を用います. 真と偽を併せて**真偽値**といいます.

例えば,`1は2より大きい`という命題は,間違っているので**偽**となります. `人間は必ず死ぬ`という命題は,今のところ不老不死の人間がいないので**真**です.

プログラミングではこのような命題の判断がしばしば必要となるため,それらを扱うデータ型が提供されています.

真偽値を表すデータ型として`Bool`があります. `Bool`は`True`(真),`False`(偽)のいずれかです.

Pythonには命題の判定を行う演算子として,以下のようなものが準備されています.

|記号       | 意味         |
| :---:     | :---:        |
| `>`       |  より大きい  |
| `>=`      |  以上        |
| `<`       |  より小さい  |
| `<=`      |  以下        |
| `==`      |  等しい      |
| `!=`      |  等しくない  |
| `in`      |  含む        |

数値などの大小関係を調べるときには,比較演算子 `>`,`>=`.`<`,`<=`を利用します. 演算子の左右に数値を書くと,結果に応じて真偽値が帰ってきます.

~~~ py
>>> 1 > 2
False
>>> 1 < 1.5
True
~~~

リストや文字列に特定の要素(文字列)が含まれているかは,`in`で判定できます.

~~~ py
>>> 'ab' in 'abcd'
True
>>> 1 in [3,4,5]
False
~~~

値が等しいか/等しくないかを判定するには,`==`と`!=`を利用します.

~~~ py
>>> 4 == 4
True
>>> 'cat' != 'cat'
False
~~~

これ以外にもPythonにはいくつかの演算が準備されていますし,自分で作ることも可能です.

`True` や `False`などの`Bool`値は, `AND`(かつ),`OR`(または),`NOT`という演算で計算することができます(`XOR`というのもあるが省略).
PythonではAND は `&`, OR は `|`, NOT は `not` という演算子が提供されています.

A,Bが命題だとして,`A & B`は両方`True`のときに,`True`となります. `A | B`は片方どちらかが`True`のときに`True`となります.

例えば,

- `1は2より大きい かつ 2は0より大きい` という命題は,`2は0より大きい`は正しいですが,`1は2より大きい`が間違っているので全体として,`False`です.

- `ネコは哺乳類である または ネコは鳥類である`という命題は `ネコは鳥類である`が間違っていますが全体としては`True`です.

演算の結果は,それぞれ以下のようになります. これを真偽値表といいます. ここでは,最低限の例だけを紹介しますが,より深く理解したい人は論理学などの講義を受講しましょう.

| 命題Aの値 | Bの値 | `A & B` | `A | B`|
| :---:     | :---: | :---:   | :---:  |
| True      | True  | True    | True   |
| False     | True  | False   | True   |
| True      | False | False   | True   |
| False     | False | False   | False  |

Pythonではそれぞれの命題を丸括弧で囲んで,`&`,`|`演算子で論理演算を行うことができます.


~~~ py
>>> (1 > 2)
False
>>> (2 > 0)
True
>>> (1 > 2) & (2 > 0)
False
>>> (1 > 2) | (2 > 0)
True
~~~

`not` は命題の否定を表しており `True`が`False`,`False`が`True`になります.`not`は命題の前に書きます.

~~~ py
>>> (1 > 2)
False
>>> not (1 > 2)
True
~~~

::: note

**演習**

ある値が偶数かどうかは,2で割った余りが0かどうかを判定することで判定できます.
`x=101`,`y=202`として, 以下の命題の真偽をPythonで計算してください.

- xが偶数
- yが偶数
- xが偶数かつyが偶数
- xが偶数またはyが偶数
- x + y が奇数

:::


## (復習)スクリプトの実行

これまでは,対話環境でプログラムを実行してきましたが,対話環境は複雑な処理には適しません.
これから,1行1行プログラムを記述して対話環境で実行するのではなく,複数行のプログラムをまとめて記述して一気に実行する方式に切り替えます. 複数行のプログラムを一つのファイルにまとめたものをスクリプトファイルと呼び, 書かれているプログラムをスクリプトといいます. 前回行った`Hello World`はスクリプトを実行していました.

::: note
**Pythonのスクリプト実行の方法**

- テキストファイルにプログラムを書く

- ファイルの拡張子を`.py`にして保存する
    - 講義で作成したプログラムは,あとで自分が参考にできる最高の資料です.
        - あとで何をやっているのか自分が理解できるように
        プログラムにはできるだけ沢山のコメントを付けましょう

        - ファイル名は,後からみて,中身が何であるかわかるよう`英数字`で名付けましょう
            - `a.py`や`file.py`,`課題.py`などはやめましょう

- Shell上でそのファイルが保存されている場所に移動する

- `python ファイル名` コマンドで実行する
    - ※ `python` は空白の後に続くプログラムを実行するためのコマンドです.
:::


::: note

**演習**

前回行った`Hello World`を参考に,`Let's start Python programming!!` と表示されるプログラムを作成しましょう. ファイル名などは適切に名付けてください.
まだ,今後沢山のスクリプトを書いていきますので,適切にフォルダなどを整理しましょう.

:::

## エラーへの対応法とよくある間違い

これまで皆さんは1行のプログラムを記述して, REPL上で実行してきました. これから何行かに渡るプログラムを記述すると, うまくできない人が出てきます. ここでは,学生のつまずきやすいポイントとその対策について,事前に学習しておきましょう.

### エラーへの対応方法

間違った手順,プログラムの記述方法でプログラムを実行すると,エラー文がTerminalに表示されます. どれだけプログラミングが得意な人でも, 完璧な作業はできません. 必ずエラーが発生します. そのような意味でも,プログラミングをするというのは,プログラムを書いて発生したエラーに対応するということでもあります.

エラーへの対応は, プログラミングにある程度習熟した人でも,Webなどで調べて解決する場合が多いです.PCやプログラミングができるということは,すべての場合をすべて記憶して対応できるということではなく,**問題が起きたら自分で解決できる**ということを意味しています.

したがって, まず必要なのは問題が起きたら解決策を**自分で調べること**です.

解決方法を調べるためには,検索するためのワードとして,**機能や名称を知っていること**が重要です.
例として, Excelのフィルター機能の存在を知っていれば, **Excel フィルター 使い方**などで検索することができます.しかし,Excelも,フィルターも知らなければ,調べることすらできません.

すべての概念や機能を最初から完全に理解する必要はありませんが,概念の存在や名称を覚えるようにしましょう. そのためにも自分の**メモやチートシートを作成,整理しておくこと**が重要です.

プログラミングの学習において, 何かが間違っている場合には, Terminalにエラー文が表示されます. Pythonはエラー文が親切なので,エラー文を読めば大抵のことは解決できるようになっています.

しかし,エラーが起きても**エラー文を一切読まない人**が一定の割合で存在します. そのような人に理由を尋ねると最も多い理由は**英語で書かれていること**,2番目に**どこを見ればいいのか分からないこと**を挙げます.

それほど難しい英語は使われていませんが,まず英語で書かれていても読んでみましょう. 英語が理解できなければ,機械翻訳にかけましょう.

pythonのエラー文は,基本的にエラー文の**一番最初にスクリプトのエラーが発生している場所**が書かれています.また,**一番最後にどのようなエラーが起きているのか**が書かれています. 複雑なプログラムになると,エラー全体を理解する必要が出てきますが,この講義で扱う程度の事例に関してはその2個所のみを読めばほとんどが解決します. ただし,検索結果は珠玉混合です, 正しい情報の取捨選択に関しては,情報入門の教科書などで復習しておきましょう.

![エラー文の読み方](/images/python-error-sample1.png)

![](/images/translate-error.png)

エラー文を読んでも意味が理解できない,あるいは対処方法が分からない場合には,エラー文の最後をそのまま検索しましょう. なお,Googleでは,`""`で囲うことで文章ごと検索(センテンス検索)できます. Pythonは日本語ユーザーも非常に多いため,`エラー文 Python`で検索すれば,大抵の問題は日本語で解決方法を読むことができます.

解決しない場合には,AND検索で情報を付加して,結果を絞りましょう. 検索条件に加えるべき情報の候補としては以下のようなものがあります.

::: note

- OS (Windows, Mac)

- Pythonのversion

- 利用しているライブラリ

- やろうとしている作業

:::

例えば,これから行う`pandas`を利用した`ファイルの読み込み`において`No such file or directory`というエラーが出た場合には `pandas ファイル読み込み "No such file or directory" Windows`などで検索してみましょう.

**エラー文を読んで**,**Webで検索しても**問題が解決しない場合には, 教員に聞いて下さい. 専門的な内容になるほど,日本語のページは少なくなります.また,新しい情報に関しては,日本語に翻訳されておらず公式のドキュメントなどを読む必要があります. それでも解決しない場合には, Pythonのコミュニティなどで質問をする必要があります. 最終的にはこれらを自分でできるようになる必要がありますが,最初は難しいと思います. 講義の教員は,それらを代替するためにいますので,教員に聞きましょう.

しかし,繰り返しになりますが,PCが使える,プログラミングができる,ということは自分で問題の解決策を調べて解決できるということです.したがって,まずは自分で調べて解決する癖をつけるようにしましょう.

### エラーを体験してみよう

皆さんのプログラムが上手く動かない理由の圧倒的No1が**スペルミス,タイプミス**です. プログラミングの作業は,プログラムもコマンドも英語で記述します. プログラムは,1文字でも間違っていると上手く動かないので,しっかりとタイピングしましょう. 特にこれから行う作業で非常に多いスペルミスは以下のようなものです.

::: note
- 学生のスペルミス例
    - Data → Date  (なぜか3割くらいの学生が間違えます.)
    - industry → indusutry, industly, indstry
    - python → pyton, pyhon
    - answer →  anser, answere, ansewer
    - salary → sarary, saraly, sarasly
    - python --version → python version, python --vertion
:::

スペルミスに対応するには注意するしかありません. 単純に英語の単語を覚えていない or タイピングミスが原因なので注意しましょう.英単語の意味がや綴がわからない場合には検索しましょう.
基本的にプログラミングにおいて無意味な英単語は利用していないので,意味を考えましょう(意味がない単語の例としてhoge,hugaなどは良く使いますが).特に`Date (日付)`, `Data(データ)`などは頻出ですが, 単語の意味を考えればミスしづらいかと思います. また,エラー文を読めばどこが間違っているか教えてくれています.
エラー文にでてきた文字列’industry’や’Data’に該当する部分が間違っていないかチェックしましょう.

事例として,以下のプログラムの実行結果と,エラーについて見てみましょう. なお,プログラムの内容や詳細に関しては,このあとやるので理解できなくても問題ありません.

プログラムを実行するにあたって作業ディレクトリに以下のプログラム`error_sample.py`とプログラム内で読み込むデータ`data/error_sample.csv`が存在することを前提とします. ここでは,あくまで事例として紹介するので皆さんはデータとプログラムを用意する必要はありません.


::: warn

やる必要はありませんが,同じ作業を試してみたい場合は,作業ディレクトリで以下のコマンドをコピーして実行しましょう

- Windowsの人

~~~ sh
pip install pandas
echo "name,salary\ntaro,100" > data/error_sample.csv
echo "import pandas as pd\ndf = pd.read_csv('data/error_sample.csv') \nprint(df['salary'])" > error_sample.py
~~~

- Macの人

~~~ sh
pip3 install pandas
echo "name,salary\ntaro,100" > data/error_sample.csv
echo "import pandas as pd\ndf = pd.read_csv('data/error_sample.csv') \nprint(df['salary'])" > error_sample.py
~~~


ファイルの構成が以下のようになっていれば問題ありません.

~~~ sh
❯ ls
error_sample.py
❯ ls data
salary_data.csv
~~~

それぞれ,以下のようなファイルができているはずです(コメントは入っていません).

- error_sample.py

~~~ python
import pandas as pd
#dataフォルダにある,error_sample.csvファイルを読み込み
df = pd.read_csv('data/error_sample.csv')
#読み込んだファイルのsalary列を表示
print(df['salary'])
~~~

- error_sample.csv

~~~ python
name,salary
taro,100
~~~

:::

このプログラムを実行してみると,`error_sample.csv`の`salary`列の値が表示されます.

~~~ sh
❯ python error_sample.py
0    100
Name: salary, dtype: int64
~~~

プログラムを以下のように修正して実行してみます.

~~~ python
import pandas as pd
# error_sample.csvをarara_sample.csv に変更
df = pd.read_csv('data/arara_sample.csv')
print(df['salary'])
~~~

以下のようなエラーが表示されます.

~~~ sh
❯ python3 error_sample.py
Traceback (most recent call last):
  File "/Users/akagi/Documents/Programs/Python/slds/error_sample.py", line 2, in <module>
    df = pd.read_csv('data/arara_sample.csv')
  File "/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/pandas/io/parsers/readers.py", line 912, in read_csv
    return _read(filepath_or_buffer, kwds)
  File "/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/pandas/io/parsers/readers.py", line 577, in _read
    parser = TextFileReader(filepath_or_buffer, **kwds)
  File "/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/pandas/io/parsers/readers.py", line 1407, in __init__
    self._engine = self._make_engine(f, self.engine)
  File "/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/pandas/io/parsers/readers.py", line 1661, in _make_engine
    self.handles = get_handle(
  File "/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/pandas/io/common.py", line 859, in get_handle
    handle = open(
FileNotFoundError: [Errno 2] No such file or directory: 'data/arara_sample.csv'
~~~

先ほど解説したように最初の部分と,最後の部分だけを見てみましょう.
最初の部分ではエラーの発生場所を説明しています. このエラーは
`line 2, in <module>`の`df = pd.read_csv('data/arara_sample.csv')`
部分で発生しています.先ほど変更を加えた2行目のファイル名の部分ですね.

最後の部分では,発生したエラーの中身について説明しています. エラーの詳細は

`FileNotFoundError: [Errno 2] No such file or directory: 'data/arara_sample.csv'`

であり,`data`フォルダに`arara_sample.csv`というファイルがないという意味です.

プログラムの`arara_sample.csv`の部分を修正して,今度は,最後の行をエラーが出るように変更しています.

~~~ python
import pandas as pd
#dataフォルダにある,error_sample.csvファイルを読み込み
df = pd.read_csv('data/error_sample.csv')
#salaryをsararyに変更
print(df['sarary'])
~~~

実行すると以下のようなエラーが発生します.

~~~ sh
❯ python3 error_sample.py
Traceback (most recent call last):
  File "/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/pandas/core/indexes/base.py", line 3652, in get_loc
    return self._engine.get_loc(casted_key)
  File "pandas/_libs/index.pyx", line 147, in pandas._libs.index.IndexEngine.get_loc
  File "pandas/_libs/index.pyx", line 176, in pandas._libs.index.IndexEngine.get_loc
  File "pandas/_libs/hashtable_class_helper.pxi", line 7080, in pandas._libs.hashtable.PyObjectHashTable.get_item
  File "pandas/_libs/hashtable_class_helper.pxi", line 7088, in pandas._libs.hashtable.PyObjectHashTable.get_item
KeyError: 'sarary'

The above exception was the direct cause of the following exception:

Traceback (most recent call last):
  File "/Users/akagi/Documents/Programs/Python/slds/error_sample.py", line 3, in <module>
    print(df['sarary'])
  File "/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/pandas/core/frame.py", line 3761, in __getitem__
    indexer = self.columns.get_loc(key)
  File "/Library/Frameworks/Python.framework/Versions/3.10/lib/python3.10/site-packages/pandas/core/indexes/base.py", line 3654, in get_loc
    raise KeyError(key) from err
KeyError: 'sarary'
~~~

今度のエラー文を見てみると,先ほど変更を加えた

`File "/Users/akagi/Documents/Programs/Python/slds/error_sample.py", line 3, in <module>`

の  `print(df['sarary'])`でエラーが発生しており,エラーの内容は,`sarary`という`Key`が存在しないという意味の`KeyError: 'sarary'`です. pandasのDataFrameにおけるKeyについてはまだ扱っていませんが,辞書型で発生するエラーと同様なので,辞書型を参考にして大まかな意味を掴みましょう.

~~~ python
>>> xs = {"name":"taro","salary":100}
>>> xs['salary']
100
>>> xs['sarary']
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
KeyError: 'sarary'
~~~
上のコードでは,`sarary`という`xs`に存在しない`key`を呼び出したことで`KeyError`が発生しています.

::: note

- 演習

以下のプログラムをコピーして保存・実行し,エラーを確認しましょう.
どのようなエラーが含まれているのか,エラー文を読んで修正し,説明してください.

- error_sample2.py

~~~ py
#Morningの"Good"部分を抽出したい.
#わざとエラーを含むプログラム
greetings = {"Morning":"Good Morning"
            "Noon":"Hello"
            ,"Night":"Good Night"}

  print(greeting["Morming"]["0":"4"]
~~~

:::


