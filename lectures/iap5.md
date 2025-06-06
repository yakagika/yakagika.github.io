---
title: 代数プログラミング入門 Ch5 代数的データ型1
description: 資料
tags:
    - algebra
    - lecture
    - statistics
    - haskell
featured: true
katex: true
date: 2024-10-18
tableOfContents: true
previousChapter: iap4.html
nextChapter: iap6.html
---

# 代数的データ型(集合論的解釈)

Haskellのデータ型はすべて**代数的データ型**です. 代数的データ型には, **列挙型**,**直積型**,**直和型**があり,構文として**レコード構文**などが存在します.

代数的データ型は文字通り, 数学における代数の構造を参照にしたデータ型であり,代数的な定義と対応させることで様々なことが可能となります. 代数学を理解するためにはまず,集合論の基礎を理解している必要があります.ここでは,集合論と対応させる形で,代数的データ型とは何であるかを理解することを目指します.

::: warn
Haskellは代数学の一部である**圏論**と強い結びつきがあり,プログラムのデータ構造は圏論的に解釈することも可能となります. 特にHaskellの高度な機能, 多相型(ポリモーフィズム),モナド,状態系などは集合論的な理解よりも圏論的な理解のほうが適しています.
そこで,ここでは一旦集合論的に概要を把握し,後の章で圏論的な解釈を試みます.
:::

## 命題と条件式
集合論的に代数的データ型を解釈するにあたって,数理的な定義の記法に用いる演算子を導入します. 数理的な定義の内,そこで述べられた言説が,「真か偽のいずれかに分類可能とされるもの」を**命題**といい,条件が与えられた命題を**条件式**といいいます.

`x`に関する条件式を
$P(x)≔***$ や $Q(x)≔***$
と書き，`***`の部分に,命題が記述されます．

命題の記述には以下の論理演算子が用いられます．

- $\neg p(x):p(x)$ の否定

- $P(x) \lor Q(x)$： $P(x)$または$Q(x)$

- $P(x) \land Q(x)$：P(x)かつQ(x)

- $P(x) \Rightarrow Q(x)$：$P(x)$ならば$Q(x)$

- $P(x) \Leftrightarrow Q(x) ∶$ $P(x)$ならば $Q(x)$ かつ $P(x)$ ならば $Q(x)$

このとき,
$p(x) \Rightarrow q(x) \Leftrightarrow \neg p(x) \lor q(x)$ となります.


memo: 全称命題と存在命題 (あとで出てきたときに書く)


## 集合

Haskellではデータ型を集合と**みなすこと**ができます. Haskellの型はあくまで型であり,厳密には集合ではありません. また,このあと出てくるリストを使った`内包表記`などの**集合論的な書き方**も数学における集合ではありません.
あくまで類似したものです.

しかし,Haskellを集合とみなすことで,関数型プログラミングや,代数的データ型の意味がより直感的に理解できるようになります. しばらく,集合論とHaskellの対応について考えてみましょう.

::: note
特定のモノがそこに｢属するか判定可能なモノの集まり｣を｢集合｣という．
:::

集合の細かな定義は置いておいて,この講義では取り敢えずこのくらいの認識で問題ありません. しかし,ただのモノの集まりではなく,特定のモノがそこに属するかどうかを判定できる必要があるので注意が必要です.

例えば, ｢頭の良い人の集合｣のようなものは,｢頭が良い基準｣が人によって異なるので,集合とはみなせません.

ノーベル賞受賞者の集合,フィールズ賞受賞者の集合,メンサ会員の集合,XX模試の偏差値が70以上の人の集合,特定の科目で85点以上取った人の集合,など,誰でも判別可能な定義が必要です.

私が過去に飼ったことのある犬の種類の集合を`MyDogs`という名前で呼ぶと,`MyDogs`に属するモノたちを`{ }`を使って以下のように書くことができます.


\begin{align*}
MyDogs = & \{ GoldenRetriever \\
         &, BlackRetriever    \\
         &, ShetlandSheepdog \\
         &, StandardPoodle \\
         &, StandardPoodle \}
\end{align*}


このとき,`GoldenRetriever`や,`ShetlandSheepdog`は`MyDogs`の`要素`であるといい,要素が特定の集合に属するとき,

$$ GoldenRetriever \in MyDogs $$ の様に書きます. 要素に属さないことは $Chihuahua \notin MyDogs$と書きます.

Haskellにおいて,このようなデータ型を以下の様に定義することが可能です.
データ型の宣言は, `data`のあとに続いて,`データ型の名前(型構築子)`を書き,`=`の後ろにその`中身(コンストラクタ/データ構築子)`を書きます.
型構築子やデータ構築子は,大文字の英字で始めるのが規則です.

~~~ haskell
data MyDogs = GoldenRetriever
            | BlackRetriever
            | ShetlandSheepdog
            | StandardPoodle
            | StandardPoodle
            deriving Show
~~~

この様にそこに属する要素をすべて書き出す(列挙する)データ型を`列挙型`といいます.

::: warn

ちなみに,大文字の英字で始まってさえいればUTF-8の文字や絵文字,記号は使用できるので,以下のような記述も可能ですが,あまりおすすめしません.

~~~ haskell
data My🐶   = Pゴールデンレトリーバー
            | Pブラックレトリーバー
            | Pシェットランドシープドッグ
            | Pスタンダードプードル
            | Pビーグル
            deriving Show
~~~
:::

`deriving Show`はコンストラクタを文字列に変換する関する`show`を自動で導入するための記法です. 自分で定義することも可能ですが,詳細に関しては後ほど扱います.

`deriving Show`を入れていない状態で

~~~ haskell
print GoldenRetriever
~~~

などを実行すると,以下のエラーがでますが,`deriving Show`を追加することで,表示することが可能となります.

~~~ haskell
ghci> :{
ghci| data MyDogs = GoldenRetriever
ghci|             | BlackRetriever
ghci| :}
ghci> print GoldenRetriever

<interactive>:17:1: error: [GHC-39999]
    • No instance for ‘Show MyDogs’ arising from a use of ‘print’
    • In the expression: print GoldenRetriever
      In an equation for ‘it’: it = print GoldenRetriever
ghci> :{
ghci| data MyDogs = GoldenRetriever
ghci|             | BlackRetriever
ghci|             deriving Show
ghci| :}
ghci> print GoldenRetriever
GoldenRetriever
~~~

なお, `print`の[実装](https://hackage.haskell.org/package/base-4.19.1.0/docs/src/System.IO.html#print)は

~~~ haskell
print :: Show a => a -> IO ()
print x = putStrLn (show x)
~~~

となっています.


要素が一つも属さない集合を`空集合`といい,記号$\phi$ または$｛｝$によって表されます．
Haskellでは空集合を表すデータ型として`Data.Void`に定義された`Void`が存在します. データ型として`ボトム型`,記号では`⊥`で表される場合もあります.

`Void`と同じ値を持たないデータ型は,コンストラクタを記述しないことで自分で実装することもできます. 例えば私が犬を今までに一匹もかったことがなければ, $$ MyPet = \phi $$ となり,データ型としては以下のように定義されます. 値が存在しない空集合と対応していることが分かります.

~~~ haskell
data Mypet
~~~

`Void`型は値が存在しないため実行することはできませんが,コンパイルを通すことはできます. ただし,あまり実用する機会はないので,以下の部分は興味がある人だけ開いて読んでください.

::: note

- Voidの利用例 開く/閉じる

`Void`型を利用したコードを記述する方法はいくつかありますが, `undefined`した実装などが良く用いられます.
`undefined`は遅延評価を利用した値で,具体的な値や式の記述を省略することができます.
未実装の部分を含めたコードを取り敢えず部分的にコンパイルしてみたい場合や, エラー処理などで利用されます.

以下のコードはコンパイルは通りますが,実行時には`undefined, called`エラーが発生します.

~~~ haskell

somFunc :: Int -> Int
someFunc = undefined

main = print $ someFunc 1

~~~

`Void`型を利用するケースは非常に限定的ですが,値が無いことを明示的に示したい場合などに利用されます.

~~~ haskell
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE EmptyDataDeriving #-}

data Empty deriving Show

head' :: Show a => [a] -> Empty ->  a
head' []     e = case e of
head' (x:[]) _ = x
head' (x:xs) _ = x

main = print $ head' ([]::[Int]) undefined -- >>> undefined, called at
~~~
このコードでは, 明示的に`先頭の値`が存在しないことを`Empty`で表し,`EmptyDataDeriving`拡張で`undefined`を評価することでエラーを発生させています.

しかし,こういったパターンでは,以下の`error`による実装や,後に説明する`Maybe型`を利用するほうが一般的です.

~~~ haskell
head'' :: Show a => [a] ->  a
head'' []     = error "Empty List"
head'' (x:[]) = x
head'' (x:xs) = x

main = print $ head'' ([]::[Int]) -- practice: Empty List error, called at
~~~

:::

単一の要素だけが存在するデータ型として`Unit`型も準備されており,`()`のような空のタプルとして表されます.



 集合の表記法には，外延的表記及び内包的表記という2通りが存在する．外延的表記とは，集合Sに含まれる要素を全て記述する方法で，x,yを要素とする集合を，
S={x,y}
と書く．集合には順番は関係ないため，{x,y}={y,z}である．また，一つの集合に同じ要素は2つ以上属することができず，{x,x}のような集合は定義できない．

 内包的表記とは，その集合に何が属するのかを定義する方法で集合Sに属する要素の集合をｘとすると，ｘがどの集合の要素であるか，どのような条件を持つかなどによって表記する．xの属する集合をX，条件式p(x)とすると，内包的表記では
S={x│x∈ X,p(x)}
と書かれる．また，内包表記において，関数や定数を定義することも許されており，
関数をf[x]で表すと，
S={f(x)|x∈X,f(x)=x+1}
のように表記される．
 条件の例として，R^+を非負の実数としたとき，R^+５以下の非負の実数を，以下のように書く．
{x|x∈R^+,x≤5}
集合には，集合が属することも可能で，集合SがTに属するときS∈ Tが成り立つ．
また，集合Sの要素を幾つか取り出した集合TをSの部分集合といい，
T⊂S
と表記される．
S={x,y,z}のとき，Sの部分集合は
{x},{x,y},{x,z},{z,y},{x,y,z},ϕ
となる．任意の集合Sに対して
ϕ⊂S
は成り立つ．
また，集合Sの部分集合全体の集合を冪集合といい，pow[S]または2^S と書く．
pow[{x,y,z}]={{x},{x,y},{x,z},{z,y},{x,y,z},ϕ}

## 型注釈と関数






### 内包表記

## 包含

## 積と和

# 代数とクラス

## マグマ

## 半群

## モノイド

## 群

## リスト

## ツリー

## ネットワーク

# 発展:交換代数


yakagika