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
previousChapter: fp4.html
nextChapter: fp6.html
open: false
---

# 代数的データ型(集合論的解釈)

Haskellのデータ型はすべて**代数的データ型**です. 代数的データ型には, **列挙型**,**直積型**,**直和型**があり,構文として**レコード構文**などが存在します.

代数的データ型は文字通り, 数学における代数の構造を参照にしたデータ型であり,代数的な定義と対応させることで様々なことが可能となります. 代数学を理解するためにはまず,集合論の基礎を理解している必要があります.ここでは,集合論と対応させる形で,代数的データ型とは何であるかを理解することを目指します.

::: warn
Haskellは代数学の一部である**圏論**と強い結びつきがあり,プログラムのデータ構造は圏論的に解釈することも可能となります. 特にHaskellの高度な機能, 多相型(ポリモーフィズム),モナド,状態系などは集合論的な理解よりも圏論的な理解のほうが適しています.
そこで,ここでは一旦集合論的に概要を把握し,後の章で圏論的な解釈を試みます.
:::


## 集合と列挙型

Haskellではデータ型を集合と**みなすこと**ができます. Haskellの型はあくまで型であり,厳密には集合ではありません. また,後の節で出てくるリストを使った`内包表記`などの**集合論的な書き方**も数学における集合ではありません.
あくまで類似したものです.

しかし,Haskellを集合とみなすことで,関数型プログラミングや,代数的データ型の意味がより直感的に理解できるようになります. しばらく,集合論とHaskellの対応について考えてみましょう.

::: note
特定のモノがそこに｢属するか判定可能なモノの集まり｣を｢集合｣という.
:::

集合の細かな定義は置いておいて,この講義では取り敢えずこのくらいの認識で問題ありません. しかし,ただのモノの集まりではなく,特定のモノがそこに属するかどうかを判定できる必要があるので注意が必要です.

例えば, ｢頭の良い人の集合｣のようなものは,｢頭が良い基準｣が人によって異なるので,集合とはみなせません.

ノーベル賞受賞者の集合,フィールズ賞受賞者の集合,メンサ会員の集合,XX模試の偏差値が70以上の人の集合,特定の科目で85点以上取った人の集合,など,誰でも判別可能な定義が必要です.

集合の表記法には,**外延(的)表記**及び**内包(的)表記**という2通りが存在します.

外延表記とは, 特定の集合に含まれる要素を全て記述する方法です.
私が過去に飼ったことのある犬の種類の集合を`MyDogs`という名前で呼ぶと,`MyDogs`に属するモノたちを記号`{ }`を使った外延表記によって以下のように書くことができます.


\begin{align*}
MyDogs = & \{ GoldenRetriever \\
         &, BlackRetriever    \\
         &, ShetlandSheepdog \\
         &, StandardPoodle \\
         &, StandardPoodle \}
\end{align*}


このとき,`GoldenRetriever`や,`ShetlandSheepdog`は`MyDogs`の`要素`であるといい,要素が特定の集合に属するとき,

$$ GoldenRetriever \in MyDogs $$ の様に書きます. 要素に属さないことは $Chihuahua \notin MyDogs$と書きます.

集合には順番は関係ないため,$\{x,y\}=\{y,z\}$ となります. また, 一つの集合に同じ要素は2つ以上属することができず, $\{x,x\}$ のような集合は定義できません.

集合には集合が属することも可能で, 集合 $S$ が $T$ に属するとき $S \in T$ が成り立ちます. また, 集合 $S$ の要素を幾つか取り出した集合 $T$ を $S$ の **部分集合** といい, $T \subset S$ と表記します.

$S = \{x, y, z\}$ のとき, $S$ の部分集合は

$$\{x\},\ \{y\},\ \{z\},\ \{x, y\},\ \{x, z\},\ \{y, z\},\ \{x, y, z\},\ \phi$$

となります. 任意の集合 $S$ に対して $\phi \subset S$ は成り立ちます.




Haskellにおいて,集合に属する要素をすべて書き出す(列挙する)データ型を`列挙型`として定義できます. 
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


要素が一つも属さない集合を`空集合`といい,記号$\phi$ または$｛｝$によって表されます.
Haskellでは空集合を表すデータ型として`Data.Void`に定義された`Void`が存在します. データ型として`ボトム型`,記号では`⊥`で表される場合もあります.

`Void`と同じ値を持たないデータ型は,コンストラクタを記述しないことで自分で実装することもできます. 例えば私が犬を今までに一匹もかったことがなければ, $$ MyPet = \phi $$ となり,データ型としては以下のように定義されます. 値が存在しない空集合と対応していることが分かります.

~~~ haskell
data Mypet
~~~

`Void`型は値が存在しないため実行することはできませんが,コンパイルを通すことはできます. ただし,あまり実用する機会はないので,以下の部分は興味がある人は読んでください.

::: note

- Voidの利用例

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


## 集合の内包表記と代数的データ型

列挙型において見た**外延表記**に対して,**内包表記**とは,$x \in S$を述語論理によって表記する方法です.

$x$の属する集合を$X$,条件式$p(x)$とすると,内包表記では

$S=\{x│x \in  X,p(x)\}$

という記法で, ｢**Xの要素のうちp(x)を満たす要素のみからなる集合S**｣を定義します.

例として,$R^+$ を非負の実数としたとき,$５$ 以下の非負の実数は

$\{x|x \in R^+, x \leqq 5 \}$

と書けます.

Haskellの代数的データ型では内包表記に基づくデータ型の定義そのものは提供されていません. しかし, **リスト内包表記**による擬似的な集合計算,及び**スマートコンストラクタ**による述語による絞り込みによって,類似した計算及びデータ型の構築が可能です. スマートコンストラクタは`module`など今後の学習内容を先取りしたかなり発展的な内容になるのでここでは記載はしますが対象は興味のある人のみとします. 

### リスト内包表記による計算

**リスト内包表記**はHaskellの標準構文で,集合論の内包表記と非常に似た形でリストを構成できます. 集合論では

$$S = \{x \mid x \in X, p(x)\}$$

と書くところを,Haskellでは

~~~ haskell
s = [ x | x <- xs, p x ]
~~~

と書きます. `x <- xs` が $x \in X$ に対応する **ジェネレータ**, `p x` が述語 $p(x)$ に対応する **ガード** です.

例として,集合論における「1 以上 10 以下の整数のうち偶数のみ」

$$E = \{x \mid x \in \mathbb{Z}, 1 \leqq x \leqq 10, x \bmod 2 = 0\}$$

は,リスト内包表記では以下のように記述できます.

~~~ haskell
evens :: [Int]
evens = [ x | x <- [1..10], x `mod` 2 == 0 ]
-- evens == [2,4,6,8,10]
~~~

ジェネレータやガードは複数書くこともでき,集合論における直積(複数の変数の並行走査)や条件の連言(AND)に自然に対応します.

~~~ haskell
-- {(x,y) | x ∈ [1..3], y ∈ [1..3], x /= y}
pairs :: [(Int,Int)]
pairs = [ (x,y) | x <- [1..3], y <- [1..3], x /= y ]
-- [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
~~~

ただし,リスト内包表記は**値のレベル**での構成であり,得られるのはあくまで `[Int]` などのリストです. 「型として偶数のみからなる新しいデータ型」を定義しているわけではない点に注意してください.

::: warn
ここではリスト内包表記を集合の内包表記の**比喩**として紹介していますが, リストと集合は別物です. リストには**順序があり, 同じ要素を重複して含むことができ, 要素の等価性も個別に比較する必要があります**. 一方,数学における集合は順序を持たず,同じ要素を重複して含むこともありません. リストが代数的にどのように定義されるかに関してはのちの章で扱います.

したがって,実際に**集合としての操作**(要素の存在判定,和集合 $\cup$,積集合 $\cap$,差集合 $\setminus$,重複の自動排除など)が必要な場合は,リストではなく `containers` パッケージの提供する `Data.Set` の `Set` 型を利用するほうが適切であり,計算量の面でも有利です.

~~~ haskell
import qualified Data.Set as Set

a, b :: Set.Set Int
a = Set.fromList [1,2,3,4]
b = Set.fromList [3,4,5,6]

main :: IO ()
main = do
    print $ Set.union        a b  -- fromList [1,2,3,4,5,6]  (和集合)
    print $ Set.intersection a b  -- fromList [3,4]          (積集合)
    print $ Set.difference   a b  -- fromList [1,2]          (差集合)
    print $ Set.member       3 a  -- True                    (要素判定)
~~~

リスト内包表記は「集合の内包表記と構文が似ていて直感的に書ける」という**記法の便利さ**のために使うもので,集合演算そのものを扱いたい場合は `Set` を用いるのがHaskellにおける一般的な流儀です.
:::

### スマートコンストラクタ (発展)

**型のレベル**で「他のデータ型から述語で絞り込んだ新しいデータ型」を作るには, **スマートコンストラクタ**というイディオムを用います. 代数的データ型の `data` / `newtype` 宣言そのものには述語で絞り込む機能はないため, `newtype` とモジュールシステムを組み合わせて事実上の絞り込みを実現します.

たとえば,`Int` のうち「3の倍数」のみを要素とするデータ型 `Mult3`

$$\mathrm{Mult3} = \{n \mid n \in \mathrm{Int}, n \bmod 3 = 0\}$$

を作りたいとします. 以下のようにモジュールを定義します.

~~~ haskell
module Mult3
  ( Mult3        -- 型名は公開
  , mkMult3      -- スマートコンストラクタ
  , unMult3      -- 値の取り出し
  ) where

-- コンストラクタ Mult3 は公開しない (モジュール外から直接作れないようにする)
newtype Mult3 = Mult3 Int

-- 検査付きのコンストラクタ: 3 の倍数のときのみ Just を返す
mkMult3 :: Int -> Maybe Mult3
mkMult3 n
  | n `mod` 3 == 0 = Just (Mult3 n)
  | otherwise      = Nothing

-- 値の取り出し
unMult3 :: Mult3 -> Int
unMult3 (Mult3 n) = n
~~~

ポイントは,`module` 宣言の出力リストで **コンストラクタ `Mult3` をエクスポートしていない**ことです. これにより,`Mult3` 型の値はモジュール外からは必ず `mkMult3` を経由してしか作れず,かつ `mkMult3` は 3 の倍数のときしか値を返しません. したがって `Mult3` 型の要素は必然的に $\{n \in \mathrm{Int} \mid n \bmod 3 = 0\}$ という述語付きの集合に対応する,という保証が値の生成経路のレベルで得られます.

利用側は以下のようになります.

~~~ haskell
import Mult3

main :: IO ()
main = do
    print $ mkMult3 9   -- Just (Mult3 9)
    print $ mkMult3 10  -- Nothing
    -- Mult3 10         -- ← コンパイルエラー: コンストラクタが見えない
~~~

::: warn
スマートコンストラクタは,代数的データ型そのものの機能ではなく,**代数的データ型とモジュールシステムの組み合わせ**によって間接的に内包表記的な絞り込みを実現する実用的なイディオムです. 型システムそのものにもっと踏み込んだ絞り込みを型レベルで与えたい場合は,GADTs や Refinement Types (Liquid Haskell) などの拡張機能がありますが,本講義の範囲では扱いません.
:::



## 直和型

集合 $A$, $B$ の**和集合(union)**を $A \cup B$, **積集合(intersection)**を $A \cap B$ と表し, それぞれ以下で定義されます.

$$A \cup B = \{x \mid x \in A \lor x \in B\}$$

$$A \cap B = \{x \mid x \in A \land x \in B\}$$

$A \cap B = \phi$ のとき, $A \cup B$ を $A$ と $B$ の **直和(Direct sum)** といいます.

事例として A \subset MyPet

Haskellでは以下のように直和型を定義できます.

~~~ haskell
data S = A a1 a2 | B b
data T = A { f :: a1, g :: a2 } | B { h :: b }
~~~

これを集合論的に解釈すると,

$$S = (A_1 \times A_2) \cup B$$

$$T = (A_1 \times A_2) \cup B$$

$$f((a_1, a_2) \in A_1 \times A_2 \subset T) = a_1$$

$$g((a_1, a_2) \in A_1 \times A_2 \subset T) = a_2$$

$$h(b \in B \subset T) = b$$

となります.

直和にすることで擬似的なダックタイピングが可能になります.


## 直積型

$A \times B = \{(a, b) \mid a \in A, b \in B\}$ を $A$ と $B$ の **直積(Cartesian Product)** といいます. 日本語では**積集合(intersection)**と似ていますが異なる概念なので注意しましょう.

Haskellでは以下のように直積型を定義できます.

~~~ haskell
data S = S x y
data T = T { f :: x, g :: y }
~~~

これを集合論的に解釈すると,

$$S = X \times Y$$

$$T = X \times Y, \quad f((x, y) \in T) = x, \quad g((x, y) \in T) = y$$

となります.

## new type





yakagika