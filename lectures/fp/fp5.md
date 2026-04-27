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
open: true
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
         &, Beagle \}
\end{align*}


このとき,`GoldenRetriever`や,`ShetlandSheepdog`は`MyDogs`の`要素`であるといい,要素が特定の集合に属するとき,

$$ GoldenRetriever \in MyDogs $$ の様に書きます. 要素に属さないことは $Chihuahua \notin MyDogs$と書きます.

集合には順番は関係ないため,$\{x,y\}=\{y,x\}$ となります. また, 一つの集合に同じ要素は2つ以上属することができず, $\{x,x\}$ のような集合は定義できません.

Haskellにおいて,集合に属する要素をすべて書き出す(列挙する)データ型を`列挙型`として定義できます. 
データ型の宣言は, `data`のあとに続いて,`データ型の名前(型構築子)`を書き,`=`の後ろにその`中身(コンストラクタ/データ構築子)`を書きます.
型構築子やデータ構築子は,大文字の英字で始めるのが規則です.

~~~ haskell
data MyDogs = GoldenRetriever
            | BlackRetriever
            | ShetlandSheepdog
            | StandardPoodle
            | Beagle
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


要素が一つも属さない集合を`空集合`といい,記号 $\phi$ または $\{\}$ によって表されます.
Haskellでは空集合を表すデータ型として`Data.Void`に定義された`Void`が存在します. データ型として`ボトム型`,記号では`⊥`で表される場合もあります.

`Void`と同じ値を持たないデータ型は,コンストラクタを記述しないことで自分で実装することもできます. 例えばある人が犬を今までに一匹もかったことがない場合を想定し, その人の飼った犬の集合を `EmptyDogs` と呼ぶことにすると, $$ \mathrm{EmptyDogs} = \phi $$ となり, データ型としては以下のように定義されます. 値が存在しない空集合と対応していることが分かります.

~~~ haskell
data EmptyDogs
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

::: note

**練習問題**

1. 曜日を表す列挙型 `Weekday` を日曜日から土曜日までの7つのコンストラクタで定義してください. `deriving Show` を付けて `print` できるようにしてください.

2. `Weekday` 型の値を受け取り, その日が週末(土日)であれば `True`, 平日であれば `False` を返す関数 `isWeekend :: Weekday -> Bool` をパターンマッチで実装してください.

3. `Weekday` 型の値を受け取り, 翌日の曜日を返す関数 `nextDay :: Weekday -> Weekday` を実装してください. 土曜日の次は日曜日に戻るように循環させます.

~~~ haskell
-- 実行例
main :: IO ()
main = do
    print $ isWeekend Sunday    -- True
    print $ isWeekend Monday    -- False
    print $ isWeekend Saturday  -- True
    print $ nextDay Friday      -- Saturday
    print $ nextDay Saturday    -- Sunday
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data Weekday = Sunday
             | Monday
             | Tuesday
             | Wednesday
             | Thursday
             | Friday
             | Saturday
             deriving Show

isWeekend :: Weekday -> Bool
isWeekend Saturday = True
isWeekend Sunday   = True
isWeekend _        = False

nextDay :: Weekday -> Weekday
nextDay Sunday    = Monday
nextDay Monday    = Tuesday
nextDay Tuesday   = Wednesday
nextDay Wednesday = Thursday
nextDay Thursday  = Friday
nextDay Friday    = Saturday
nextDay Saturday  = Sunday

main :: IO ()
main = do
    print $ isWeekend Sunday    -- True
    print $ isWeekend Monday    -- False
    print $ isWeekend Saturday  -- True
    print $ nextDay Friday      -- Saturday
    print $ nextDay Saturday    -- Sunday
~~~

列挙型では各コンストラクタを直接パターンマッチで場合分けでき, `_`(ワイルドカード)を使うと残り全てをまとめて扱えます.

</details>

:::


## 集合の内包表記と代数的データ型

列挙型において見た**外延表記**に対して,**内包表記**とは,$x \in S$を述語論理によって表記する方法です.

$x$ の属する集合を $X$, 条件式を $p(x)$ とすると, 内包表記では

$$S = \{x \mid x \in X, p(x)\}$$

という記法で, ｢**$X$ の要素のうち $p(x)$ を満たす要素のみからなる集合 $S$**｣を定義します.

例として, $\mathbb{R}^+$ を非負の実数としたとき, $5$ 以下の非負の実数は

$$\{x \mid x \in \mathbb{R}^+, x \leq 5\}$$

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

$$E = \{x \mid x \in \mathbb{Z}, 1 \leq x \leq 10, x \bmod 2 = 0\}$$

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

::: note

**練習問題**

1. 正の整数 `n` を受け取り, `n` の正の約数をすべて昇順に並べたリストを返す関数 `divisors :: Int -> [Int]` をリスト内包表記を用いて実装してください.

2. 正の整数 `n` を受け取り, $a^2 + b^2 = c^2$ を満たす $1 \leq a \leq b \leq c \leq n$ のピタゴラス数 $(a, b, c)$ の組をすべて返す関数 `pythagoreans :: Int -> [(Int, Int, Int)]` をリスト内包表記で実装してください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
    print $ divisors 12       -- [1,2,3,4,6,12]
    print $ divisors 13       -- [1,13]
    print $ pythagoreans 20   -- [(3,4,5),(5,12,13),(6,8,10),(8,15,17),(9,12,15),(12,16,20)]
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
-- 1. n の正の約数: 1 から n までの整数のうち n を割り切るもの
divisors :: Int -> [Int]
divisors n = [ x | x <- [1..n], n `mod` x == 0 ]

-- 2. ピタゴラス数: a <= b <= c かつ a^2 + b^2 = c^2
pythagoreans :: Int -> [(Int, Int, Int)]
pythagoreans n =
    [ (a, b, c)
    | a <- [1..n]
    , b <- [a..n]
    , c <- [b..n]
    , a*a + b*b == c*c
    ]

main :: IO ()
main = do
    print $ divisors 12
    print $ divisors 13
    print $ pythagoreans 20
~~~

ジェネレータ `b <- [a..n]`, `c <- [b..n]` のように **前のジェネレータで束縛された変数を後続のジェネレータで使える** 点がポイントです. これにより $a \leq b \leq c$ という順序関係の下で候補を生成でき, 重複を避けられます.

</details>

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

集合には集合が属することも可能で, 集合 $S$ が $T$ に属するとき $S \in T$ が成り立ちます. また, 集合 $S$ の要素を幾つか取り出した集合 $T$ を $S$ の **部分集合** といい, $T \subset S$ と表記します.

$S = \{x, y, z\}$ のとき, $S$ の部分集合は

$$\{x\},\ \{y\},\ \{z\},\ \{x, y\},\ \{x, z\},\ \{y, z\},\ \{x, y, z\},\ \phi$$

となります.

事例として $A, B \subset \mathrm{MyDogs}$, $A = \{\mathrm{GoldenRetriever}, \mathrm{BlackRetriever}, \mathrm{ShetlandSheepdog}\}$, $B = \{\mathrm{BlackRetriever}, \mathrm{StandardPoodle}\}$ のとき, 和集合 $A \cup B$ と積集合 $A \cap B$ はそれぞれ

$$A \cup B = \{\mathrm{GoldenRetriever}, \mathrm{BlackRetriever}, \mathrm{ShetlandSheepdog}, \mathrm{StandardPoodle}\}$$

$$A \cap B = \{\mathrm{BlackRetriever}\}$$

となります. 和集合は「$A$ または $B$ のいずれかに属する要素」を集めた集合, 積集合は「$A$ と $B$ の両方に属する要素」のみを集めた集合です. このとき $A \cap B = \{\mathrm{BlackRetriever}\} \neq \phi$ なので, $A$ と $B$ は直和にはなりません. 一方, $A = \{\mathrm{GoldenRetriever}, \mathrm{ShetlandSheepdog}\}$, $B = \{\mathrm{BlackRetriever}, \mathrm{StandardPoodle}\}$ のように共通要素がない場合は $A \cap B = \phi$ となり, $A \cup B$ は $A$ と $B$ の直和となります.

Haskellでは既に定義した `Int` と `MyDogs` の直和に相当する型を, 次のように定義できます. ここでは「整数または `MyDogs` のいずれかの値を持てるデータ型 `IntOrDog`」を定義します.

~~~ haskell
data IntOrDog = MkInt Int
              | MkDog MyDogs
              deriving Show
~~~

**記法の解説:**

- 左辺の `IntOrDog` は **型構築子(type constructor)**, 右辺の `MkInt`, `MkDog` は **データ構築子(data constructor)** と呼ばれます. どちらも大文字で始める必要があります.
- `|` は「または」を意味し, `IntOrDog` の値は `MkInt <整数>` または `MkDog <犬>` のいずれかの形をとる, という **直和** を宣言します.
- データ構築子の後ろに書かれた `Int`, `MyDogs` は包み込む値の型です. 具体的には `MkInt :: Int -> IntOrDog`, `MkDog :: MyDogs -> IntOrDog` という関数として扱えます.

値を作ったり, パターンマッチで取り出したりできます.

~~~ haskell
describe :: IntOrDog -> String
describe (MkInt n) = "整数 " ++ show n
describe (MkDog d) = "犬 "   ++ show d

main :: IO ()
main = do
    putStrLn $ describe (MkInt 42)               -- 整数 42
    putStrLn $ describe (MkDog GoldenRetriever)  -- 犬 GoldenRetriever
~~~

集合論的に解釈すると, `MkInt` と `MkDog` という互いに異なるタグでくるむことで `Int` と `MyDogs` が **互いに素** な形で一つの型に合流するため,

$$\mathrm{IntOrDog} = \mathrm{Int} \cup \mathrm{MyDogs} \quad (\mathrm{Int} \cap \mathrm{MyDogs} = \phi)$$

という **直和** になります. 一般に, 代数的データ型の `|` で分けたコンストラクタは必ずタグで区別されるため, Haskellの直和型は常に直和の構造を持ちます.

なお, データ構築子は任意個の引数を取ることができ, `MkPair Int MyDogs` のように複数の型を並べると, その部分は **直積** になります. つまり直和型の各コンストラクタは「いくつかの直積をタグ付きで合流させたもの」として解釈できます.

### レコード構文

記法のバリエーションとして, **レコード構文(record syntax)** を使うと各データ構築子にラベル(フィールド名)を付けられます.

~~~ haskell
data PetEntry = ByNumber { petNumber :: Int }
              | ByBreed  { petBreed  :: MyDogs }
              deriving Show
~~~

- `ByNumber { petNumber = 7 }` のように **フィールド名による値の生成** ができます. 従来の構文 `ByNumber 7` も併用可能です.
- フィールド名 `petNumber`, `petBreed` は **自動的にアクセサ関数**として定義されます.
    - `petNumber :: PetEntry -> Int`
    - `petBreed  :: PetEntry -> MyDogs`

集合論的には, これらのフィールド名は直和型の各部分集合から元の値を取り出す **射影** に対応します.

$$\mathrm{petNumber}(\mathrm{ByNumber}(n) \in \mathrm{Int} \subset \mathrm{PetEntry}) = n$$

$$\mathrm{petBreed}(\mathrm{ByBreed}(d) \in \mathrm{MyDogs} \subset \mathrm{PetEntry}) = d$$

~~~ haskell
ghci> petNumber (ByNumber 7)
7
ghci> petBreed (ByBreed GoldenRetriever)
GoldenRetriever
~~~

::: warn
レコード構文のアクセサ関数は, そのフィールドを持たない別のコンストラクタに適用すると **実行時エラー** になります. たとえば `petNumber (ByBreed GoldenRetriever)` は実行時にエラーを発生させます. 直和型に対するレコード構文のアクセサは部分関数である点に注意してください. 安全に取り出すにはパターンマッチを利用するか, 後の章で扱う `Maybe` を返す関数として包み直すのが一般的です.
:::

このように, 直和にすることで互いに異なる型の値を一つの型に集約でき, 型安全性を保ったまま「複数の型のいずれかをとる値」を表現できるようになります. 動的型付け言語におけるダックタイピングに似た柔軟さを, 型システムの保証を壊さずに実現する手段と考えることができます.

::: note

**練習問題**

1. 図形を表す直和型 `Shape` を以下の3つのコンストラクタで定義してください.
    - `Circle` : 半径(`Double`)を1つ持つ
    - `Rectangle` : 幅と高さ(`Double` 2つ)を持つ
    - `Triangle` : 3辺の長さ(`Double` 3つ)を持つ

2. `Shape` の値を受け取り, その図形の面積を返す関数 `area :: Shape -> Double` をパターンマッチで実装してください. 三角形の面積はヘロンの公式

    $$s = \frac{a + b + c}{2}, \quad S = \sqrt{s(s-a)(s-b)(s-c)}$$

    を用います.

~~~ haskell
-- 実行例
main :: IO ()
main = do
    print $ area (Circle 1)            -- 3.141592653589793
    print $ area (Rectangle 3 4)       -- 12.0
    print $ area (Triangle 3 4 5)      -- 6.0
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data Shape = Circle Double
           | Rectangle Double Double
           | Triangle Double Double Double
           deriving Show

area :: Shape -> Double
area (Circle r)       = pi * r * r
area (Rectangle w h)  = w * h
area (Triangle a b c) = sqrt (s * (s - a) * (s - b) * (s - c))
  where
    s = (a + b + c) / 2

main :: IO ()
main = do
    print $ area (Circle 1)       -- 3.141592653589793
    print $ area (Rectangle 3 4)  -- 12.0
    print $ area (Triangle 3 4 5) -- 6.0
~~~

直和型の各コンストラクタが異なる数・種類の引数を持ってよいため, 図形ごとに必要な情報を自然に表現できます. 面積計算のような処理はパターンマッチで場合分けして書くのが定石です.

</details>

:::


## 直積型

$A \times B = \{(a, b) \mid a \in A, b \in B\}$ を $A$ と $B$ の **直積(Cartesian Product)** といいます. 直積は $A$ と $B$ から要素を一つずつ選んで並べた組 $(a, b)$ 全体からなる集合です. 日本語では**積集合(intersection)**と字面が似ていますが異なる概念なので注意しましょう.

事例として $\mathrm{MyDogs}$ と整数の集合 $\mathbb{Z}$ の直積を考えると,

$$\mathrm{MyDogs} \times \mathbb{Z} = \{(d, n) \mid d \in \mathrm{MyDogs}, n \in \mathbb{Z}\}$$

となり, 「犬種と整数(たとえば年齢)のペア」全体の集合を表します. たとえば $(\mathrm{GoldenRetriever}, 3), (\mathrm{Beagle}, 7) \in \mathrm{MyDogs} \times \mathbb{Z}$ です.

Haskellでは既に定義した `MyDogs` と `Int` の直積に相当する型を, 次のように定義できます. ここでは「犬種と年齢のペアを持つデータ型 `DogAge`」を定義します.

~~~ haskell
data DogAge = MkDogAge MyDogs Int
            deriving Show
~~~

**記法の解説:**

- 左辺の `DogAge` は **型構築子(type constructor)**, 右辺の `MkDogAge` は **データ構築子(data constructor)** です. 直和型と違い, ここでは構築子は一つしかありません(`|` が無い).
- データ構築子の後ろに **複数の型を空白区切りで並べる** ことで, その構築子が包む値が直積になります. `MkDogAge MyDogs Int` は「`MyDogs` と `Int` を並べた組」を構成するコンストラクタです.
- 関数としての型は `MkDogAge :: MyDogs -> Int -> DogAge` となり, カリー化された2引数関数のように扱えます.

値の生成とパターンマッチは以下のようになります.

~~~ haskell
goldenAge :: DogAge
goldenAge = MkDogAge GoldenRetriever 3

breedOf :: DogAge -> MyDogs
breedOf (MkDogAge d _) = d

ageOf :: DogAge -> Int
ageOf (MkDogAge _ n) = n

main :: IO ()
main = do
    print $ breedOf goldenAge  -- GoldenRetriever
    print $ ageOf   goldenAge  -- 3
~~~

集合論的に解釈すると,

$$\mathrm{DogAge} = \mathrm{MyDogs} \times \mathrm{Int}$$

となります. `MkDogAge` は2つの集合の要素を一組にまとめる対応

$$\mathrm{MkDogAge} : \mathrm{MyDogs} \times \mathrm{Int} \to \mathrm{DogAge}, \quad (d, n) \mapsto \mathrm{MkDogAge}\,d\,n$$

に対応します.

### レコード構文

直和型と同様に, 直積型でも **レコード構文(record syntax)** を用いて各フィールドに名前を付けることができます. 直積型ではコンストラクタが一つのため, レコード構文の恩恵(アクセサ関数の自動生成やフィールド名による値の生成)が特に活きます.

~~~ haskell
data DogAge = MkDogAge { breed :: MyDogs
                       , age   :: Int
                       }
            deriving Show
~~~

この定義により, 以下のアクセサ関数が **自動的に** 定義されます.

- `breed :: DogAge -> MyDogs`
- `age   :: DogAge -> Int`

集合論的には, これらのフィールド名は直積からそれぞれの成分を取り出す **射影(projection)** $\pi_1, \pi_2$ に対応します.

$$\mathrm{breed}((d, n) \in \mathrm{DogAge}) = d$$

$$\mathrm{age}((d, n) \in \mathrm{DogAge}) = n$$

値の生成はフィールド名を明示する形でも, 従来の位置引数の形でも可能です.

~~~ haskell
goldenAge :: DogAge
goldenAge = MkDogAge { breed = GoldenRetriever, age = 3 }

-- もしくは位置引数で
goldenAge' :: DogAge
goldenAge' = MkDogAge GoldenRetriever 3

main :: IO ()
main = do
    print $ breed goldenAge  -- GoldenRetriever
    print $ age   goldenAge  -- 3
~~~

また, レコード構文では **一部のフィールドのみを更新した新しい値** を作る記法も利用できます. Haskellでは値は不変(再代入できない)なので,「更新」とは元の値を変更するのではなく, 一部を書き換えた **新しい値を返す** ことを意味します.

~~~ haskell
olderGolden :: DogAge
olderGolden = goldenAge { age = 10 }
-- MkDogAge { breed = GoldenRetriever, age = 10 }
~~~

直和型の場合と異なり, 直積型のレコードフィールドは単一のコンストラクタに属しているため **アクセサは全域関数** であり, 実行時エラーの心配はありません.

::: note

**練習問題**

1. 人物を表す直積型 `Person` を, 以下のフィールドを持つレコード構文で定義してください.
    - `personName :: String` (氏名)
    - `personAge :: Int` (年齢)
    - `personEmail :: String` (メールアドレス)

2. `Person` 型の値を受け取り, 年齢を1歳加えた新しい `Person` を返す関数 `birthday :: Person -> Person` を **レコード更新構文** を用いて実装してください.

3. 2人の `Person` の年齢の合計を返す関数 `totalAge :: Person -> Person -> Int` を, レコード構文のアクセサ関数を用いて実装してください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
    let alice = Person { personName = "Alice", personAge = 30, personEmail = "alice@example.com" }
        bob   = Person { personName = "Bob"  , personAge = 25, personEmail = "bob@example.com"   }
    print $ birthday alice           -- Alice の年齢が 31 に
    print $ totalAge alice bob       -- 55
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data Person = Person
    { personName  :: String
    , personAge   :: Int
    , personEmail :: String
    } deriving Show

-- レコード更新構文で年齢のみを更新した新しい値を返す
birthday :: Person -> Person
birthday p = p { personAge = personAge p + 1 }

-- アクセサ関数 personAge を利用して合計を計算
totalAge :: Person -> Person -> Int
totalAge p1 p2 = personAge p1 + personAge p2

main :: IO ()
main = do
    let alice = Person { personName = "Alice", personAge = 30, personEmail = "alice@example.com" }
        bob   = Person { personName = "Bob"  , personAge = 25, personEmail = "bob@example.com"   }
    print $ birthday alice      -- Person {personName = "Alice", personAge = 31, personEmail = "alice@example.com"}
    print $ totalAge alice bob  -- 55
~~~

レコード更新構文 `p { personAge = ... }` は `p` を破壊的に書き換えるのではなく, `personAge` のみを変えた新しい `Person` 値を作って返します. 他のフィールドは `p` のものがそのままコピーされるため, フィールド数が多いレコードの部分更新を簡潔に書けます.

</details>

:::

## newtype

(後ほど実装)





yakagika