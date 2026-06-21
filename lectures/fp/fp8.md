---
title: 関数型プログラミング Ch8 代数的データ型2
description: 資料
tags:
    - algebra
    - lecture
    - statistics
    - haskell
featured: true
katex: true
date: 2025-06-06
tableOfContents: true
previousChapter: fp7.html
nextChapter: fp9.html
open: true
---

[前章 (第7章)](fp7.html)では, 代数的データ型を **集合論的に** 解釈しました. 列挙型を集合の外延表記, 直積型を直積 $A \times B$, 直和型を直和 $A \cup B$ と対応させ, 型を「値の集合」とみなす見方を学びました.

しかし[第7章](fp7.html)では, データ型を定義するたびに書いてきた `deriving Show` について「詳細に関しては後ほど扱います」と先送りしていました ([第7章](fp7.html)「集合と列挙型」). この `deriving` の正体が **型クラス (type class)** です.

この章では前半で型クラスとは何かを学び, [第7章](fp7.html)で先送りした `deriving Show` を **自分の手で書いて** その中身を明らかにします. 後半では, 型クラスが数学の **代数構造** (半群・モノイド・群) と自然に対応することを見ます. [第7章](fp7.html)で「型 = 集合」と捉えた上に「演算」を載せると **代数 (algebra)** になる, という流れです.

::: note
[第7章](fp7.html)では型を **集合** とみなしました. この章で扱う型クラスは, いわば **「ある演算や性質を備えた型たちの集まり」**, つまり「型の集合」に近いものです. 集合 (型) の上に演算を入れたものが代数構造であり, Haskell ではその演算を型クラスのメソッドとして, 演算が満たすべき法則を (コンパイラは強制しませんが) **規約** として表現します.
:::

# 型クラス

## 型クラスとは

[第7章](fp7.html)までに, いくつかの異なる型に対して「同じ名前の操作」を使ってきました. たとえば `show` は `Int` でも `MyDogs` でも `Bool` でも使えますし, `==` は数でも文字列でも使えます. これらは「型ごとに別々に定義されているが, 同じ名前で呼べる」操作です.

このように **複数の型に共通する操作の集まりに名前を付け, その操作を持つ型を統一的に扱う仕組み** が **型クラス (type class)** です. 「型のクラス (種類分け)」であって, オブジェクト指向の「クラス (オブジェクトの設計図)」とは別物である点に注意してください.

型クラスは `class` で **宣言** し, 個々の型をそのクラスに **所属させる** には `instance` を使います. まず最小の例で構文を見ます. 「挨拶できる」という性質を表す型クラス `Greet` を定義してみましょう.

~~~ haskell
-- 型クラスの宣言: 「greet という操作を持つ型 a の集まり」
class Greet a where
  greet :: a -> String
~~~

`class Greet a where` の `a` は **型変数** で,「これからこのクラスに所属する任意の型」を表します. `greet :: a -> String` はこのクラスに属する型が必ず備えるべき操作 (**メソッド**) のシグネチャです.

次に, [第7章](fp7.html)で定義したような列挙型をこのクラスに所属させます.

~~~ haskell
data Animal = Cat | Dog

-- Animal を Greet クラスのインスタンスにする
instance Greet Animal where
  greet Cat = "にゃー"
  greet Dog = "わん"
~~~

`instance Greet Animal where` は「`Animal` 型は `Greet` クラスに所属し, その `greet` はこう定義する」という宣言です. これで `greet Cat` が `"にゃー"` を返すようになります.

型クラスの効果は, **クラス制約 (class constraint)** を使った多相関数で発揮されます. [第5章](fp5.html)・[第6章](fp6.html)・[第7章](fp7.html)で `print :: Show a => a -> IO ()` のように `=>` の付いた型を見てきましたが, この `Show a =>` こそがクラス制約です.

~~~ haskell
-- 「Greet のインスタンスである任意の型 a」を受け取れる関数
hello :: Greet a => a -> String
hello x = "こんにちは, " ++ greet x
~~~

`Greet a => a -> String` は「`a` が `Greet` のインスタンスでありさえすれば, どの型でも受け取れる」という意味です. `=>` の左がクラス制約, 右が本来の型です. これにより, 型ごとに `helloAnimal`, `helloDog` …と別名で関数を書く必要がなくなります.

::: note
集合論的に言えば, 型クラス `C` は「メソッドを備えた型たちの集まり」であり, クラス制約 `C a =>` は「型 `a` がその集まりに属する ($a \in C$ とみなせる) ことを要求する」記法だと考えられます. ただし厳密には型クラスは型の集合そのものではなく, GHC が解決する **制約** です.
:::

Haskell の標準ライブラリには, 最初から多くの型クラスが用意されています. 以下ではそのうち基本的な `Show` / `Eq` / `Ord` / `Enum` / `Bounded` を順に見ていきます. これらはすべて `deriving` で自動導出できますが, ここでは **自分の手で書く** ことで `deriving` が何を生成しているのかを理解します.

## Show

[第7章](fp7.html)で何度も書いてきた `deriving Show` は, `Show` という型クラスのインスタンスを **GHC に自動生成させる** 記法でした. `Show` クラスの中心となるメソッドは `show` です.

~~~ haskell
-- 標準ライブラリの Show (簡略版)
class Show a where
  show :: a -> String
~~~

`show :: a -> String` は「値を文字列に変換する」操作です. [第7章](fp7.html)で見た `print x = putStrLn (show x)` の `show` がこれです. `deriving Show` を書く代わりに, この `show` を **自分で実装** すれば, それが手書きの `Show` インスタンスになります.

~~~ haskell
data Color = Red | Green | Blue

instance Show Color where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"

main :: IO ()
main = do
  print Red    -- Red
  print Green  -- Green
  print Blue   -- Blue
~~~

この手書きの `show` は, 各コンストラクタを **その名前と同じ文字列** に変換しています. これはまさに `deriving Show` が自動生成する内容と一致します. つまり, 列挙型に対する `deriving Show` は,

~~~ haskell
instance Show Color where
  show Red   = "Red"
  show Green = "Green"
  show Blue  = "Blue"
~~~

を機械的に書いたものだと考えてよいのです. [第7章](fp7.html)で「`deriving Show` はコンストラクタを文字列に変換する `show` を自動で導入するための記法」と説明したのは, このことを指していました.

もちろん, `show` を自分で書けば, コンストラクタ名とは違う表示にすることもできます.

~~~ haskell
data Color = Red | Green | Blue

instance Show Color where
  show Red   = "あか"
  show Green = "みどり"
  show Blue  = "あお"
~~~

::: warn
コンストラクタが **引数を持つ** 場合 (直積型・直和型), `deriving Show` は値を `MkDogAge GoldenRetriever 3` のように表示し, 必要に応じて括弧を付けます. この「必要に応じた括弧付け」は `show` ではなく `showsPrec` という, 表示の優先順位を考慮するメソッドによって行われます. 手書きで完全に `deriving` と一致させるには `showsPrec` を実装する必要がありますが, 本講義では立ち入りません. 多くの場合 `deriving Show` をそのまま使えば十分です.
:::

::: note
`Show` の対になるクラスとして, 文字列から値へ変換する `Read` クラス (`read :: Read a => String -> a`) もあります. `read "Red" :: Color` のように使いますが, 失敗時に実行時エラーになるため, 実用では後の章で扱う `Maybe` を返す `readMaybe` がよく使われます.
:::

::: note

### Exercise CH8-1

**方位型 `Direction` に `Show` を手書きする**

1. 東西南北を表す列挙型 `Direction` を `North`, `East`, `South`, `West` の 4 つのコンストラクタで定義してください. ただし **`deriving Show` は付けず**, `Show` インスタンスを手書きしてください.

2. 手書きの `show` は, 各方位を日本語 1 文字 (`"北"`, `"東"`, `"南"`, `"西"`) に変換するようにしてください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print North   -- 北
  print East    -- 東
  print South   -- 南
  print West    -- 西
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data Direction = North | East | South | West

instance Show Direction where
  show North = "北"
  show East  = "東"
  show South = "南"
  show West  = "西"

main :: IO ()
main = do
  print North   -- 北
  print East    -- 東
  print South   -- 南
  print West    -- 西
~~~

`deriving Show` ならコンストラクタ名そのもの (`North` 等) が表示されますが, `show` を手書きすればこのように好きな文字列へ変換できます. `Show` の最小完全定義は `show` (または `showsPrec`) です.

</details>

:::

::: note

### Exercise CH8-2

**No instance for (Show ...) (インスタンスの欠如)**

以下のコードはコンパイルが通りません. 原因を答えて修正してください.

~~~ haskell
-- ch8-2.hs (誤りあり)
data Fruit = Apple | Orange | Grape

main :: IO ()
main = print Apple
~~~

実エラー:

~~~ sh
ch8-2.hs:4:8: error: [GHC-39999]
    • No instance for ‘Show Fruit’ arising from a use of ‘print’
    • In the expression: print Apple
      In an equation for ‘main’: main = print Apple
  |
4 | main = print Apple
  |        ^^^^^^^^^^^
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

**原因**: `print :: Show a => a -> IO ()` は引数の型が `Show` のインスタンスであることを要求するが, `Fruit` には `Show` インスタンスが無い. `deriving Show` も手書きインスタンスも無いため, 値を文字列に変換できずエラーになる. [第7章](fp7.html)で `deriving Show` を付け忘れたときに出たのと同じエラーで, その正体が「型クラスのインスタンス欠如」であることが本章で分かる.

修正方針は 2 通り.

**修正 A**: `deriving Show` で自動導出する.

~~~ haskell
data Fruit = Apple | Orange | Grape deriving Show

main = print Apple   -- Apple
~~~

**修正 B**: `Show` インスタンスを手書きする.

~~~ haskell
data Fruit = Apple | Orange | Grape

instance Show Fruit where
  show Apple  = "りんご"
  show Orange = "みかん"
  show Grape  = "ぶどう"

main = print Apple   -- りんご
~~~

`No instance for (C T)` というエラーは「型 `T` が型クラス `C` のインスタンスになっていない」ことを意味する. `deriving` するか, `instance` を書くかで解決する.

</details>

:::

## Eq — 等価性

`Eq` は「2 つの値が **等しいか** を判定できる」型クラスです. 比較演算子 `==` と `/=` (等しくない) を提供します.

~~~ haskell
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
~~~

`Eq` を手書きしてみます.

~~~ haskell
data Color = Red | Green | Blue

instance Eq Color where
  Red   == Red   = True
  Green == Green = True
  Blue  == Blue  = True
  _     == _     = False

main :: IO ()
main = do
  print (Red == Red)    -- True
  print (Red == Blue)   -- False
  print (Red /= Blue)   -- True
~~~

ここで `/=` を定義していないのに `Red /= Blue` が動くことに注目してください. `Eq` クラスには **デフォルト実装** があり, `x /= y = not (x == y)` と定義されています. そのため `==` だけ書けば `/=` は自動的に使えます. このように「これだけ定義すれば残りはデフォルトで埋まる」最小限のメソッドの組を **最小完全定義 (minimal complete definition)** といいます. `Eq` の最小完全定義は「`==` または `/=` のどちらか一方」です.

`deriving Eq` は, この「同じコンストラクタどうしなら等しく, 引数があればその引数も再帰的に `==` で比較する」という定義を自動生成します.

::: note
数学的には, 等価性 `==` は次の **同値関係 (equivalence relation)** の法則を満たすべきものです. `deriving Eq` が生成する定義はこれらを満たしますが, 手書きするときは自分で保証する必要があります.

- **反射律**: $\forall x.\ x = x$
- **対称律**: $\forall x, y.\ \ x = y \implies y = x$
- **推移律**: $\forall x, y, z.\ \ (x = y \land y = z) \implies x = z$

いずれも先頭に $\forall$ が付いた **全称命題** ([第7章](fp7.html)で導入) で, ｢**任意の** 要素について成り立つ｣ことを要求しています.
:::

::: note

### Exercise CH8-3

**クラス制約の不足 (Could not deduce ...)**

以下の関数 `same` は「2 つの値が等しいか」を判定しようとしていますが, コンパイルが通りません. 原因を答えて修正してください.

~~~ haskell
-- ch8-3.hs (誤りあり)
same :: a -> a -> Bool
same x y = x == y

main :: IO ()
main = print (same 1 1)
~~~

実エラー:

~~~ sh
ch8-3.hs:3:12: error: [GHC-39999]
    • No instance for ‘Eq a’ arising from a use of ‘==’
      Possible fix:
        add ‘Eq a’ to the context of
          the type signature for:
            same :: forall a. a -> a -> Bool
    • In the expression: x == y
      In an equation for ‘same’: same x y = x == y
  |
3 | same x y = x == y
  |            ^^^^^^
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

**原因**: `same :: a -> a -> Bool` は「**任意の** 型 `a`」を受け取ると宣言しているが, 本体で `x == y` を使っている. `==` は `Eq` クラスのメソッドなので, `a` が `Eq` のインスタンスである保証が無いと使えない. 型注釈にクラス制約 `Eq a =>` が欠けているため, GHC は「`Eq a` が導けない」と指摘している.

修正: 型注釈に **クラス制約** `Eq a =>` を加える.

~~~ haskell
same :: Eq a => a -> a -> Bool
same x y = x == y

main = print (same 1 1)   -- True
~~~

「多相関数の本体でクラスのメソッド (`==`, `<`, `show` など) を使うなら, その型変数にクラス制約を付ける」のが原則. もし大小比較 `<` も使うなら `Ord a =>` が必要になる (`Ord` は `Eq` をスーパークラスに含むので, `Ord a =>` だけで `==` も使えるようになる).

</details>

:::

## Ord — 順序

`Ord` は「2 つの値の **大小** を比較できる」型クラスです. ここで重要なのは, `Ord` が `Eq` を **前提とする** 点です. 大小を比べるには「等しい」という概念が必要だからです. これを **スーパークラス (superclass)** といい, 次のように宣言されています.

~~~ haskell
class Eq a => Ord a where
  compare :: a -> a -> Ordering
  (<)  :: a -> a -> Bool
  (<=) :: a -> a -> Bool
  (>)  :: a -> a -> Bool
  (>=) :: a -> a -> Bool
  max  :: a -> a -> a
  min  :: a -> a -> a
~~~

`class Eq a => Ord a where` の `Eq a =>` が「`Ord` のインスタンスになるには, まず `Eq` のインスタンスでなければならない」という制約です. これが, 本章で `Eq` を `Ord` より先に説明する理由です.

中心となるメソッドは `compare` で, 2 つの値を比べて `Ordering` 型の値を返します. `Ordering` は

~~~ haskell
data Ordering = LT | EQ | GT
~~~

という, [第7章](fp7.html)で学んだ **列挙型** そのものです (`LT` = less than, `EQ` = equal, `GT` = greater than). `Ord` の最小完全定義は「`compare` または `<=` のどちらか一方」で, それさえ書けば `<`, `>`, `max`, `min` などはデフォルト実装から導かれます.

~~~ haskell
data Size = Small | Medium | Large deriving (Eq, Show)

instance Ord Size where
  compare a b = compare (rank a) (rank b)
    where
      rank :: Size -> Int
      rank Small  = 0
      rank Medium = 1
      rank Large  = 2

main :: IO ()
main = do
  print (compare Small Large)             -- LT
  print (Medium < Large)                  -- True
  print (maximum [Small, Large, Medium])  -- Large
~~~

ここでは各コンストラクタに `Int` の **順位** を割り当て, その `Int` どうしの `compare` に処理を委ねています (`Int` はすでに `Ord` のインスタンスなので `compare` が使えます). `<` や `maximum` を一切定義していないのに使えるのは, これらが `compare` を用いたデフォルト実装を持つためです.

`deriving Ord` を使うと, **コンストラクタを宣言した順序** がそのまま大小になります. 上の `Size` なら `Small < Medium < Large` です. 手書きの `rank` を使った定義は, この自動導出と同じ結果を与えます.

::: warn
`deriving (Eq, Ord)` のように複数のクラスをまとめて導出できますが, `Ord` を導出するなら `Eq` も導出する (またはインスタンスにする) 必要があります. スーパークラスの関係上, `Eq` 抜きで `Ord` だけを導出することはできません.
:::

::: note
`Ord` が満たすべき法則は **全順序 (total order)** の公理です.

- **反対称律**: $\forall x, y.\ \ (x \le y \land y \le x) \implies x = y$
- **推移律**: $\forall x, y, z.\ \ (x \le y \land y \le z) \implies x \le z$
- **全順序性**: $\forall x, y.\ \ (x \le y) \lor (y \le x)$

`Eq` と同様, これらも $\forall$ で始まる **全称命題**です.
:::

::: note

### Exercise CH8-4

**メダル型 `Medal` に `Eq` と `Ord` を手書きする**

1. メダルを表す列挙型 `Medal` を `Gold`, `Silver`, `Bronze` で定義し, `deriving Show` だけ付けてください.

2. `Eq` インスタンスを手書きしてください (同じ種類なら等しい).

3. `Ord` インスタンスを手書きしてください. ただし **金 > 銀 > 銅** の順序 (金が最も大きい) になるようにしてください. `compare` を実装します.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (Gold == Gold)            -- True
  print (Gold == Silver)          -- False
  print (compare Gold Bronze)     -- GT
  print (Silver < Gold)           -- True
  print (maximum [Bronze, Gold, Silver])  -- Gold
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data Medal = Gold | Silver | Bronze deriving Show

instance Eq Medal where
  Gold   == Gold   = True
  Silver == Silver = True
  Bronze == Bronze = True
  _      == _      = False

instance Ord Medal where
  compare a b = compare (rank b) (rank a)   -- 金を大きくするため逆順に
    where
      rank :: Medal -> Int
      rank Gold   = 0
      rank Silver = 1
      rank Bronze = 2

main :: IO ()
main = do
  print (Gold == Gold)                    -- True
  print (Gold == Silver)                  -- False
  print (compare Gold Bronze)             -- GT
  print (Silver < Gold)                   -- True
  print (maximum [Bronze, Gold, Silver])  -- Gold
~~~

`Gold` を順位 `0` (最小の数) にしておき, `compare (rank b) (rank a)` と **左右を入れ替える** ことで「数が小さい金ほど大きい」順序を作っています. `Ord` は `Eq` をスーパークラスに持つので, `Eq` インスタンスも必要です. `<` や `maximum` は `compare` のデフォルト実装から自動的に使えます.

</details>

:::

## Enum と Bounded — 列挙

[第7章](fp7.html)の演習 CH7-1 では, 曜日型 `Weekday` の「翌日」を返す `nextDay` を, 7 通りすべて手書きで場合分けしました. `Enum` と `Bounded` を使うと, これをずっと簡潔に書けます.

`Enum` は「値を **順番に数え上げられる**」型クラスで, 前後の値を返す `succ` (successor, 次) / `pred` (predecessor, 前) や, 値と整数を相互変換する `toEnum` / `fromEnum` を提供します. `[a..b]` という範囲記法も `Enum` の上に成り立っています.

~~~ haskell
class Enum a where
  succ     :: a -> a
  pred     :: a -> a
  toEnum   :: Int -> a
  fromEnum :: a -> Int
  -- ほか, 範囲記法 [a..b] を支えるメソッド
~~~

`Bounded` は「**最小値と最大値を持つ**」型クラスで, `minBound` と `maxBound` を提供します.

~~~ haskell
class Bounded a where
  minBound :: a
  maxBound :: a
~~~

この 2 つを `deriving` すると, [第7章](fp7.html)の `nextDay` は次のように書き直せます.

~~~ haskell
data Weekday = Sunday | Monday | Tuesday | Wednesday | Thursday | Friday | Saturday
  deriving (Show, Eq, Ord, Enum, Bounded)

-- 翌日: 土曜の次は日曜に戻るよう循環させる
nextDay :: Weekday -> Weekday
nextDay d
  | d == maxBound = minBound
  | otherwise     = succ d

-- 全曜日のリスト
allDays :: [Weekday]
allDays = [minBound .. maxBound]

main :: IO ()
main = do
  print (nextDay Friday)    -- Saturday
  print (nextDay Saturday)  -- Sunday
  print allDays             -- [Sunday,Monday,Tuesday,Wednesday,Thursday,Friday,Saturday]
~~~

`succ d` が「宣言順での次のコンストラクタ」を返し, `d == maxBound` (= `Saturday`) のときだけ `minBound` (= `Sunday`) に巻き戻すことで, 7 通りの場合分けを書かずに循環を表現できています. `[minBound .. maxBound]` は `Enum` の範囲記法と `Bounded` の端点を組み合わせて「全要素のリスト」を作るイディオムです.

::: warn
`succ maxBound` や `pred minBound` は範囲外なので **実行時エラー** になります. 上の `nextDay` で `d == maxBound` を先に確認しているのは, `succ Saturday` を呼ばないためです.
:::

::: note

### Exercise CH8-5

**`Enum` / `Bounded` で方位を時計回りに回す**

Exercise CH8-1 の `Direction` に `deriving (Show, Eq, Enum, Bounded)` を付け直し (`Show` は導出してよい), 時計回りに 90 度回した方位を返す関数 `turnRight :: Direction -> Direction` を実装してください. コンストラクタの順序は `North | East | South | West` (時計回り) とし, `West` の次は `North` に戻るよう **循環** させます. 手書きの 4 通り場合分けではなく, `succ` / `minBound` / `maxBound` を使ってください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (turnRight North)   -- East
  print (turnRight East)    -- South
  print (turnRight West)    -- North
  print [minBound .. maxBound :: Direction]  -- [North,East,South,West]
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data Direction = North | East | South | West
  deriving (Show, Eq, Enum, Bounded)

turnRight :: Direction -> Direction
turnRight d
  | d == maxBound = minBound
  | otherwise     = succ d

main :: IO ()
main = do
  print (turnRight North)   -- East
  print (turnRight East)    -- South
  print (turnRight West)    -- North
  print [minBound .. maxBound :: Direction]  -- [North,East,South,West]
~~~

本文の `nextDay` と同じイディオムです. `d == maxBound` (= `West`) のときだけ `minBound` (= `North`) に巻き戻し, それ以外は `succ` で次のコンストラクタへ進めます. 循環の境界処理を `Bounded` の端点で書けるのがポイントです.

</details>

:::

## Num — 数値リテラルの正体

ここまで `3` や `7` といった **数値リテラル** を当たり前に使ってきました. では `3` の型は何でしょうか. 実は Haskell の数値リテラルは **多相** で, `3 :: Num a => a` という型を持ちます. コンパイラは `3` を **`fromInteger 3`** と読み替えます. `fromInteger :: Num a => Integer -> a` は「整数から各数値型の値を作る」メソッドで, 文脈が要求する `Num` のインスタンス (`Int`, `Integer`, `Double`, …) に化けます. だから同じ `3` が `Int` にも `Double` にもなれるのです.

[第4章](fp4.html)で見た `No instance for (Num String)` や `No instance for (Fractional Int)` というエラーは, まさにこの仕組みの裏返しでした. リテラルが要求する `Num` / `Fractional` のインスタンスがその型に無かったために起きていたのです.

`Num` は次のメソッドを束ねた型クラスです.

~~~ haskell
class Num a where
  (+), (-), (*) :: a -> a -> a
  negate        :: a -> a
  abs, signum   :: a -> a
  fromInteger   :: Integer -> a   -- 数値リテラルはこれで作られる
~~~

普段は `Int` / `Integer` / `Double` などの組込の `Num` インスタンスを使うだけですが, **自作の型にも `Num` を与えれば数値リテラルが書けます**. [第7章](fp7.html)で作った自然数 `Nat` に `Num` インスタンスを与えてみましょう.

~~~ haskell
data Nat = Zero | Succ Nat deriving (Show, Eq)

instance Num Nat where
  -- 数値リテラルの変換: 0 を Zero に, n を Succ の n 段重ねにする
  fromInteger n
    | n == 0    = Zero
    | n > 0     = Succ (fromInteger (n - 1))
    | otherwise = error "Nat: 負の整数は表せない"
  -- 足し算・掛け算
  Zero   + m = m
  Succ n + m = Succ (n + m)
  Zero   * _ = Zero
  Succ n * m = m + n * m
  -- 絶対値 abs はそのまま (ℕ は非負なので符号を外す操作は不要)
  abs    n    = n
  -- signum は符号を返すメソッド (正→1, 0→0, 負→-1). ℕ には負がないので
  -- Zero なら 0, それ以外は 1 (= Succ Zero) のみ
  signum Zero = Zero
  signum _    = Succ Zero
  -- 加法逆元 (負の数) は ℕ に存在しない
  negate _ = error "Nat: 負の数は表せない"

main :: IO ()
main = do
  print (3 :: Nat)             -- Succ (Succ (Succ Zero))
  print (2 + 1 == (3 :: Nat))  -- True
  print (2 * 3 == (6 :: Nat))  -- True
~~~

`3 :: Nat` と書けるようになり, その正体が `Succ (Succ (Succ Zero))` であることが `print` の結果から見えます —— 数値リテラルが `fromInteger` で展開される様子をそのまま観察できるわけです.

ここで `negate` (符号反転) を **エラー** にした点に注目してください. ℕ には「足して `Zero` に戻す相手 (加法逆元)」が `Zero` 以外に存在しないため, `negate` は意味を持てません. このことは, 後の **群** の節で「自然数の加法 $(\mathbb{N}, +, 0)$ はモノイドだが **群ではない**」と述べることと, ちょうど対応しています.

# 代数とクラス

ここからは, 型クラスが数学の **代数構造** とどう対応するかを見ます.

**代数構造 (algebraic structure)** とは, おおまかには次の 3 つの組です.

1. **台集合 (carrier set)**: 対象となる値の集合 (= Haskell の型).
2. **演算 (operation)**: 集合の要素どうしを組み合わせる規則 (= 型クラスのメソッド).
3. **法則 (law)**: 演算が満たすべき性質 (= 規約. コンパイラは強制しない).

数学では, この「台集合とその上の演算 (と特別な元)」をまとめて **1 つの組 (tuple)** として書きます. たとえば二項演算 $\bullet$ を 1 つ持つ構造なら $(S, \bullet)$, さらに単位元 $e$ を持つなら $(S, \bullet, e)$ です. **この組に書き出すのは「材料」(台集合・演算・特別な元) だけ**で, 3 番目の **法則は組には現れません**. 同じ材料 $(S, \bullet)$ でも, 法則を課すか課さないかで別の構造 (後述のマグマと半群) になる, という点に注意してください.

この章では, **数学の組 → Haskell の型クラス** という 1 対 1 の対応を軸に進めます. 大まかには次の辞書です.

| 数学 (組の材料) | Haskell |
| --- | --- |
| 台集合 $S$ | 型 `a` |
| 二項演算 $\bullet$ | 型クラスのメソッド (`(<>)` など) |
| 単位元 $e$ | メソッド (`mempty`) |
| 法則 (結合律など) | 規約 (コンパイラは強制しない) |

[第7章](fp7.html)では「型 = 集合」と捉えました. その集合の上に「演算」を載せ, さらに「どんな法則を満たすか」で構造を分類したものが代数構造です. ここでは, 演算が満たす法則の **少ない順** に, マグマ → 半群 → モノイド → 群 と積み上げていきます. 組の **材料が増える** ($(S,\bullet) \to (S,\bullet,e) \to (S,\bullet,e,{}^{-1})$) のと, **法則が増える** のが同時に進みます.

| 構造 | 組 (材料) | 満たす法則 | 対応する型クラス |
| --- | --- | --- | --- |
| マグマ | $(S, \bullet)$ | 閉じている (だけ) | (標準には無い) |
| 半群 | $(S, \bullet)$ | 結合律 | `Semigroup` |
| モノイド | $(S, \bullet, e)$ | 結合律 + 単位元律 | `Monoid` |
| 群 | $(S, \bullet, e, {}^{-1})$ | 結合律 + 単位元律 + 逆元律 | (標準には無い) |

マグマと半群は組 (材料) が同じ $(S, \bullet)$ で, 違いは **法則 (結合律) を課すかどうか** だけです. モノイドで単位元 $e$, 群で逆元 ${}^{-1}$ と, 下に行くほど組の材料が増えていきます.

## マグマ

最も法則の少ない代数構造が **マグマ (magma)** です. 台集合 $S$ と二項演算 $\bullet : S \times S \to S$ の組 $(S, \bullet)$ で, 要求されるのは **演算が集合の中で閉じている** (結果がまた $S$ の要素になる) ことだけです. 結合律も単位元も要求しません.

Haskell の標準ライブラリにマグマのクラスはありませんが, この二項演算 $\bullet$ に対応する演算子 `(|*|)` をメソッドに持つクラスを学習用に定義してみましょう. 組 $(S, \bullet)$ の **台集合 $S$ が型変数 `a`**, **演算 $\bullet$ がメソッド `(|*|)`** に対応します.

~~~ haskell
class Magma a where
  (|*|) :: a -> a -> a   -- 数式の · に対応する閉じた二項演算 (法則は要求しない)
~~~

身近なマグマの例が **じゃんけん** です. 台集合を $S = \{\text{グー}, \text{チョキ}, \text{パー}\}$ とし, 「2 手を出して勝った方を返す」演算 $a \bullet b$ を考えます (あいこなら左を返すことにします). たとえば $\text{グー} \bullet \text{チョキ} = \text{グー}$ です. この組 $(\{\text{グー}, \text{チョキ}, \text{パー}\}, \bullet)$ を Haskell に写すと, 台集合が型 `RPS`, 演算 $\bullet$ が `(|*|)` になります.

~~~ haskell
data RPS = Rock | Paper | Scissors deriving (Show, Eq)

-- (|*|) が数式の · に対応する
instance Magma RPS where
  Rock     |*| Scissors = Rock      -- グーはチョキに勝つ
  Scissors |*| Rock     = Rock
  Paper    |*| Rock     = Paper     -- パーはグーに勝つ
  Rock     |*| Paper    = Paper
  Scissors |*| Paper    = Scissors  -- チョキはパーに勝つ
  Paper    |*| Scissors = Scissors
  a        |*| _        = a         -- あいこは左を返す
~~~

`(|*|)` は 2 つの `RPS` から `RPS` を返すので, 確かに `RPS` の中で閉じています. これでマグマの条件は満たします. しかしこの演算は **結合律を満たしません**. すなわち, ある $a, b, c$ で $(a \bullet b) \bullet c \neq a \bullet (b \bullet c)$ となります.

~~~ haskell
-- (Rock |*| Paper) |*| Scissors
--   = Paper |*| Scissors = Scissors
-- Rock |*| (Paper |*| Scissors)
--   = Rock |*| Scissors  = Rock
-- Scissors /= Rock  なので結合律は成り立たない
~~~

「グーにパーで勝ち, それにチョキで勝つ」と「パーにチョキで勝ち, それにグーが向かう」とで結果が変わってしまうわけです. このように, **閉じてはいるが結合律を満たさない** のがマグマの典型例です.

## 半群 (Semigroup)

マグマに **結合律 (associativity)** を加えたものが **半群 (semigroup)** です. 組としてはマグマと同じ $(S, \bullet)$ のまま (新しい材料は増えません) で, 変わるのは **課す法則** だけです. 結合律とは, 二項演算 $\bullet$ が **すべての** $x, y, z$ について

$$\forall x, y, z.\ \ (x \bullet y) \bullet z = x \bullet (y \bullet z)$$

を満たすことです. 先頭の $\forall$ (= すべての) は [第7章](fp7.html) で導入した **全称命題** で, 「どの 3 要素をとっても成り立つ等式」を要求しています. 結合律が成り立つと「演算する順序 (どこから括弧でくくるか) を気にしなくてよい」ため, $x \bullet y \bullet z$ と括弧なしで書けます.

Haskell では半群は `Semigroup` クラスで表され, 演算子は `(<>)` です.

~~~ haskell
class Semigroup a where
  (<>) :: a -> a -> a   -- 結合律を満たすことが規約
~~~

例として, 整数の上の演算「2 つの整数の **大きい方** を返す」$a \sqcup b = \max(a, b)$, すなわち組 $(\mathbb{Z}, \sqcup)$ を考えます. `max` は結合律 $(a \sqcup b) \sqcup c = a \sqcup (b \sqcup c)$ を満たす (どこから比べても最大は同じ) ので半群になります. まず数式の $\sqcup$ に対応する 2 項演算子 `(|+|)` を定義し, それを使って `Semigroup` の `(<>)` を与えます (台集合 $\mathbb{Z}$ は型 `Max` で表します).

~~~ haskell
newtype Max = Max Integer deriving (Show, Eq)

-- (|+|) が数式の ⊔ (= max) に対応する
(|+|) :: Max -> Max -> Max
Max a |+| Max b = Max (max a b)

-- <> は (|+|) そのものとして定義する
instance Semigroup Max where
  (<>) = (|+|)

main :: IO ()
main = do
  print (Max 3 |+| Max 7)           -- Max 7
  print (Max 3 <> Max 7)            -- Max 7   (<> は |+| と同じ)
  print (Max 3 <> Max 7 <> Max 5)   -- Max 7
~~~

`(<>) = (|+|)` と定義したので, `<>` と `(|+|)` はまったく同じ演算です. `(Max 3 <> Max 7) <> Max 5` でも `Max 3 <> (Max 7 <> Max 5)` でも結果は `Max 7` で一致します. これが結合律 $(a \sqcup b) \sqcup c = a \sqcup (b \sqcup c)$ です.

::: warn
`Semigroup` クラスはメソッドのシグネチャを定めるだけで, **結合律が成り立つかどうかをコンパイラは検査しません**. 結合律を満たさない `(<>)` を書いてもコンパイルは通ってしまいます. 法則を守るのはプログラマの責任です (本章末のコラム「法則を Haskell でどう守るか」で QuickCheck による確認方法を扱います).
:::

`Max` には「これと演算しても相手を変えない値」, すなわち **単位元** が **ありません**. 台集合 $\mathbb{Z}$ には最小元がなく, どんな整数よりも小さい値が存在しないため, `max` に対する単位元を作れないのです. このように一般の半群には単位元があるとは限らず, 単位元の有無が次のモノイドとの違いになります.

## モノイド (Monoid)

半群に **単位元 (identity element)** を加えたものが **モノイド (monoid)** です. ここで初めて組に新しい材料が増え, $(S, \bullet)$ から $(S, \bullet, e)$ になります ($e$ が単位元). 単位元とは, 「すべての $x$ と演算しても相手を変えない要素 $e$ が **存在する**」という条件で, [第7章](fp7.html) の **存在命題** を使って

$$\exists e.\ \forall x.\ \ e \bullet x = x \bullet e = x$$

と書けます. ここで $\exists e$ が $\forall x$ の **外側** にある点が大切で, 「(全体で) たった 1 つの $e$ が, すべての $x$ に効く」ことを表します. 「何もしない」値, と考えるとよいでしょう (足し算の $0$, 掛け算の $1$ がこれにあたります).

Haskell では `Monoid` クラスで表され, 単位元を `mempty` という名前で与えます.

~~~ haskell
class Semigroup a => Monoid a where
  mempty :: a   -- 単位元
~~~

`class Semigroup a => Monoid a` とあるように, `Monoid` は `Semigroup` を **スーパークラス** に持ちます. 「単位元付きの半群」がモノイドなので, まず半群 (`<>`) であることが前提になるわけです.

整数は, **同じ台集合の上に 2 つのモノイド構造** を持ちます. 足し算の組 $(\mathbb{Z}, +, 0)$ と掛け算の組 $(\mathbb{Z}, \times, 1)$ です. ここが組記法の効きどころで, **台集合 $\mathbb{Z}$ だけでは構造は決まらず**, どの演算 $\bullet$ と単位元 $e$ を選ぶかまで指定して初めて 1 つのモノイドになります. $(\mathbb{Z}, +, 0)$ と $(\mathbb{Z}, \times, 1)$ は数学的に **別の対象** です.

この 2 つの組がそれぞれ **モノイドの条件 (結合律・単位元律) を満たすこと** を確認しておきましょう.

**加法の組 $(\mathbb{Z}, +, 0)$ がモノイドであること.**

- 結合律: $\forall a, b, c \in \mathbb{Z}.\ (a + b) + c = a + (b + c)$. これは整数の加法がもともと満たす性質 (加法の結合法則) なので成り立ちます.
- 単位元律: $e = 0$ とおくと $\forall a \in \mathbb{Z}.\ a + 0 = 0 + a = a$. すなわち $0$ はどの $a$ に足しても $a$ を変えないので, 単位元の条件 $\exists e.\ \forall a.\ e + a = a + e = a$ を満たします.

両方を満たすので $(\mathbb{Z}, +, 0)$ はモノイドです.

**乗法の組 $(\mathbb{Z}, \times, 1)$ がモノイドであること.**

- 結合律: $\forall a, b, c \in \mathbb{Z}.\ (a \times b) \times c = a \times (b \times c)$. これも整数の乗法の結合法則として成り立ちます.
- 単位元律: $e = 1$ とおくと $\forall a \in \mathbb{Z}.\ a \times 1 = 1 \times a = a$. すなわち $1$ が単位元です.

両方を満たすので $(\mathbb{Z}, \times, 1)$ もモノイドです.

整数の加法・乗法が結合的であること自体は, 整数がもともと備える基本性質 (環の公理) として認めます. ここで本質的なのは, **同じ台集合 $\mathbb{Z}$ でも演算と単位元の組を取り替えると別のモノイドになる**こと, そして単位元が $0$ と $1$ で異なることです (前者を整数環の **加法モノイド**, 後者を **乗法モノイド** と呼びます).

この「台集合は同じだが構造が違う」状況が, Haskell の `newtype` の動機そのものです. 1 つの型に `Monoid` インスタンスは 1 つしか付けられないので, **任意精度整数 `Integer`** (有界な `Int` と違い, 数学の $\mathbb{Z}$ をそのまま表せます) を 2 通りに包み分けて, 組ごとに別の型 (`Add` = $(\mathbb{Z},+,0)$, `Mul` = $(\mathbb{Z},\times,1)$) を与えます.

~~~ haskell
-- 加法のモノイド:  a ⊕ b = a + b,  単位元 e = 0
newtype Add = Add Integer deriving (Show, Eq)
(.+.) :: Add -> Add -> Add
Add a .+. Add b = Add (a + b)
instance Semigroup Add where (<>)   = (.+.)   -- <> = ⊕
instance Monoid    Add where mempty = Add 0   -- e  = 0

-- 乗法のモノイド:  a ⊗ b = a × b,  単位元 e = 1
newtype Mul = Mul Integer deriving (Show, Eq)
(.*.) :: Mul -> Mul -> Mul
Mul a .*. Mul b = Mul (a * b)
instance Semigroup Mul where (<>)   = (.*.)   -- <> = ⊗
instance Monoid    Mul where mempty = Mul 1   -- e  = 1

main :: IO ()
main = do
  print (Add 3 .+. Add 4)             -- Add 7
  print (Add 3 <> mempty)             -- Add 3   (単位元と演算しても変わらない)
  print (mconcat [Add 1, Add 2, Add 3])  -- Add 6
  print (Mul 3 .*. Mul 4)             -- Mul 12
  print (mconcat [Mul 1, Mul 2, Mul 3])  -- Mul 6
~~~

`Add 3 <> mempty` が `Add 3` のまま変わらないのが単位元律です. また, モノイドには `mconcat :: [a] -> a` という「リストの要素を `<>` で畳み込む」関数が用意されています. `mconcat [Add 1, Add 2, Add 3]` は `Add 1 <> Add 2 <> Add 3 <> mempty` を計算して `Add 6` を返します. 空リストに対しては `mconcat [] = mempty` となり, 単位元が「空の畳み込みの結果」として効いてきます.

::: note
ここで定義した `Add` / `Mul` に相当する型は, 標準ライブラリの `Data.Monoid` に `Sum` / `Product` として用意されています. 実務ではこれらを使えば十分ですが, 中身は上で書いたものと同じです.
:::

::: note

### Exercise CH8-6

**統計量を集計するモノイド `Stats`**

データを 1 つずつ畳み込んで「個数」と「合計」を同時に集計する型 `Stats` を作り, モノイドにします.

1. `Stats` を, 個数 `statCount :: Int` と合計 `statSum :: Int` を持つレコード型として定義してください (`deriving (Show, Eq)`).

2. `Semigroup` インスタンスを定義してください. 2 つの `Stats` を合成するときは, 個数どうし・合計どうしをそれぞれ足します.

3. `Monoid` インスタンスを定義してください. 単位元は「個数 0, 合計 0」です.

4. 整数 1 つを `Stats` 1 件分 (件数 1・その値が合計) にする関数 `singleton :: Int -> Stats` を定義し, `mconcat (map singleton xs)` でリスト `xs` の件数と合計をまとめて求められることを確認してください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (singleton 10 <> singleton 20)          -- Stats {statCount = 2, statSum = 30}
  print (mconcat (map singleton [1,2,3,4]))     -- Stats {statCount = 4, statSum = 10}
  print (mconcat (map singleton []) )           -- Stats {statCount = 0, statSum = 0}
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data Stats = Stats { statCount :: Int, statSum :: Int }
  deriving (Show, Eq)

instance Semigroup Stats where
  Stats c1 s1 <> Stats c2 s2 = Stats (c1 + c2) (s1 + s2)

instance Monoid Stats where
  mempty = Stats 0 0

singleton :: Int -> Stats
singleton n = Stats 1 n

main :: IO ()
main = do
  print (singleton 10 <> singleton 20)     -- Stats {statCount = 2, statSum = 30}
  print (mconcat (map singleton [1,2,3,4])) -- Stats {statCount = 4, statSum = 10}
  print (mconcat (map singleton []))       -- Stats {statCount = 0, statSum = 0}
~~~

各データを `singleton n = Stats 1 n` (件数 1・その値が合計) で「1 点分の要約」に持ち上げ, モノイドの畳み込み `mconcat` でまとめて集計しています. `singleton` という名前は, `Data.Set.singleton` や `Data.Map.singleton` と同じく「要素 1 つからその構造を作る」関数を表す慣用です. 空リストのときは `mempty = Stats 0 0` が結果になり, 単位元が「集計の初期値」として効きます. このように, 集計処理をモノイドで表すと「1 件への持ち上げ (`singleton`)」と「畳み込み (`mconcat`)」に分離でき, 平均 (`statSum / statCount`) なども後から組み立てられます.

</details>

:::

## ブール代数

「1 つの台集合の上に 2 つのモノイド構造が載る」例は, 整数の (加法・乗法) だけではありません. **真偽値** の上にも, **論理積 $\wedge$** と **論理和 $\vee$** という 2 つのモノイドが載ります.

ここでは Haskell 組込の `Bool` を使わず, [第7章](fp7.html)で学んだ **列挙型** として真偽値を自分で定義してみましょう. 真 `T` と偽 `F` の 2 値からなる型 `Bool2` です.

~~~ haskell
data Bool2 = T | F deriving (Show, Eq)

-- 論理積 ∧ に対応する演算子 (.&.):  a ∧ b
(.&.) :: Bool2 -> Bool2 -> Bool2
T .&. T = T
_ .&. _ = F

-- 論理和 ∨ に対応する演算子 (.|.):  a ∨ b
(.|.) :: Bool2 -> Bool2 -> Bool2
F .|. F = F
_ .|. _ = T

-- 否定 ¬ は 1 項演算なので関数のまま:  ¬a
not2 :: Bool2 -> Bool2
not2 T = F
not2 F = T
~~~

この台集合 $\{\text{T}, \text{F}\}$ の上にも, 整数のときと同じく 2 つのモノイドの組が載ります.

- 論理積の組 $(\{\text{T}, \text{F}\}, \wedge, \text{T})$: 演算 $\wedge$ (`(.&.)`), 単位元 `T` ($\text{T} \wedge x = x$).
- 論理和の組 $(\{\text{T}, \text{F}\}, \vee, \text{F})$: 演算 $\vee$ (`(.|.)`), 単位元 `F` ($\text{F} \vee x = x$).

整数の $(\mathbb{Z}, +, 0)$ / $(\mathbb{Z}, \times, 1)$ とまったく同じ構図で, 台集合は共通でも組が違えば別のモノイドです. よって 1 つの型に `Monoid` は 1 つしか付けられない以上, `newtype` で 2 通りに包み分けます (組込の `Bool` に対する同じ構造が, `Data.Monoid` に `All` / `Any` として実在します).

~~~ haskell
-- 論理積のモノイド: 全部 T なら T  (<> = ∧)
newtype All2 = All2 { getAll2 :: Bool2 } deriving (Show, Eq)
instance Semigroup All2 where All2 a <> All2 b = All2 (a .&. b)
instance Monoid    All2 where mempty = All2 T

-- 論理和のモノイド: 1 つでも T なら T  (<> = ∨)
newtype Any2 = Any2 { getAny2 :: Bool2 } deriving (Show, Eq)
instance Semigroup Any2 where Any2 a <> Any2 b = Any2 (a .|. b)
instance Monoid    Any2 where mempty = Any2 F

main :: IO ()
main = do
  print (T .&. F)                             -- F
  print (T .|. F)                             -- T
  print (not2 T)                              -- F
  print (getAll2 (All2 T <> All2 F))          -- F
  print (getAll2 (mconcat [All2 T, All2 T]))  -- T
  print (getAll2 (mempty :: All2))            -- T   (∧ の単位元)
  print (getAny2 (Any2 F <> Any2 T))          -- T
  print (getAny2 (mempty :: Any2))            -- F   (∨ の単位元)
~~~

`All2` を `mconcat` で畳み込めば「全要素が真か」, `Any2` なら「1 つでも真か」を計算できます. これは `and` / `or` といった標準関数の代数的な正体でもあります.

この $(\wedge, \vee)$ の 2 つのモノイドは, 互いに無関係ではなく **分配律** や **吸収律**, さらに各要素の **補元 (否定 $\neg$)** で結びついています. このように, 2 つのモノイド演算 $\wedge, \vee$ と補元 $\neg$, および両端 (最大元 `T`・最小元 `F`) を備え, 次の法則を満たす代数構造を **ブール代数 (Boolean algebra)** といいます.

- **両演算がモノイド (可換)**: $\wedge$ は単位元 `T`, $\vee$ は単位元 `F`.
- **分配律**: $x \wedge (y \vee z) = (x \wedge y) \vee (x \wedge z)$, および $\vee$ と $\wedge$ を入れ替えたもの.
- **補元律**: $x \wedge \neg x = \text{F}$, $x \vee \neg x = \text{T}$.

ブール代数は, このように **2 つのモノイドが対になって協調する** 代数構造の代表例です. 真偽値の論理だけでなく, [第7章](fp7.html)で扱った **集合演算** (積集合 $\cap$ が $\wedge$, 和集合 $\cup$ が $\vee$, 補集合が $\neg$ に対応) も同じ法則を満たすブール代数になっています.

::: note
**発展: 束 (lattice) とブール代数**

ブール代数は, より一般的な **束 (lattice)** という代数構造の一種です. 束とは, 集合 $L$ と 2 つの二項演算 **交わり (meet) $\wedge$** ・ **結び (join) $\vee$** の組で, 任意の $a, b, c \in L$ について次を満たすものです.

- **結合律**: $(a \wedge b) \wedge c = a \wedge (b \wedge c)$ および $(a \vee b) \vee c = a \vee (b \vee c)$
- **交換律**: $a \wedge b = b \wedge a$ および $a \vee b = b \vee a$
- **吸収律**: $a \wedge (a \vee b) = a$ および $a \vee (a \wedge b) = a$

(吸収律から冪等律 $a \wedge a = a$, $a \vee a = a$ も導けます.)

束は **順序** を使っても等価に定義できます. すなわち, **半順序集合** $(L, \le)$ のうち, 任意の 2 元 $a, b$ に対して **下限** $a \wedge b$ (両者以下の元のなかで最大のもの) と **上限** $a \vee b$ (両者以上の元のなかで最小のもの) が常に存在するもの, と言っても同じです. ここで順序は **半順序** (任意の 2 元が比較できるとは限らない) でよく, `Ord` の **全順序** ($\forall x, y.\ (x \le y) \lor (y \le x)$) より弱い条件である点に注意してください.

束に条件を足していくと, ブール代数にたどり着きます.

- **有界束**: 最大元 $\top$ と最小元 $\bot$ をもつ束. これらは $\wedge$ / $\vee$ の **単位元** になり, $(L, \wedge, \top)$ と $(L, \vee, \bot)$ がそれぞれモノイドになります (本節で見た 2 つのモノイドはこれです).
- **分配束**: 分配律 $a \wedge (b \vee c) = (a \wedge b) \vee (a \wedge c)$ (および $\wedge, \vee$ を入れ替えたもの) を満たす束.
- **可補束**: 各元 $a$ に **補元** $\neg a$ ($a \wedge \neg a = \bot$ かつ $a \vee \neg a = \top$) が存在する有界束.

そして, **有界・分配・可補をすべて満たす束がブール代数**です. `Bool2` では $\wedge$ / $\vee$ / 最大元 $\top =$ `T` / 最小元 $\bot =$ `F` / 補元 $=$ `not2` がそれぞれ対応します. また [第7章](fp7.html) で扱った **部分集合の全体 (べき集合)** も, 包含 $\subseteq$ を順序, 積集合 $\cap$ を交わり, 和集合 $\cup$ を結び, 補集合を補元とするブール代数の代表例です. 束のより進んだ理論や応用は本講義の範囲を超えるため, ここでは「2 つのモノイドが順序を介して協調する構造」という理解で十分です.
:::

## 群 (Group)

モノイドに **逆元 (inverse element)** を加えたものが **群 (group)** です. 組はさらに伸びて $(S, \bullet, e, {}^{-1})$ になります (${}^{-1}$ が「各元にその逆元を返す」操作). 逆元とは, 「**各要素** $x$ に対して, 演算で打ち消して単位元に戻せる相手 $y$ が **存在する**」という条件で, 全称命題と存在命題を組み合わせて

$$\forall x.\ \exists y.\ \ x \bullet y = y \bullet x = e \quad (e \text{ は単位元})$$

と書けます (この $y$ を $x^{-1}$ と書きます). ここでは $\exists y$ が $\forall x$ の **内側** にあり, 「$x$ **ごとに** 別の $y$ を選んでよい」ことを表します.

モノイドの単位元 $\exists e.\ \forall x$ と, 群の逆元 $\forall x.\ \exists y$ では **量化子の順序が逆** です. この違いが, 「(全体で) 1 つの単位元」と「(元ごとに) それぞれの逆元」という差を生みます.

整数の加法 $(\mathbb{Z}, +, 0)$ は群です. 各 $n$ の逆元は $-n$ で, $n + (-n) = 0$ となります. 一方, 整数の乗法 $(\mathbb{Z}, \times, 1)$ はモノイドですが **群ではありません**. たとえば $2$ の逆元 $\frac{1}{2}$ は整数ではないからです. また, 自然数の加法 $(\mathbb{N}, +, 0)$ もモノイドですが, 負の数がないので群にはなりません. 本章の **Num** 節で `Nat` の `negate` をエラーにしたのは, まさにこの加法逆元の非存在を表しています.

Haskell の標準ライブラリに群のクラスはないので, モノイドを拡張する形で学習用に定義します. 組の 4 つ目の材料 ${}^{-1}$ が, メソッド `invert` に対応します.

~~~ haskell
class Monoid a => Group a where
  invert :: a -> a   -- 逆元 (組の 4 つ目の材料) を返す
~~~

有限の群の例として, 「3 で割った余り」の世界 $\mathbb{Z}/3\mathbb{Z} = \{0, 1, 2\}$ における加法 (これを $\oplus$ と書きます), すなわち組 $(\mathbb{Z}/3\mathbb{Z}, \oplus, 0, {}^{-1})$ を考えます.

この群を数式で定義すると, 下の Haskell 実装と次のように 1 対 1 で対応します.

- **台集合** $\mathbb{Z}/3\mathbb{Z} = \{0, 1, 2\}$: 型 `Z3` の `Z0` / `Z1` / `Z2`.
- **演算** $\oplus$ (`(.+.)`, すなわち `(<>)`): 整数として足してから 3 で割った余りをとる.

$$a \oplus b = (a + b) \bmod 3$$

- **単位元** $e = 0$ (`mempty = Z0`): $a \oplus 0 = (a + 0) \bmod 3 = a$ で, 何も動かさない元.
- **逆元** $-a$ (`invert`): 各 $a$ に対して

$$-a = (3 - a) \bmod 3$$

で定めます. 具体的には $-0 = 0,\ -1 = 2,\ -2 = 1$ です.

**なぜ $(3 - a) \bmod 3$ が逆元になるのか.** 逆元とは「$a$ と演算すると単位元 $0$ に戻る相手」, すなわち $a \oplus (-a) = 0$ を満たす元でした. 実際に計算すると

$$a \oplus (-a) = \big(a + (3 - a)\big) \bmod 3 = 3 \bmod 3 = 0 = e$$

となり, どの $a$ でも単位元 $0$ に戻ります ($\oplus$ は可換なので $(-a) \oplus a = 0$ も成り立ちます). 直感的には **3 時間で 1 周する時計** を思い浮かべてください. 文字盤を $a$ だけ進めたあと残り $3 - a$ だけ進めれば, 合わせて $a + (3 - a) = 3$ でちょうど 1 周し, この世界では $3 \equiv 0$ なので出発点 $0$ に戻ります.

外側の $\bmod 3$ が効くのは $a = 0$ のときです. $3 - 0 = 3$ は台集合 $\{0, 1, 2\}$ からはみ出すため, $3 \bmod 3 = 0$ と折り返して台集合に戻します (これが `invert` 内の `mod 3` に対応します). 結果, $0$ の逆元は $0$ 自身になります.

以上の数式を, [第7章](fp7.html)で学んだ **列挙型** にそのまま写したのが次のコードです.

~~~ haskell
data Z3 = Z0 | Z1 | Z2 deriving (Show, Eq, Enum, Bounded)

-- 群の演算 ⊕:  a ⊕ b = (a + b) mod 3
(.+.) :: Z3 -> Z3 -> Z3
a .+. b = toEnum ((fromEnum a + fromEnum b) `mod` 3)

instance Semigroup Z3 where (<>)   = (.+.)  -- <> = ⊕
instance Monoid    Z3 where mempty = Z0     -- 単位元 e = 0
instance Group     Z3 where
  invert a = toEnum ((3 - fromEnum a) `mod` 3)  -- 逆元 -a = (3 - a) mod 3

main :: IO ()
main = do
  print (Z1 .+. Z2)         -- Z0   (1 + 2 = 3 ≡ 0)
  print (Z2 .+. Z2)         -- Z1   (2 + 2 = 4 ≡ 1)
  print (invert Z1)         -- Z2   (1 の逆元は 2)
  print (Z1 <> invert Z1)   -- Z0   (元と逆元の演算は単位元)
~~~

`fromEnum` で列挙型を整数に直し, `mod 3` で「3 で割った余り」の世界に閉じ込め, `toEnum` で再び `Z3` に戻しています. `Z1 <> invert Z1` が単位元 `Z0` に戻るのが逆元律です. このように, [第7章](fp7.html)の列挙型に演算と法則を載せることで, 有限群という代数構造をそのまま表現できます.

::: note
ここまでの マグマ → 半群 → モノイド → 群 は, 組の **材料** と **法則** を 1 つずつ足して構造を強くしていく階層になっています. 組が伸びる ($(S,\bullet) \to (S,\bullet,e) \to (S,\bullet,e,{}^{-1})$) のと, 法則が積み上がるのが対応します.

- **マグマ** $(S, \bullet)$: 閉じた演算だけ (じゃんけん)
- **半群** $(S, \bullet)$: + 結合律 (`Max`) — 組は同じ, 法則だけ追加
- **モノイド** $(S, \bullet, e)$: + 単位元 (`Add` / `Mul`, `All2` / `Any2`)
- **群** $(S, \bullet, e, {}^{-1})$: + 逆元 ($\mathbb{Z}$ の加法, `Z3`)

`Semigroup` → `Monoid` のスーパークラス関係は, この「半群に単位元を足すとモノイド」という数学的な階層を, そのまま型クラスの継承関係として写したものです.
:::
::: note
**コラム: 法則を Haskell でどう守るか — QuickCheck**

型クラスのインスタンスを書くとき, 結合律や単位元律といった **法則はコンパイラに強制されません** (シグネチャさえ合えば, 法則を破る実装でもコンパイルは通ります). ではどう守るのか. Haskell で広く使われるのが **QuickCheck** による **性質ベーステスト (property-based testing)** です.

通常のテストが「特定の入力に対する期待値」を 1 つずつ書くのに対し, QuickCheck は **法則を $\forall$ つきの性質として書き**, ランダムな入力を多数自動生成して反例を探します. たとえば本章のモノイド `Add` の結合律と単位元律は次のように書けます.

~~~ haskell
import Test.QuickCheck

-- Add は前掲のモノイド. ランダム生成のため Arbitrary を与える
instance Arbitrary Add where
  arbitrary = Add <$> arbitrary

-- 結合律:  ∀ x y z. (x <> y) <> z == x <> (y <> z)
prop_assoc :: Add -> Add -> Add -> Bool
prop_assoc x y z = (x <> y) <> z == x <> (y <> z)

-- 単位元律:  ∀ x. mempty <> x == x  かつ  x <> mempty == x
prop_identity :: Add -> Bool
prop_identity x = mempty <> x == x && x <> mempty == x
~~~

`prop_assoc` の 3 引数 `x y z` に付いた $\forall$ を QuickCheck が肩代わりし, `Add` の値をランダムに大量生成して等式を確かめます (ランダム生成には, その型の `Arbitrary` インスタンスが必要です). `quickCheck prop_assoc` を実行して

~~~ text
+++ OK, passed 100 tests.
~~~

と出れば, 100 通りの組で結合律の反例が見つからなかったことになります. 反例があれば QuickCheck はその具体値を表示します.

ただしこれは **テスト (反例探し)** であって **証明ではありません**. 「破れていないこと」を有限個の事例で確認しているだけで, すべての場合での成立を保証するわけではない点に注意してください (全称命題を有限のサンプルで確かめている, とも言えます). 法則を **証明** したい場合は Liquid Haskell や Agda/Coq といった証明系を使いますが, 本講義の範囲を超えます. なお, モノイドや半群など標準的な法則は `quickcheck-classes` パッケージが既製の検査 (`semigroupLaws` / `monoidLaws` 等) を提供しており, 自分で `prop_*` を書かずに済みます.
:::

# 代数のインスタンスにする利点

自分の型をわざわざ `Semigroup` / `Monoid` のインスタンスにすると, 何が嬉しいのでしょうか. 最大の利点は, **`(<>)` と `mempty` を前提に書かれた汎用関数が, 自分の型に対してそのまま使えるようになる** ことです. インスタンスを 1 度与えるだけで, 標準ライブラリの機能を「ただで」受け取れます.

## foldMap と Foldable

[第6章](fp6.html) では `foldr` / `foldl` を使い, `(+)` と初期値 `0` を渡して `[1,2,3]` を `6` に畳み込みました. このとき **「畳む関数」と「初期値」を自分で渡していた** ことを思い出してください. ここで, リストのように **要素を格納して畳み込める入れ物** を **コンテナ (container)** と呼びます. コンテナを束ねて扱う型クラスが `Foldable` で, リストはもちろん, 次章 [第9章](fp9.html) で自作する木 (`Tree`) や `Maybe` なども Foldable なコンテナです.

ここで Monoid が効いてきます. 型が Monoid なら **畳む関数は `(<>)`, 初期値は `mempty`** と最初から決まっているので, [第6章](fp6.html) のように毎回渡す必要がありません. これを使うのが `fold` と `foldMap` です.

~~~ haskell
-- 既に Monoid 値が詰まったコンテナを畳む
fold    :: (Foldable t, Monoid m) => t m -> m

-- 各要素を Monoid に変換してから畳む
foldMap :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
~~~

- `fold` は **すでに Monoid 値が詰まった** コンテナを `(<>)` で 1 つに畳みます. リストなら `fold = foldr (<>) mempty`, つまり [第6章](fp6.html) の `foldr` の「畳む関数」を `(<>)` に, 「初期値」を `mempty` に固定したものです.
- `foldMap f` は, 畳む前に **各要素を `f` で Monoid に変換** してから畳みます ([第6章](fp6.html) で `map` してから畳むのと同じ流れです). 両者には `fold = foldMap id` の関係があります.

`Add` を Monoid にしておけば, 整数のリストを合計に畳み込めます. (以降のコード片は, モノイド節で定義した `Add` と, `import Data.Foldable (fold)` / `import Data.Semigroup (stimes)` を前提とします. `foldMap` は標準で使えます.)

~~~ haskell
-- foldMap: 各 Int を Add に変換してから畳む
--   foldMap Add [1,2,3] = Add 1 <> Add 2 <> Add 3 = Add 6
total :: Add
total = foldMap Add [1, 2, 3]        -- Add 6

-- fold: すでに Add が詰まったコンテナをそのまま畳む
total' :: Add
total' = fold [Add 1, Add 2, Add 3]  -- Add 6
~~~

ポイントは, `Add` を **1 度** Monoid にすれば, `foldMap Add` がリストに限らず **あらゆる Foldable コンテナ** (`Maybe` や木, `Data.Map` など) に対して動くことです. 「どう集計するか」(`Add` の `(<>)` と `mempty`) と「何を走査するか」(コンテナ) が完全に分離されます.

## stimes — 同じ演算を n 回

`Semigroup` には, 同じ要素を $n$ 回 `(<>)` で繋ぐ `stimes` が用意されています.

~~~ haskell
stimes :: (Semigroup a, Integral b) => b -> a -> a
~~~

`stimes n x` は `x <> x <> ... <> x` ($x$ が $n$ 個) です. インスタンスを与えるだけでこれも使えます.

~~~ haskell
-- stimes 3 (Add 2) = Add 2 <> Add 2 <> Add 2 = Add 6
sixfold :: Add
sixfold = stimes 3 (Add 2)   -- Add 6

-- リストでも: stimes 3 [1] = [1] <> [1] <> [1] = [1,1,1]
ones :: [Int]
ones = stimes 3 [1]          -- [1,1,1]
~~~

しかも `stimes` の既定実装は **繰り返し二乗法** で動くため, 素朴に $n-1$ 回 `(<>)` するより高速です. これは「どの順序で括弧をくくっても結果が同じ」という結合律があるからこそ可能な最適化です.

## 結合律は並列化を可能にする

`(<>)` が結合律を満たすことは, 単に「括弧を省ける」だけではありません. $a \bullet b \bullet c \bullet d$ を $(a \bullet b) \bullet (c \bullet d)$ のように **好きな形に分割して計算してよい** ことを意味します. つまり大きなデータの集計を **分割統治・並列** で実行できます (大規模分散処理の MapReduce は, この「結合的な演算 = モノイド」で集計を分散させる考え方そのものです). 単位元 `mempty` は「空の断片」の集計結果として働きます.

## さらに先の利点 (予告)

モノイドの恩恵は後の章でも繰り返し現れます.

- **`Data.Map` の併合**: 値が `Semigroup` なら, マップ同士を `Map.unionWith (<>)` で賢く併合できます (例: 単語の出現回数 `Map String (Sum Int)` の集計と合算). `Map` 自体は [第9章](fp9.html) で扱います.
- **Writer モナド**: ログや集計を貯める `Writer w` は, 蓄積器 `w` が `Monoid` であることを要求します. 自分の型を Monoid にしておけば, 後のモナドの章でそのまま蓄積器として使えます.

このように, 「演算・単位元・法則」を 1 度宣言しておけば, それに乗る汎用機能の恩恵を継続的に受けられる — これが, 自分の型を代数構造のインスタンスとして与える最大の利点です.

なお, リストや木といった身近なデータ構造も, いずれもモノイドとして捉えられます. これらは型引数を持つ **多相データ型** でもあるため, 次章 [第9章](fp9.html) で多相データ型・関手 (Functor) と合わせて扱います.
