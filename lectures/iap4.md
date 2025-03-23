---
title: 代数プログラミング入門 Ch4 関数
description: 資料
tags:
    - algebra
    - lecture
    - haskell
featured: true
katex: true
date: 2024-10-18
tableOfContents: true
previousChapter: iap3.html
nextChapter: iap5.html
---

# 関数

Haskellは関数型言語なので,関数の記述がプログラミングにおける花形です. この章ではHaskellの関数に関する記法を学びましょう.

## 関数と演算子

関数型言語では関数を組み合わせてプログラムを書きます. 関数の正確な定義は後に譲るとして,ここでは取り敢えず｢特定のデータ型の値を受け取って,特定のデータ型の値を返すもの｣という定義にしましょう.このとき受け取る値を**引数**,返す値を**返り値**といいます.

Haskellでは,数学の記法と非常に近い方法で関数を定義します.
例えば,

$$
f : \mathbb{Z} \rightarrow \mathbb{Z} \\
f(x) = x + 1
$$

という,整数`x`を受け取って整数`x + 1`を返すだけの関数について考えましょう.

Haskellでは上の関数は以下のように定義されます.

~~~ haskell
f :: Int -> Int
f x = x + 1

main = do
    print $ f 4 --  5
~~~

`()`の代わりにスペースを使う点以外は全く同じ書き方で, `=`の左側に関数名と引数,右側に返り値を書きます.
関数名は小文字の英字で始めるというルールがあります.

`f :: Int -> Int`は型注釈であり,この`f`という関数が,引数に`Int`を取り,返り値として`Int`を返すということを指定しています.
型注釈は高度な処理をしない限り省略しても自動的にGHCが推論してくれますが,可読性のためにもできるだけ書くようにしましょう.

`do`以下の記述で, `f 4`の結果を確認しています. `print`は,文字列に変換可能な値を受け取り,標準出力する関数です. また `(f 4)`を省略して`$ f 4` としています.

引数は何個でも利用できます. 例えば2引数関数

$$ multiple(x,y) = x * y $$

は以下のように定義できます.

~~~ haskell
multiple :: Double -> Double -> Double
multiple x y = x * y
multiple 3 4

main = do
    print $ multiple 3 4 --  12.0
~~~

また,以下の記号を組み合わせて中置演算子名として利用することも可能です.

::: note
~ !  #  $  %  &  *  +  = .  /  < >  ?  @  \  ^  |  -
:::


~~~ haskell
(.*) :: Double -> Double -> Double
x .* y = x * y

main = do
    print $ 3 .* 4 --  12.0
~~~

絵文字などのUnicode記号も利用することができます.

~~~ haskell

(✖) :: Double -> Double -> Double
x ✖ y = x * y

main = do
    print $ 3 ✖ 4 --  12.0
~~~

記号を利用して関数を定義する場合には,定義時に`()` で囲うことで一般の関数のように定義することができます.
例えば, 乗算を新たに定義するとして,以下のように書くことができます.

~~~ haskell
(.*) :: Int -> Int -> Int
(.*) x y = x * y
main = do
    print $ 3 .* 4 --  12
~~~

前置の2引数関数も` `` ` (バッククオート)で囲むことで中置演算子として定義することができます.

~~~ haskell
x `multiple` y = x * y

main = do
    print $ 3 `multiple ` 4 --  12
~~~

### 結合性

先に述べたように異なる複数の演算子が連なっている式は, 演算子の優先順位に従って計算される順位が変わります.

例えば,`*` の優先順位は7で, `+` の優先順位は6なので,` 2 * 3 + 3` という式は,

~~~
   2 * 3  + 3
= (2 * 3) + 3
= 6 + 3
= 9
~~~

と言う風に`*`が優先して計算されます.

では,同じ演算子が複数回連なっている場合にはどのような順序で計算されるのでしょうか? このルールを決めるのが **結合性(Associativity)** です.

::: note

結合性には, **左結合(Left-associative)**, **右結合(Right-associative)**, **非結合(Non-associative)** の3種類があり,ユーザーが定義することができます.

- **左結合(Left-associative)**

左結合の場合, 演算子は左から右へと評価されます. 例えば, `+` は左結合であり,式 `a + b + c` は `(a + b) + c` として評価されます

- **右結合(Right-associative)**

右結合演算子の場合,演算子は右から左へと評価されます.例えば、`^`は右結合です。式 `a ^ b ^ c` は `a ^ (b ^ c)` として評価されます

- **非結合(Non-associative)**

非結合演算子は,同じ式内で連続して使用することは許されていません. 非結合演算子の例としては,比較演算子（`<`,`>` など）があります.

式 `a < b < c` は Haskell では文法的に不正です. 比較を連鎖させる場合は,`a < b && b < c`のように明確に分けて記述する必要があります.

:::

ユーザーが作成した演算子の結合性を指定するには,右,左,非の順に`infixr`,`infixl`,`infix`宣言を利用します. いずれも, `infix(r/l/なし) 優先順位 記号` の順に書きます.

例えば先程作成した,`.*` を右結合の優先順位7で指定するには,以下のように書きます.

~~~ haskell
x .* y = x * y
infixr 7 .*
~~~

## カリー化,部分適用

Haskellでは多引数関数を実装できることは先程確認しました. しかし,Haskellは**すべての関数が,引数を一つだけとる**という原則があります. これは,矛盾するようですが,この矛盾を解消する概念が**`カリー化(Currying)
`**です.

カリー化とは複数引数関数に対して,｢一つの引数を取り,次に残りの引数を取る関数を返すようにする変換｣です.

例として,以下のxとyを受け取りその和を返す関数`add`は

~~~ haskell
add :: Int -> Int -> Int
add x y = x + y
~~~

実際には

~~~ haskell
add :: Int -> (Int -> Int)
~~~

として機能しています. 関数の呼び出しは左結合なので,


`add 5 10 = (add 5) 10` であり, ここで`(add 5) :: Int -> Int`という新たな関数に2が適用されています.

~~~ sh
ghci> :{
ghci| add :: Int -> Int -> Int
ghci| add x y = x + y
ghci| :}
ghci> :t add
add :: Int -> Int -> Int
ghci> :t (add 5)
(add 5) :: Int -> Int
~~~

Haskellでは,標準で全ての関数がカリー化されており,これによって関数の複数の引数のうち一部だけを与えて,残りの引数を持つ関数を生成する**`部分適用(Partial Application)`**が可能となっています.

~~~ haskell
-- add関数を利用した部分適用
add5 :: Int -> Int
add5 = add 5

-- 実際の利用例
result = add5 10
~~~


## 分岐

関数型言語において,手続き型言語におけるIF文に相当するのが**パターンマッチ**と**指示関数(特性関数)**です.

### パターンマッチ

パターンマッチに近い概念は既にフィボナッチ数の漸化式として出てきています.フィボナッチ数の漸化式は,以下のように表されます.

::: note
$$
F_0 = 1 \\
F_1 = 1 \\
F_n = F_{n-1} + F_{n-2} (n >= 2)
$$
:::

この関数はPythonでは,以下のようにif文による分岐で記述されるのが一般的です.

~~~ python
def fib(x):
    if x == 0:
        return 1
    elif x == 1:
        return 1
    else:
        return fib(x-1) + f(x-2)

~~~
これをHaskellでパターンマッチを利用して以下のように定義することができます.

~~~ haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
~~~

このHaskellのコードは,

- 関数`fib`の引数が`0`のときには返り値として`1`を返し,

- 関数`fib`の引数が`1`のときには返り値として`1`を返し,

- 関数`fib`の引数が`それ以外`のときには返り値として`fib (n - 1) + fib (n - 2)`を返します.

この最後の`fib n = fib (n - 1) + fib (n - 2)`は再帰関数といって後ほど扱いますが,取り敢えず,特定の引数に対して特定の返り値を指定するこのような関数の記述方法を**パターンマッチ**といいます.

パターンマッチは,数値以外の引数に関しても適用可能であり,リストではリストの要素数に応じて使い分けることが多いです.

以下の,`strHead`関数は,リストの先頭の要素を文字列として表示する関数です.リストが空のときには`"Empty"`,要素が一つのときにはその要素,それ以外のときには先頭の要素を文字列にして返します.

`show`の詳細は後ほど扱いますが,どの様に標準出力に表示するかを定めてあるデータ型を文字列に変換する関数です.

::: warn
`Show a =>`の部分は任意のデータ型`a`が`show`を利用できるという制約を意味しており, **型クラス制約**といいます.
クラスの詳細に関しては後ほど扱います.
:::

~~~ haskell
strHead :: Show a => [a] -> String
strHead []     = "Empty"
strHead [x]    = show x
strHead (x:xs) = show x

main = do
    print $ strHead [] --  "Empty"
    print $ strHead [3,4] --  "3"
~~~

パターンマッチはこのようにリスト`x:xs`の先頭部分`x`を指定するなどの利用法が可能です. 値の特定の部分を取得する用法として頻出なのがタプルを引数に取るパターンマッチです.

以下のコードは,3つ組のタプル`(x,y,z)`から指定した位置の値を取り出す関数`getFromTuple`です.

~~~ haskell
getFromTuple (x,y,z) 0 = x
getFromTuple (x,y,z) 1 = y
getFromTuple (x,y,z) 2 = z
~~~

このような用法は後に紹介する代数的データ型を扱う際にも頻出します.


### ガード

数式における分岐は,指示関数を用いて行うこともできます.

::: note

$$
fib(n) =
\begin{cases}
1, ~if~n = 0 \\
1, ~if~n = 1 \\
fib(n-1) + fib(n-2),~if~n >=2
\end{cases}
$$
:::



Haskellにおいて指示関数の記法に相当するのが**ガード**です.

~~~ haskell
fib :: Int -> Int
fib n | n == 0 = 1
      | n == 1 = 1
      | n >= 2 = fib (n-1) + fib (n-2)

main = do
    print $ fib 5 --  8
~~~

特徴関数におけるifの位置が先に来ている以外は,基本的に対応関係にあるのがわかるかと思います.

### case式

パターンマッチをインデントブロックで実現する手法として**case式**があります. パターンマッチで判定する変数を`case 変数 of` のあとに指定して, それぞれのパターンとその結果を`->`でつなげる記法です. 指定のパターンに当てはまらないものすべて(これを**ワイルドカード**といいます)を指定するために`_`を利用します.

~~~ haskell
fib :: Int -> Int
fib n = case n of
        0 -> 1
        1 -> 1
        _ -> fib (n-1) + fib (n-2)

main = do
    print $ fib 5 --  8
~~~

ワイルドカードはどのような値に対しても同じ値を返す関数を実装する場合などにも利用されます.

~~~ haskell

return10 :: a -> Int
return10 _ = 10
~~~


### if式

Haskellにはifも存在します. `if 条件`に当てはまる場合の返り値を`then`で指定します. `else if` で条件を追加し, `else`でそれ以外のパターンを指定します. Pythonなどのif文と異なり,式なので`else`の場合の返り値も必ず指定する必要があります.


~~~ haskell
fib :: Int -> Int
fib n = if n == 0 then 1
        else if n == 1 then 1
        else fib (n-1) + fib (n-2)

main = do
    print $ fib 5 --  8
~~~

Haskellではあまりif式は利用されませんが,
1行で書けるため,式の中で部分的に利用する場合に便利です.

~~~ haskell
fib :: Int -> Int
fib n = if n == 0 then 1 else if n == 1 then 1 else fib (n-1) + fib (n-2)
~~~


## 再帰

Haskellにおいても**for文に相当する記法は存在します**が,基本的にループは**再帰**によって実装されます.
再帰とは関数内で自分自身を呼び出すことです. これまで何度も登場していた`fib`も再帰を利用していましたが,
もう少し細かく見てみましょう.

以下のPythonにおけるfor文を事例に考えてみましょう.

~~~ python
def total(xs):
    result = 0
    for x in xs:
        result += x
    return result
~~~

これと同値なプログラムをHaskellで記述すると以下のようになります.

~~~ haskell
total :: [Int] -> Int
total []  = 0
total [x] = x
total (x:xs) = x + (total xs)

main = print $ tatal [1..10] --  55
~~~

このtotal関数は,与えられたリストが空の場合0を返し,要素が一つの場合その要素を返します.
要素が複数あるリストの場合には,先頭の要素`x`をそれ以降の要素`xs`の合計に足すという処理を行います.

`total [1,2,3]`における処理の流れを追っていくと以下のようになります.

~~~
total [1,2,3]
= 1 + (total [2,3])
= 1 + (2 + (total [3]))
= 1 + 2 + 3 + (total [])
= 1 + 2 + 3 + 0
= 6
~~~

再帰の基本は,**ループの終了状態**をパターンマッチなどで指定して,そこに至るまでの状態の変化を再帰で記述することです.
処理がどのような状態になったら終わるのかを意識して記述しないと永遠に終了しないプログラムになるので注意しましょう.



::: note

練習問題

1. リストの長さを返す`length2 :: [a] -> Int` 関数を新しく実装してください.

2. 与えられた整数のリストを引数にとり,要素毎にFizzBuzzを実行した結果を文字列のリストで返す関数
`fizzBuzz :: [Int] -> [String]`実装してください.

:::



## 高階関数
これまで扱ってきた関数の引数はすべて,値でしたが値ではなく**関数**を引数として指定することが可能です. **関数を引数に取る関数を高階関数といいます**.

例えば関数`f`とリスト`[x,y,z]`を引数として受け取り,リストの各要素に`f`を適用したリスト`[f x, f y, f z]`を返す関数は以下のように実装できます.

~~~ haskell
applyFToList :: (a -> b) -> [a] -> [b]
applyFToList f []     = []
applyFToList f [x]    = [f x]
applyFToList f (x:xs) =  (f x): (applyFToList f xs)

main = do
    print $ applyFToList (2*) [4,5,6]  -- [8,10,12]
    print $ applyFToList (1+) [4,5,6]  -- [5,6,7]
    print $ applyFToList show [4,5,6]  -- ["4","5","6"]
    print $ applyFToList fib  [4,5,6]  -- [5,8,13]
~~~

関数部分は, `(a -> b)`のように,丸括弧で囲んでいます.

### map

この関数と同じものが組み込み関数(あらかじめ定義された関数)として提供されている代表的な高階関数`map :: (a -> b) -> [a] -> [b]`です.

~~~ haskell
main = do
    print $ map (2*) [4,5,6]  -- [8,10,12]
    print $ map (1+) [4,5,6]  -- [5,6,7]
    print $ map show [4,5,6]  -- ["4","5","6"]
    print $ map fib  [4,5,6]  -- [5,8,13]
~~~

::: warn
- Prelude
---
Haskellの組み込み関数はライブラリ`Prelude`として提供されています.
`Prelude`はすべてのプロジェクトで自動で読み込まれています.

`map`関数は他のライブラリでも同名のものが提供されているため,それらと名前が被っている場合はどちらの`map`を利用するのか判別できないというエラーが起きます.

例として,`Data.Text`も`map`を提供しているために,`Data.Text`を`import`している場合には以下のようなエラーが出ます.

~~~ sh
Ambiguous occurrence ‘map’
    It could refer to
       either ‘Prelude.map’,
              imported from ‘Prelude’ at app/practice.hs:1:1
              (and originally defined in ‘GHC.Base’)
           or ‘Data.Text.map’,
              imported from ‘Data.Text’ at app/practice.hs:4:1-16
~~~

同名の関数が複数のライブラリで定義されている場合は,`Prelude.map`など,どのライブラリの`map`であるかを明示するか,
`hiding`を利用して特定の関数のみを`import`対象から外します.

~~~ haskell
import Data.Text hiding (map)
-- map 以外すべてをimport
~~~

あるいは,利用する関数のみを明示的に`import`することも可能です.

~~~ haskell
import Data.Text hiding (Text,empty)
-- Text,emptyのみをimport
~~~

:::

以下, よく用いられる代表的な高階関数に関して紹介します.

### filter

`filter :: (a -> Bool) -> [a] -> [a]`はリストの中から与えられた関数で判定される条件に合致するもののみを抽出する関数です.


~~~ haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text (elem)

main = do
    print $ filter (10 < ) [5,10,15,20] --  [15,20]
    print $ filter (Data.Text.elem 'a') ["cat","dog","bird"] --  ["cat"]
~~~

### fold
`foldl :: (a -> b -> a) -> a -> [b] -> a`,

`foldr :: (a -> b -> b) -> b -> [a] -> b`

は畳み込み関数です.`foldl`はリストの左端,`foldr`はリストの右端から値を一つずつ抜き出して,2引数関数によって一つの値に畳み込んでいきます.
リストのデータ構造的に基本的には`foldl`のほうが効率が良いので`foldl`が用いられます.

例として,

~~~ haskell
main = do
    print $ foldl (+) 0 [1,2,3] --  6
~~~

の挙動は,

`foldl (+) 0 [1,2,3]`

`foldl (+) (0+1) [2,3]`

`foldl (+) (1+2) [3]`

`foldl (+) (3+3) []`

`6`

となります.

### zipWith, zip

`zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]`

は2つのリストからそれぞれ値を順番に取り出して,関数を適用した結果をリストに格納する高階関数です.

例として.

~~~ haskell
main = do
    print $ zipWith (++) ["a","b","c"] ["x","y","z"] --  ["ax","by","cz"]
~~~

の挙動は,

`zipWith (++) ["a","b","c"] ["x","y","z"] `

`["a" ++ "x" ,"b" ++ "y","c" ++ "z"]`

となります.

`zip :: [a] -> [b] -> [(a,b)]`

は2つのリストからそれぞれ値を順番に取り出して,`[(左のリスト値,右のリストの値)]`を返す関数です.
タプルを返す2引数関数 `,` によって `zipWith (,)` として実装されます.

~~~ haskell
zip' :: [a] -> [b] -> [(a,b)]
zip' = zipWith (,)

tuple :: a -> b -> (a,b)
tuple a b = (a,b)

zip'' :: [a] -> [b] -> [(a,b)]
zip'' = zipWith tuple

main = do
    print $ zip [1,2,3] [11,12,13] --  [(1,11),(2,12),(3,13)]
    print $ zip' [1,2,3] [11,12,13] --  [(1,11),(2,12),(3,13)]
    print $ zip'' [1,2,3] [11,12,13] --  [(1,11),(2,12),(3,13)]
~~~

::: note

- 練習問題
---

- 与えられた整数のリストの各要素を二乗する関数squareListを,mapを使って定義してください.

~~~ haskell
squareList [1,2,3,4] -- [1,4,9,16]
~~~

- 整数のリストの総積を計算する関数productListを,foldlを使って定義してください.

~~~ haskell
productList [1,2,3,4] -- 24
~~~


- 2つのリストから,それぞれの要素の大きい方を選んで新しいリストを作る関数maxListを,zipWithを使って定義してください.

~~~ haskell
maxList [1,4,3] [2,2,5] -- [2,4,5]
~~~


:::

## 無名関数(ラムダ式)

高階関数に与える関数はその場限りの利用となる場合が多いため,先程の`zipWith`と`tuple`によって`zip`を定義した例のように, いちいち別の関数名をつけることは手間が多くなり,コードも冗長になりがちです.
そのような場合に, 使い捨ての関数を定義する手法が,**無名関数(ラムダ式) Lambda expression**です.

ラムダ計算は$\lambda$を表す記号,`\`を用いて, `\ 引数 -> 返り値`の形で式を定義できます.

例として,

`f x y z = x + y + z` は

`\ x y z -> x + y + z` となります.

`zipWith` の例は以下のようにも定義できます.

~~~ haskell
main = do
    print $ zip [1,2,3] [11,12,13] -- [(1,11),(2,12),(3,13)]
    print $ zipWith (\ x y -> (x,y)) [1,2,3] [11,12,13] --  [(1,11),(2,12),(3,13)]
~~~

また,練習問題中の`maxList`は,以下のように定義できます.


~~~ haskell
main = do
    print $ zipWith (\x y -> if x > y then x else y)
                    [1,4,3]
                    [2,2,5] -- [2,4,5]
~~~

::: note
- `flip` と高階関数
---

`flip :: (a -> b -> c) -> b -> a -> c`は,**関数の引数の順番を入れ替える関数**であり,以下のような挙動を示します.

~~~ haskell
main = do
   print $ (,) "a" "b" -- ("a","b")
   print $ flip (,) "a" "b" -- ("b","a")
   ---
   print $ (>) 1 2 -- False
   print $ flip (>) 1 2 -- True
~~~

高階関数にラムダ式を組み合わせたことで,記述が長くなった場合などには,`flip`で引数の関数とリストを入れ替え,**手続き型言語における`for文`に近い記法**を採用する場合があります.


~~~ haskell
main :: IO ()
main = do
   print $ flip map [-3 .. 3]
         $ \ x -> case x >= 0 of
                True  -> 1
                False -> 0
        -- [0,0,0,1,1,1,1]
~~~

このような`flip`,ラムダ式と`$`を組み合わせた記法は今後の**状態系**や**モナド**に関する議論などで頻出します.
また,このような書き方を前提とした`forM`,`forM_`などの関数も登場するので,頭の片隅に入れておいてください.

:::

::: note

- 練習問題
---

- ラムダ式と高階関数を利用して,リストの各要素に3を加える関数addThreeを定義してください.

~~~ haskell
addThree [1,2,3] -- [4,5,6]
~~~

- ラムダ式と高階関数を利用して,整数のリストから偶数だけを取り出す関数onlyEvenを定義してください.

~~~ haskell
onlyEven [1,2,3,4,5,6] -- [2,4,6]
~~~

- ラムダ式と高階関数を利用して,整数のリストに含まれる要素の絶対値の合計を求める関数sumAbsを定義してください.

~~~ haskell
sumAbs [-3,4,-1,2] -- 10
~~~

::::


## 関数合成

数学において,2つの関数 $f(x), g(x)$があるとき, $f(g(x))$を合成関数と呼び, $$f \circ g $$ とも書きます.
通常Haskellでも関数を合成する場合には,

`f (g x)` あるいは `f $ g x` と書きますが,関数 `(.)`によって `(f . g) x` と書くことができます.
関数定義においては

`h = f . g` のように定義することが可能です.


~~~ haskell
f :: Int -> Int
f x = 2 * x

g :: Int -> Int
g x = 3 + x


-- 実行例
main :: IO ()
main = do
    -- f(g(x))
    print $ f $ g 2 -- 10
    -- \[f \circ  g \]
    print $ (f . g) 2 -- 10
    -- 定義
    let h = f . g
    print $ h 2 -- 10
~~~


:::

# 変数(値の束縛)

Pythonなどの言語では,特定の変数に値を代入することができます.例えば,以下の最大値を求めるプログラムでは,変数`m`に最初の中身はリストの最初の要素が代入された後,次々とより大きな変数が代入されていきます. `変数`は名前の通り,次々とその値を変更していきます.

~~~ python
xs = [3,5,2,4,6,7,1]
m  = xs[0]

for x in xs[1:]:
    if x > m:
        m = x
print('max value:',m)
~~~

一方でHaskellでは,変数に一度値を割り当てると,その変数の値を後から変更することができません. 変数に値を再代入するという操作が許されていないのです. この性質を`普遍性` （immutability）といいます. したがって,Haskellでは代入という言葉を使わず`束縛`といいます.
これは,通常の手続き型言語との大きな違いになります.

::: warn
※1 値を変えられないなら｢変数じゃない｣じゃないという意見もありますが,数学において変数と呼ばれているものに近い概念だと考えましょう.

※2 実は後にでてくる`State`や`ST`などHaskellでも`再代入(破壊的代入)`を扱うことはできますが,特定の仕組みによって以下の純粋関数型言語の特徴を保っています.
:::

例えば,以下のように一度値を束縛した変数に新しく変数を代入しようとすると`xという変数に複数の宣言をしている`というエラーが出ます(ghciでやる場合には,`:{ :}`を入れる必要があり余計にややこしいですね.すみません).

~~~ haskell
ghci> :{
ghci| x = 1
ghci| x = 2
ghci| :}

<interactive>:5:1: error:
    Multiple declarations of ‘x’
    Declared at: <interactive>:4:1
                 <interactive>:5:1
~~~

これは一見非常に不便なように感じられますが,これによって関数型プログラムでは,プログラムの安全性を高めています.

例えばPythonにおける以下のプログラムについて考えてみましょう.

~~~ python
counter = 0

def count_plus(x):
    global counter
    counter += x
    return counter

print(count(1))  # 出力: 1
print(count(1))  # 出力: 2
~~~

このプログラムでは,`count()`関数に対して同じ引数1を与えているにもかかわらず,関数を呼び出すたびに,グローバル変数`counter`が変更されて,結果が変わります. 同じ関数を呼び出しても,結果が変わるために関数のみから,関数の挙動を把握することができません.

一方でHaskellでは,常に同じ関数は,同じ入力に対して,同じ返り値を返します. このような特性を**参照透過性(Referential Transparency)**と呼び,これによってプログラムの挙動を把握しやすくしています.

また,上記のPythonのプログラムは,関数を実行するたびに,関数の外にある,`counter`という変数の状態が変化しています. このような,関数が実行されることで単に値を返す以外に何らかの｢外部の状態を変化させる｣ことを関数の**副作用(Side Effect)**といいます. これは言い換えれば,関数の実行によるプログラム全体への影響が,関数以外の外部の状態に依存していることを意味しており,プログラムの挙動を予測することを難しくします.

参照透過性と副作用は相互に結びついた概念ですが,Haskellでは参照透過性を保ち,副作用を排除するようにプログラムが設計されています.
このように, **｢参照透過性｣** と **｢副作用の排除｣** の両方を持った関数型言語を **純粋関数型言語** と呼びHaskellの大きな特徴の一つです.

Haskellにおいて,変数への再代入が禁止されていることのメリットは理解していただけたかと思いますが,Haskellにも変数自体はあります.

Haskellにおける変数は主に,**トップレベル変数**及び**ローカル変数**に大別されます.

## トップレベル変数

先程の `x=1`のように,独立して宣言される変数を`トップレベル変数`と呼びます. トップレベル変数は,Pythonなどの言語における`グローバル変数`と同様に,スクリプト内のどこ場所からでも利用することができます.


~~~ haskell
x = 1

someFunc :: Int -> Int
someFunc y = x + y

main = do
    print $ someFunc 1 --  2
~~~

## ローカル変数
手続き型言語においてスコープが制限された変数のように,特定の関数内でのみ参照可能な局所変数として,**ローカル変数**が存在します. Haskellにおけるローカル変数は, `let式`,`where節`の2つのパターンが用意されています(ラムダ式内の引数も見方によってはローカル変数かもしれません.)

### let式
関数内で `let 宣言 in 式`の形式で局所変数を定義できます.

~~~ haskell
someFunc :: Int -> Int
someFunc y = let x = 1
           in x + y

main = do
    print $ someFunc 1 --  2
~~~

この変数`x`は別の関数内で参照することはできません.

~~~ haskell
someFunc :: Int -> Int
someFunc y = let x = 1
           in x + y

someFunc2 :: Int -> Int
someFunc2 y = x + y

main = do
    print $ someFunc2 1 --  Variable not in scope: x :: Int
~~~
複数の宣言をひとまとめにすることも可能です.

~~~ haskell
someFunc :: Int -> Int
someFunc z = let x = 1
                 y = 2
           in x + y + z

main = do
    print $ someFunc 1 --  4
~~~

`Do`記法を利用すると`in`を省略することができます.

~~~ haskell
someFunc :: Int -> Int
someFunc z = do
    let x = 1
        y = 2
    x + y + z

main = do
    print $ someFunc 1 --  4
~~~

### where節
数式の直後にインデントをつけて`where 宣言`と書くことでも局所変数や局所関数を定義できます.

~~~ haskell
someFunc :: Int -> Int
someFunc z = f z
    where
    x = 1
    y = 2
    f z = x + y + z

main = do
    print $ someFunc 1 --  4
~~~

# 練習問題(関数総合)

::: note

1. 統計量
---

- 与えられたリストの標本標準偏差`s`を計算する関数を実装してください.

- 与えられた2つのリストの積率相関係数`r`を計算する関数を実装してください.

それぞれの定義は以下とします.

$$
s = \sqrt{\frac{\sum_{i=1}^{n}(x_i - \bar{x})^2}{n}}
$$
$$
r = \frac{\sum_{i=1}^{n}(x_i - \bar{x})(y_i - \bar{y})}{\sqrt{\sum_{i=1}^{n}(x_i - \bar{x})^2 \sum_{i=1}^{n}(y_i - \bar{y})^2}}
$$

~~~ haskell
-- 実行例
main :: IO ()
main = do
  let xs = [1, 2 .. 5]
      ys = [5, 4 .. 1]
  putStrLn $ "標準偏差: " ++ show (stddev xs) ---  1.4142135623730951
  putStrLn $ "相関係数: " ++ show (correlation xs ys) --- -0.9999999999999998
~~~


<details>
    <summary> 回答例 </summary>

~~~ haskell
-- 平均値を求める関数\mean :: [Double] -> Double
mean xs = sum xs / fromIntegral (length xs)

-- 標本標準偏差を求める関数
stddev :: [Double] -> Double
stddev xs = sqrt variance
  where
    m = mean xs
    n = fromIntegral (length xs)
    variance = sum (map (\x -> (x - m)^2) xs) / n

-- 積率相関係数を求める関数
correlation :: [Double] -> [Double] -> Double
correlation xs ys = covariance / (stddev xs * stddev ys)
  where
    mx = mean xs
    my = mean ys
    n  = fromIntegral (length xs)
    covariance = sum (zipWith (\x y -> (x - mx)*(y - my)) xs ys) / n

-- 実行例
main :: IO ()
main = do
  let xs = [1, 2 .. 5]
      ys = [5, 4 .. 1]
  putStrLn $ "標準偏差: " ++ show (stddev xs) ---  1.4142135623730951
  putStrLn $ "相関係数: " ++ show (correlation xs ys) --- -0.9999999999999998
~~~

</details>


2. パーセプトロン
---

- or 回路を表すパーセプトロンの発火関数 `f x1 x2` を以下のように定める.
(パーセプトロンの意味などがわからない場合は, [特別講義資料](slds14.html)を参照のこと)

$$
f(x1, x2) =
\begin{cases}
1 & (0.5 x_1 + 0.5 x_2 \geq 0.2)\\
0 & (\text{otherwise})
\end{cases}
$$

この回路を表す`perceptronOR :: Bool -> Bool -> Bool`を実装せよ.

~~~ haskell
-- 実行例
main :: IO ()
main = do
   print $ perceptronOR False False -- False
   print $ perceptronOR True False  -- True
   print $ perceptronOR False True  -- True
   print $ perceptronOR True True   -- True
~~~

<details>
    <summary> 回答例 </summary>

~~~ haskell
perceptronOR :: Bool -> Bool -> Bool
perceptronOR x1 x2
  | sum >= 0  = True
  | otherwise = False
  where
    g True = 1
    g False = 0
    sum = 0.5 * g x1 + 0.5 * g x2 - 0.2

-- 実行例
main :: IO ()
main = do
   print $ perceptronOR False False -- False
   print $ perceptronOR True False  -- True
   print $ perceptronOR False True  -- True
   print $ perceptronOR True True   -- True
~~~

</details>

:::


