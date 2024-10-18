---
title: 代数プログラミング入門 Ch4 関数
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
previousChapter: iap3.html
nextChapter: iap5.html
---

# 関数

Haskellは関数型言語なので,関数の記述がプログラミングにおける花形です. この章ではHaskellの関数に関する記法を学びましょう.


## スクリプトファイルの実行

::: warn
ここから先は,コードが複数行に渡ることが多くなるので,ghciの利用をやめてスクリプトを書きます.

`app` フォルダ内に `practice.hs`を作成しそこで事例の勉強をしましょう.
:::

`practice.hs` ファイルを作成したら,ファイルを以下のように記述しましょう.

~~~ haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text

main :: IO ()
main = putStrLn "practice"
~~~

::: warn
`module XXX () where`

という記述は,他のファイルからインポート可能なmodule化を行うための宣言です.
また,Stackでは,**大文字で始まる`*.hs`ファイルは,moduleとして認識されます.**

したがって,一つのプロジェクトに複数の実行可能ファイルを生成する場合には,

`module XXX () where`

の記述をなくし, ファイル名を小文字ではじめる必要があります.

これは,`Hello World`のために編集した`Main.hs`も同様であるため,`Main.hs`を`hello.hs`に名前を変更し,ファイル内の `module Main (main) where`の記述も削除し,以下のように変更しましょう.

cf. [他にもいくつかの方法があるようです](https://www.reddit.com/r/haskell/comments/capuz7/multiple_executable_in_project/)
:::

~~~ haskell
import Lib

main :: IO ()
main = helloWorld
~~~

`package.yaml`の`executables:`を以下のように編集して`hello.hs`と`practice.hs`を実行可能ファイルとして登録します. `Data.Text`を利用するために,`dependencies:`以下に`- text`を追加しておきましょう.

~~~ yaml
dependencies:
- base >= 4.7 && < 5
- text

#ghc-options:

library:
  source-dirs: src

executables:
  hello:
    main:                main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - hello-world

  practice:
    main:                practice.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - hello-world
~~~

`stack run practice` で`practice!`と表示されれば成功です.

これからスクリプトで実行していくにあたって,`practice.hs`の中身をもう少し詳しく見てみましょう.

~~~ haskell
import Lib

main :: IO ()
main = putStrLn "practice!!"
~~~

haskellのプログラムを実行すると, `main関数`のみが実行されます.

Haskellは関数型言語なので,これから`import Lib`と`main`の間に関数を定義していき,`main`の中で実行していくことになります.

main 関数で行うことは関数として実行することになりますが,これから学習する通常の関数の定義で記述するのは今は難しいので,`do`記法を紹介します. main 関数の=以下に`do`と書くことで,do以下のインデントブロックに記述された内容が手続き型的に1行ずつ実行されます.

以下のプログラムでは, `"practice1"`,`"practice2"`,`"practice3"`の順に標準出力されます.

~~~ haskell
import Lib

main :: IO ()
main = do
    putStrLn "practice1" -- >>> "practice1"
    putStrLn "practice2" -- >>> "practice2"
    putStrLn "practice3" -- >>> "practice3"
~~~

`stack run practice`の結果を確認すると以下のようになります.

~~~ sh
> stack run practice
"practice1"
"practice2"
"practice3"
~~~

また,ghciと異なって,出力結果が同じ画面に現れないので,
以降のコード例では, その行の結果をコメント内で`>>>`に続けて書くこととします. コメント部分は,記述しなくても結果は変わらないので,省略しても構いません.



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
    print $ f 4 -- >>> 5
~~~

`()`の代わりにスペースを使う点以外は全く同じ書き方で, `=`の左側に関数名と引数,右側に返り値を書きます.
関数名は小文字の英字で始めれるというルールがあります.

`f :: Int -> Int`は型注釈であり,この`f`という関数が,引数に`Int`を取り,返り値として`Int`を返すということを指定しています.

`do`以下の記述で, `f 4`の結果を確認しています. `print`は,文字列に変換可能な値を受取,標準出力する関数です. また `(f 4)`を省略して`$ f 4` としています.

引数は何個でも利用できます. 例えば2引数関数

$$ multiple(x,y) = x * y $$

は以下のように定義できます.

~~~ haskell
multiple :: Double -> Double -> Double
multiple x y = x * y
multiple 3 4

main = do
    print $ multiple 3 4 -- >>> 12.0
~~~

また,以下の記号を組み合わせて中置演算子名として利用することも可能です.

::: note
~ !  #  $  %  &  *  +  = .  /  < >  ?  @  \  ^  |  -
:::


~~~ haskell
(.*) :: Double -> Double -> Double
x .* y = x * y

main = do
    print $ 3 .* 4 -- >>> 12.0
~~~

絵文字などのUnicode記号も利用することができます.

~~~ haskell

(✖) :: Double -> Double -> Double
x ✖ y = x * y

main = do
    print $ 3 ✖ 4 -- >>> 12.0
~~~

記号を利用して関数を定義する場合には,定義時に`()` で囲うことで一般の関数のように定義することができます.
例えば, 乗算を新たに定義するとして,以下のように書くことができます.

~~~ haskell
(.*) :: Int -> Int -> Int
(.*) x y = x * y
main = do
    print $ 3 .* 4 -- >>> 12
~~~

前置の2引数関数も` `` ` (バッククオート)で囲むことで中置演算子として定義することができます.

~~~ haskell
x `multiple` y = x * y

main = do
    print $ 3 `multiple ` 4 -- >>> 12
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

## 分岐

関数型言語において,手続き型言語におけるIF文に相当するのが**パターンマッチ**と**指示関数(特性関数)**です.

### パターンマッチ

パターンマッチに近い概念は既にフィボナッチ数の漸化式として出てきています.フィボナッチ数の漸化式は,以下のように表されます.

::: note
$$ F_0 = 1 $$
$$ F_1 = 1 $$
$$ F_n = F_{n-1} + F_{n-2} (n >= 2)  $$
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
fib :: Int -> Int -> Int
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

~~~ haskell
strHead :: Show a => [a] -> String
strHead []     = "Empty"
strHead [x]    = show x
strHead (x:xs) = show x

main = do
    print $ strHead [] -- >>> "Empty"
    print $ strHead [3,4] -- >>> "3"
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
    print $ fib 5 -- >>> 8
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
    print $ fib 5 -- >>> 8
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
    print $ fib 5 -- >>> 8
~~~

Haskellではあまりif式は利用されませんが,
1行で書けるため,式の中で部分的に利用する場合に便利です.

~~~ haskell
fib :: Int -> Int
fib n = if n == 0 then 1 else if n == 1 then 1 else fib (n-1) + fib (n-2)
~~~


## 再帰

Haskellにおいてもfor文に相当する記法は存在しますが,基本的にループは**再帰**によって実装されます.
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

main = print $ tatal [1..10] -- >>> 55
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

## ラムダ式

## 高階関数
map, fold, zip


## 合成

## 値の束縛

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

※2 実は後にでてくるStateやSTなどHaskellでも`再代入(破壊的代入)`を扱うことはできますが,特定の仕組みによって以下の純粋関数型言語の特徴を保っています.
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

また,上記のPythonのプログラムは,関数を実行するたびに,関数の外にある,`counter`という変数の状態が変化しています. このような,関数が実行されることで単に値を返す以外に何らかの｢外部の状態を変化させる｣ことを関数の**副作用(Side Effect)**といいます. これは言い換えれば,関数の実行によるプログラム全体への影響が,関数以外の外部の状態に依存していることを意味しており,プログラムの挙動を予測することを難しくします. Haskell

参照透過性と副作用は相互に結びついた概念ですが,Haskellでは参照透過性を保ち,副作用を排除するようにプログラムが設計されています.
このように, **｢参照透過性｣** と **｢副作用の排除｣** の両方を持った関数型言語を **純粋関数型言語** と呼びHaskellの大きな特徴の一つです.

Haskellにおいて,変数への再代入が禁止されていることのメリットは理解していただけたかと思いますが,Haskellにも変数自体はあります.

トップレベル変数

ローカル変数
where
let

