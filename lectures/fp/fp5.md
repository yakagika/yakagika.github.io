---
title: 関数型プログラミング Ch5 関数
description: 資料
tags:
    - algebra
    - lecture
    - haskell
featured: true
katex: true
date: 2024-10-18
tableOfContents: true
previousChapter: fp4.html
nextChapter: fp6.html
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
    print $ 3 `multiple` 4 --  12
~~~

::: note

**なぜ中置演算子として定義するのか**

中置演算子を自前で定義するのは, Pythonなどの言語ではあまり行われないので｢なぜわざわざこんなことをするのだろう｣と感じる人がいるかも知れません.
中置演算子の最大のメリットは, このあと扱う計算順序(優先順位・結合性)の設計とあわせて, 式の認知負荷を大幅に下げられることです.

例えば `1 + 1 + 1 + 1` という計算を考えてみましょう.

これを **前置記法 (ポーランド記法)** で書くと,

`((+) ((+) ((+) 1 1) 1) 1)` となります. Haskell では `(+)` のように記号演算子を括弧で囲むと普通の前置関数として扱えるので, これは Haskell の構文としても合法ですが, 実際に何が計算されているかを読み取るのは大変です.

更に複数の演算子が絡むと事態はより複雑になります.

`1 + 1 * 2 + 3 / 2` のような計算は, 中置演算子であれば非常にシンプルですが, 前置記法では

`(+) ((+) 1 ((*) 1 2)) ((/) 3 2)` となります.

このように, 適切な記号と優先順位で中置演算子を設計することで, 人間が読むときの理解負荷も実装上の括弧の量も大幅に下がります.

:::


::: note

### Exercise CH5-1

**基本関数と中置演算子の定義**

1. 整数を受け取り, その値を 2 倍したものを返す関数 `double :: Int -> Int` を定義してください.

2. 整数 `x`, `y` を受け取り, `x * y + x + y` を返す2引数関数 `f :: Int -> Int -> Int` を定義してください.

3. 2引数の中置演算子 `(.+) :: Int -> Int -> Int` を, `x .+ y = x + y + 1` として定義してください.

4. 2引数の前置関数 `divide :: Double -> Double -> Double` を `x / y` を返すように定義し, バッククオート で囲むことで中置演算子として呼び出してください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
    print $ double 7        -- 14
    print $ f 2 3           -- 11
    print $ 4 .+ 5          -- 10
    print $ 10 `divide` 3   -- 3.3333333333333335
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
double :: Int -> Int
double x = 2 * x

f :: Int -> Int -> Int
f x y = x * y + x + y

(.+) :: Int -> Int -> Int
x .+ y = x + y + 1

divide :: Double -> Double -> Double
divide x y = x / y

main :: IO ()
main = do
    print $ double 7        -- 14
    print $ f 2 3           -- 11
    print $ 4 .+ 5          -- 10
    print $ 10 `divide` 3   -- 3.3333333333333335
~~~

- `f 2 3 = 2 * 3 + 2 + 3 = 11`. 関数の型注釈と本体は `=` の左右で対応付けて書きます.
- 記号からなる中置演算子は `x .+ y = ...` のように直接定義してもよく, 型注釈側では `(.+)` のように括弧で囲って前置関数として宣言します.
- 通常の前置2引数関数 (`divide` など) も, バッククオートで囲むだけで中置記法に切り替えられます.

</details>

:::

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

右結合演算子の場合,演算子は右から左へと評価されます. 例えば, `^`は右結合です. 式 `a ^ b ^ c` は `a ^ (b ^ c)` として評価されます

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

::: note

### Exercise CH5-2

**結合性と優先順位**

1. 2引数の中置演算子 `(.+) :: Int -> Int -> Int` を `x .+ y = x + y` として定義し, **右結合・優先順位 5** を意味する `infixr 5 .+` 宣言を付けてください. このとき `1 .+ 2 .+ 3` がどのように括弧づけされて評価されるか答えてください.

2. 以下の **4 つの演算子** を 1 つのスクリプトに定義します. 中身はどれも「加算」または「乗算に 1 を足す」だけで, **異なるのは `infix` 宣言で与えた優先順位だけ** です.

    ~~~ haskell
    -- セット A: 通常の + * と同じく, 乗算系の方が優先順位が高い
    (.+) :: Int -> Int -> Int
    x .+ y = x + y
    infixl 6 .+

    (.*) :: Int -> Int -> Int
    x .* y = x * y + 1
    infixl 7 .*

    -- セット B: セット A と逆に, 加算系の方が優先順位が高い
    (+.) :: Int -> Int -> Int
    x +. y = x + y
    infixl 7 +.

    (*.) :: Int -> Int -> Int
    x *. y = x * y + 1
    infixl 6 *.
    ~~~

    a. `2 .+ 3 .* 4` (セット A) の括弧付けと値を答えてください.

    b. `2 +. 3 *. 4` (セット B) の括弧付けと値を答えてください.

    c. (a) と (b) で値が異なる理由を, 演算子の定義に基づいて簡潔に説明してください.

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

1. `infixr 5 .+` で右結合となるため, `1 .+ 2 .+ 3 = 1 .+ (2 .+ 3) = 1 .+ 5 = 6`. もし `infixl 5 .+` であれば `(1 .+ 2) .+ 3 = 3 .+ 3 = 6` と左から評価され, 今回は結果が同じになりますが, 結合性によって計算順が変わる点に注目してください.

~~~ haskell
(.+) :: Int -> Int -> Int
x .+ y = x + y
infixr 5 .+

main :: IO ()
main = do
    print $ 1 .+ 2 .+ 3   -- 6
~~~

2. 括弧付けと値

    a. `.*` の優先順位 (7) が `.+` (6) より高いので, `.*` が先に評価され `2 .+ (3 .* 4) = 2 .+ (3*4 + 1) = 2 .+ 13 = 15`

    b. 今度は `+.` の優先順位 (7) が `*.` (6) より高いので, `+.` が先に評価され `(2 +. 3) *. 4 = 5 *. 4 = 5*4 + 1 = 21`

    c. `(.*)` / `(*.)` の定義 `x * y + 1` は通常の乗算と異なり **分配法則 $a(b+c) = ab + ac$ を満たさない** ため, `(2 .+ 3) .* 4` と `2 .+ (3 .* 4)` は数学的にも別の式となります. どちらを先に評価するかで値が変わるため, 演算子の中身が同じでも `infix` 宣言で与える優先順位次第で **式の意味そのもの** が変わってしまいます.

~~~ haskell
-- 4 つの演算子を一つのスクリプトに同居させ, 同じ実行で両方の挙動を確認できる
(.+) :: Int -> Int -> Int
x .+ y = x + y
infixl 6 .+

(.*) :: Int -> Int -> Int
x .* y = x * y + 1
infixl 7 .*

(+.) :: Int -> Int -> Int
x +. y = x + y
infixl 7 +.

(*.) :: Int -> Int -> Int
x *. y = x * y + 1
infixl 6 *.

main :: IO ()
main = do
    print $ 2 .+ 3 .* 4   -- 15 (= 2 .+ (3 .* 4))
    print $ 2 +. 3 *. 4   -- 21 (= (2 +. 3) *. 4)
~~~

</details>

:::

## カリー化,部分適用

Haskellでは多引数関数を実装できることは先程確認しました. しかし,Haskellは**すべての関数が,引数を一つだけとる**という原則があります. これは,矛盾するようですが,この矛盾を解消する概念が **カリー化(Currying)** です.

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


`add 5 10 = (add 5) 10` であり, ここで`(add 5) :: Int -> Int`という新たな関数に10が適用されています(以下は型の確認のためにghciを利用しています.)

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
        return fib(x-1) + fib(x-2)

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
strHead []    = "Empty"
strHead [x]   = show x
strHead (x:_) = show x

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
fib n | n == 0    = 1
      | n == 1    = 1
      | n >= 2    = fib (n-1) + fib (n-2)
      | otherwise = error "fib: negative input"

main = do
    print $ fib 5 --  8
~~~

特徴関数におけるifの位置が先に来ている以外は,基本的に対応関係にあるのがわかるかと思います. なお, 上のガードでは負の整数が来た場合に備えて `otherwise` で `error` を投げています. これは次節でも扱う通り「どのガードにも該当しない値」が来た場合に `Non-exhaustive guards` で落ちるのを防ぐためのフォールバックです.

#### otherwise

ガードの最後の分岐には, 慣例的に **`otherwise`** を置きます. `otherwise` は `Prelude` で次のように定義されている単なる `True` の別名であり, 必ず真と評価されるため「それまでのどのガードにも当てはまらなかった場合」のフォールバックとして機能します.

~~~ haskell
otherwise :: Bool
otherwise = True
~~~

簡単な事例として, 整数の符号を文字列で返す関数 `sign` を考えます.

~~~ haskell
sign :: Int -> String
sign n | n > 0     = "正"
       | n < 0     = "負"
       | otherwise = "零"

main = do
    print $ sign 5     -- "正"
    print $ sign (-3)  -- "負"
    print $ sign 0     -- "零"
~~~

`n > 0` も `n < 0` も成立しない場合(つまり `n == 0` のとき)に `otherwise` の分岐が選ばれます. もちろん `| n == 0 = "零"` と書いても等価ですが, `otherwise` を用いることで「残り全て」を網羅していることが明示されます.

最後の `otherwise` を書き忘れて, かつどのガードにも該当しない値が渡された場合は `Non-exhaustive guards in function ...` という実行時エラーが発生するので注意してください.

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

::: note

### Exercise CH5-3

**4 つの記法による `describe` の実装**

引数の整数 `n` に応じて以下のような文字列を返す関数 `describe :: Int -> String` を, **パターンマッチ**, **ガード**, **case式**, **if式** の4通りの方法でそれぞれ実装してください. 同じ関数が異なる分岐の記法でどのように表現されるかを確かめることが目的です.

- `n` が `0` のとき → `"zero"`
- `n` が `1` のとき → `"one"`
- `n` が `2` のとき → `"two"`
- それ以外のとき → `"many"`

~~~ haskell
describe 0   -- "zero"
describe 1   -- "one"
describe 2   -- "two"
describe 3   -- "many"
describe 10  -- "many"
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

- **パターンマッチ**による実装

~~~ haskell
describe :: Int -> String
describe 0 = "zero"
describe 1 = "one"
describe 2 = "two"
describe _ = "many"
~~~

- **ガード**による実装

~~~ haskell
describe :: Int -> String
describe n
  | n == 0    = "zero"
  | n == 1    = "one"
  | n == 2    = "two"
  | otherwise = "many"
~~~

- **case式**による実装

~~~ haskell
describe :: Int -> String
describe n = case n of
    0 -> "zero"
    1 -> "one"
    2 -> "two"
    _ -> "many"
~~~

- **if式**による実装

~~~ haskell
describe :: Int -> String
describe n = if n == 0 then "zero"
             else if n == 1 then "one"
             else if n == 2 then "two"
             else "many"
~~~

- 実行例

~~~ haskell
main :: IO ()
main = do
    putStrLn $ describe 0   -- "zero"
    putStrLn $ describe 1   -- "one"
    putStrLn $ describe 2   -- "two"
    putStrLn $ describe 3   -- "many"
    putStrLn $ describe 10  -- "many"
~~~

いずれの記法でも,最後の「それ以外」を表す部分(パターンマッチとcase式では `_`,ガードでは `otherwise`,if式では最後の `else`)を忘れると,該当しない入力に対して実行時エラーとなるので注意してください.

</details>

:::

::: note

**4つの分岐記法の使い分け**

ここまで **パターンマッチ**, **ガード**, **case式**, **if式** の4つを見てきました. どれを使っても同じ処理を書ける場面も多いですが, それぞれ得意とする状況があります. 目安は以下の通りです.

- **パターンマッチ**: 引数の **構造** (リストが空か `x:xs` か, タプルの中身, 後の章で扱う代数的データ型のコンストラクタなど) で分けたいときの第一候補. 関数定義として最も簡潔に書けます.

- **ガード**: 引数の **値の条件** (`n > 0`, `n >= 100` のような大小比較や述語) で分けたいとき. ｢構造ではなく条件で分岐したい｣ 場合はパターンマッチより自然です.

- **case式**: 関数定義の引数だけでなく, **関数の途中で計算した値** に対してパターンマッチしたいとき. 後述の通り **式** なので, `let` の右辺など, 関数定義のトップレベル以外の場所でも使えます.

- **if式**: 条件が **真偽の2分岐** だけのとき. 1行で書けるので, 式の一部に短く埋め込む用途に向きます.

**パターンマッチと case式の使い分け**

特に紛らわしいのがパターンマッチと case式です. 両者は ｢パターンで分岐する｣ という点では同じですが, **文法上の位置づけが決定的に異なります**.

- **パターンマッチは関数定義の構文** です. `fib 0 = 1` のように, **関数を定義する `=` の左辺** にしか書けません. したがって, 関数定義から独立して単独で使うことはできず, 他の式の一部として埋め込むこともできません.

- **case式は ｢式｣** です. 評価すると値になるため, **値を書ける場所ならどこにでも書けます**. 関数の途中, `let` の右辺, さらには別の式の引数としても利用できます (次章で扱うラムダ式の本体などでも使えます).

例えば, 関数の内部で一度計算した値に応じて分岐したい場合, パターンマッチでは ｢別の補助関数を定義してそこにパターンマッチを書く｣ 必要がありますが, case式ならその場で書けます.

~~~ haskell
-- 入力を 2 倍した値で分岐したい
-- case式なら, 計算した値 (m) に対してその場で分岐できる
classify :: Int -> String
classify n =
    let m = n * 2
    in case m of
        0 -> "zero"
        2 -> "two"
        _ -> "other"

main = do
    print $ classify 0   -- "zero"
    print $ classify 1   -- "two"   (1 * 2 = 2)
    print $ classify 5   -- "other"
~~~

上の `m` のように **関数の引数そのものではなく, 途中で束縛した値** に対して分岐したいときは, 関数定義の左辺でしか使えないパターンマッチでは書けず, 式である case式の出番となります. ｢関数の引数の構造で分けるならパターンマッチ, それ以外の場所で分けたいなら case式｣ と覚えておくとよいでしょう.

:::

::: note

**コラム: 4つの分岐記法の出自 ― 数学由来とプログラミング由来**

これら4つの記法が ｢似ているのに微妙に違う｣ のは, **出自が2つの系統に分かれている** ことに理由があります.

- **パターンマッチ ― 数学の ｢再帰方程式｣ 由来**

    `fib 0 = 1` のように **複数の方程式で関数を定義する** スタイルは, 数学者 Kleene が 1936 年に整理した **再帰方程式 (recursion equations)** に由来します. これをそのまま言語構文に取り込んだのが Burstall らの **NPL (1977 頃)** で, ｢関数は再帰方程式の集合として定義し, 各方程式の **左辺のパターン** でどの式を使うか決める｣ という考え方を導入しました. NPL → Hope → ML → Haskell と受け継がれています. パターンマッチが **関数定義の左辺にしか書けない** のは, それが元々 ｢方程式の左辺｣ という数学的な出自を持つためです.

- **ガード ― 数学の ｢場合分け｣ 由来**

    `f(n) = \begin{cases}\dots\end{cases}` という **場合分け (definition by cases)** の記法に対応します. ｢guard (ガード)｣ という用語自体は Dijkstra が 1975 年に提唱した概念で, 関数型言語では SASL を縮小した **KRC (1976 頃)** が各定義の節にガードを付ける形を最初期に採用しました. 構造ではなく **値の条件** で分けたいときに, 数学の場合分けに近い見た目で書けるようにしたものです.

- **if式 ― プログラミングが生んだ概念**

    `if-then-else` を **式** として扱う発想は, John McCarthy が 1958〜1960 年に **LISP** の設計の中で発明しました (微分を行うプログラムを書く際に ｢条件式があれば再帰関数が自然に書ける｣ と気づいたのが発端です). 数学にはもともと ｢2つの式を条件で結ぶ演算子｣ は無く, **式としての条件分岐** は純粋にプログラミング側の発明でした. のちに ALGOL 60 が採用して広く普及しました.

- **case式 ― 型による分岐を構造化したもの**

    Burstall が 1969 年に, **入れ子になった if-then-else の構文糖 (syntactic sugar)** として case 式を提案しました. データ型の **コンストラクタごとの場合分け** を読みやすく書くためのもので, BCPL の ｢型による switch｣ (Richards, 1967) からも示唆を得ています. 最初から **式** として設計されたため, 値を書ける場所ならどこでも使えます.

つまり, **パターンマッチとガードは ｢関数とは何かを方程式・場合分けで述べる｣ 数学の系統**, **if式と case式は ｢式を評価して値を得る｣ プログラミングの系統** から来ています. 前述の ｢パターンマッチは関数定義なので独立して使えないが, case式は式なので関数内でも使える｣ という違いも, この出自の差から生まれた本質的なものだと理解できます. そして Haskell は, この異なる2系統をいずれも **｢式を返すもの｣** として同じ言語に統一しており, これが次節 ｢式と文｣ で扱う ｢Haskell では全てが式である｣ という性質につながっています.

**主な出典**

- D. A. Turner, *Some History of Functional Programming Languages*, TFP 2012. <https://www.cs.kent.ac.uk/people/staff/dat/tfp12/tfp12.pdf> (NPL / Hope / SASL / KRC と再帰方程式・パターンマッチの系譜)
- J. McCarthy, *Recursive Functions of Symbolic Expressions and Their Computation by Machine, Part I*, CACM, 1960. <http://jmc.stanford.edu/articles/lisp/lisp.pdf> (if-then-else を式として導入)
- H. Wayne, *A Very Early History of Algebraic Data Types*. <https://www.hillelwayne.com/post/algdt-history/> (Burstall 1969 の case 式)
- Wikipedia, *Pattern matching* / *Guard (computer science)* / *ISWIM*. <https://en.wikipedia.org/wiki/Pattern_matching>

:::


## 式と文

ここまでで関数定義・演算子・分岐といった基本的な構文を一通り見てきました. ここで Haskell を理解する上で重要な **式(Expression)** と **文(Statement)** という考え方を整理しておきましょう.

### 式・文とは

Python や C などの手続き型言語では, プログラムを構成する要素は大きく **文** と **式** の2つに分けられます.

- **文(Statement)**: ｢実行される｣命令であり, それ自体は値を持ちません. 代入 `x = 1`, `if` 文, `for` 文, `return` 文などが該当します.

- **式(Expression)**: ｢評価される｣ことで値になるものです. `1 + 2` や `f(x)`, `x > 0` などが該当します.

例えば Python では `if` は文なので, 以下のように `if` の結果を直接変数に束縛することはできません.

~~~ python
# これは文法エラー
y = if x > 0: 1 else: 0
~~~

値が欲しい場合には, 三項演算子という別の式を使う必要があります.

~~~ python
y = 1 if x > 0 else 0   # こちらは式なので OK
~~~

このように手続き型言語では ｢文｣ と ｢式｣ が区別され, 場所によって書けるものが決まっています.

### 全てが式

一方で Haskell には **文が存在せず, プログラムを構成する要素は全て式です**. つまり, あらゆる構文要素が ｢評価されると値になる｣ という性質を持っています.

例えば, 先程の `### if式` で扱った `if` も Haskell では **式** です. これが ｢`else` を省略できない｣ と述べた理由です. `if` が必ず値を返さなければならない以上, 条件が偽のときに返す値も必要だからです. このことを利用すると, `if` を値が必要な場所にそのまま埋め込むことができます.

~~~ haskell
-- if は式なので, 値が必要な場所にそのまま書ける
absPlus1 :: Int -> Int
absPlus1 x = (if x >= 0 then x else -x) + 1

main = do
    print $ absPlus1 3    -- 4
    print $ absPlus1 (-5) -- 6
~~~

同じく先程扱った `case式` も式であり, このあと扱う `let式` や次章で扱うラムダ式なども全て値を持つ式です. ｢全てが式である｣ という性質は, 小さな式を組み合わせてより大きな式を作るという関数型プログラミングの基本的な発想につながっています.

### 関数と式の違い

ここまで ｢関数｣ と ｢式｣ という言葉を使ってきましたが, この2つは対立する概念ではありません. 両者の関係を整理しておきましょう.

- **式** とは, ｢評価すると **値** になるもの｣ でした.
- **関数** とは, ｢引数を受け取って値を返す｣ という **値の一種** です.

つまり関数は式と並ぶ別物ではなく, **式を評価して得られる値のうちの一種** が関数なのです. Haskell では関数も整数や文字列と同じく値として扱える **第一級の値(first-class value)** であり, 変数に束縛したり, 他の関数に渡したり (高階関数), 返り値にしたり (カリー化) できます.

このことは, 先程の `## カリー化,部分適用` で見た `add` を例にすると分かりやすくなります.

~~~ haskell
add :: Int -> Int -> Int
add x y = x + y
~~~

このとき, 次の3つはいずれも **式** であり, 評価するとそれぞれ **値** になります.

- `add`        … 評価すると ｢2引数を待つ関数｣ という **値** (型 `Int -> Int -> Int`).
- `add 5`      … 評価すると ｢残り1引数を待つ関数｣ という **値** (型 `Int -> Int`, 部分適用).
- `add 5 10`   … 評価すると ｢`15`｣ という **値** (型 `Int`).

このように, 関数名そのものを書いた `add` も式 (値は関数), 関数に引数を与えた `add 5 10` も式 (値は整数) です. 違いは **評価した結果の値が ｢関数｣ か ｢関数以外 (整数など)｣ か** という点だけで, ｢式である｣ ことに変わりはありません.

::: warn
`add` や `add 5` のように **値が関数になる式** は, そのまま `print` できません. 関数は ｢どう表示するか｣ が定まっていない (`Show` のインスタンスでない) ためで, これが後の章で扱う `No instance for Show (Int -> Int)` エラー (本章末の Exercise CH5-8 参照) の正体です. 一方 `add 5 10` は値が `Int` なので `print` できます.
:::

整理すると, **関数は ｢式が表しうる値の一種｣ であり, 関数を引数に適用する ｢関数適用｣ もまた一つの式** だ, という入れ子の関係になっています. この視点は, 次章で扱う高階関数やラムダ式を理解する際の土台になります.

### 式の種類

ここまでに登場した式を整理すると, 主に以下のような種類があります.

- **リテラル**: `1`, `'a'`, `"hello"`, `True`, `[1,2,3]`, `(1,2)` のように値そのものを表す式.

- **変数(値の束縛)の参照**: `x` や `pi` のように, 束縛された値を指す式 (値の束縛については後述の `## 変数(値の束縛)` で扱います).

- **関数適用**: `f x` や `add 5 10` のように, 関数に引数を与える式.

- **演算子適用**: `1 + 2` や `3 .* 4` のように, 中置演算子で値を組み合わせる式.

- **条件分岐の式**: 先程扱った `if x > 0 then 1 else 0` のような `if式` や `case式`.

- **局所束縛を伴う式**: `let x = 1 in x + y` のような `let式` (後述の `### let式` で扱います).

- **ラムダ式**: `\x -> x + 1` のように, その場で関数を作る式 (次章 `Ch6 関数(応用編)` の `## 無名関数(ラムダ式)` で扱います).

これらの式はいずれも評価されると値になるため, 関数の引数や `print` の対象, あるいは別の式の一部として自由に組み合わせることができます. まだ扱っていない式の詳細は, このあとの節および次章で順に説明していきます.


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
total []     = 0
total (x:xs) = x + total xs

main = print $ total [1..10] --  55
~~~

このtotal関数は,与えられたリストが空の場合0を返します.
要素が一つ以上あるリストの場合には,先頭の要素`x`をそれ以降の要素`xs`の合計に足すという処理を再帰的に行います.

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

`total` では終了状態が ｢空リスト `[]`｣ の1つだけでしたが, 終了状態は複数あっても構いません. 本章で何度も登場している `fib` も再帰関数の一例です.

~~~ haskell
fib :: Int -> Int
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

main = print $ fib 4 --  5
~~~

この `fib` 関数では, 引数が `0` または `1` のとき `1` を返すという **2つの終了状態** をパターンマッチで指定しています. それ以外のとき (`n >= 2`) には, `fib (n - 1)` と `fib (n - 2)` という **2回の再帰呼び出し** の結果を足し合わせます.

`total` が ｢自分自身を1回だけ呼び出す｣ 単純な再帰だったのに対し, `fib` は ｢自分自身を2回呼び出す｣ 点が異なります. `fib 4` における処理の流れを追うと, 呼び出しが木のように枝分かれしていくのがわかります.

~~~
fib 4
= fib 3 + fib 2
= (fib 2 + fib 1) + (fib 1 + fib 0)
= ((fib 1 + fib 0) + fib 1) + (fib 1 + fib 0)
= ((1 + 1) + 1) + (1 + 1)
= 3 + 2
= 5
~~~

`fib (n - 1)`, `fib (n - 2)` のように引数が必ず終了状態 (`0` や `1`) へ向かって小さくなっていくため, この再帰は有限回で停止します. もし `fib n = fib (n - 1) + fib (n + 1)` のように引数が小さくならない呼び出しを書いてしまうと, 終了状態に到達できず停止しなくなる点に注意してください.



::: note

### Exercise CH5-4

**再帰による `length2` と FizzBuzz**

1. リストの長さを返す`length2 :: [a] -> Int` 関数を新しく実装してください.

2. 与えられた整数のリストを引数にとり,要素毎にFizzBuzzを実行した結果を文字列のリストで返す関数
`fizzBuzz :: [Int] -> [String]`実装してください.

FizzBuzzは,プログラミング学習で題材としてよく用いられる問題で,整数 `n` を以下のルールに従って文字列に変換します.

- `n` が **3と5の両方の倍数(=15の倍数)** のときは `"FizzBuzz"`
- `n` が **3の倍数** のときは `"Fizz"`
- `n` が **5の倍数** のときは `"Buzz"`
- それ以外のときは `n` をそのまま文字列化したもの(`show n`)

例えば `[1..15]` を入力すると,以下の結果が得られます.

~~~ haskell
fizzBuzz [1..15]
-- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz"]
~~~

なお,3と5の両方の倍数のケースを先に判定しないと `"Fizz"` や `"Buzz"` にマッチしてしまうため,条件の順序に注意が必要です.

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
-- 1. リストの長さを再帰で求める
length2 :: [a] -> Int
length2 []     = 0
length2 (_:xs) = 1 + length2 xs

-- 2. 各要素を FizzBuzz 文字列へ変換
fizzBuzz :: [Int] -> [String]
fizzBuzz []     = []
fizzBuzz (x:xs) = fb x : fizzBuzz xs
  where
    fb n
      | n `mod` 15 == 0 = "FizzBuzz"
      | n `mod`  3 == 0 = "Fizz"
      | n `mod`  5 == 0 = "Buzz"
      | otherwise       = show n

-- 実行例
main :: IO ()
main = do
  print $ length2 [1,2,3,4,5]           -- 5
  print $ length2 "hello"               -- 5
  print $ fizzBuzz [1..15]
  -- ["1","2","Fizz","4","Buzz","Fizz","7","8","Fizz","Buzz","11","Fizz","13","14","FizzBuzz"]
~~~

</details>

:::



## 変数(値の束縛)

Pythonなどの言語では,特定の変数に値を代入することができます.例えば,以下の最大値を求めるプログラムでは,変数`m`に最初の中身はリストの最初の要素が代入された後,次々とより大きな変数が代入されていきます. `変数`は名前の通り,次々とその値を変更していきます.

~~~ python
xs = [3,5,2,4,6,7,1]
m  = xs[0]

for x in xs[1:]:
    if x > m:
        m = x
print('max value:',m)
~~~

一方でHaskellでは,変数に一度値を割り当てると,その変数の値を後から変更することができません. 変数に値を再代入するという操作が許されていないのです. この性質を`不変性` (immutability) といいます. したがって,Haskellでは代入という言葉を使わず`束縛`といいます.
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

print(count_plus(1))  # 出力: 1
print(count_plus(1))  # 出力: 2
~~~

このプログラムでは,`count_plus()`関数に対して同じ引数1を与えているにもかかわらず,関数を呼び出すたびに,グローバル変数`counter`が変更されて,結果が変わります. 同じ関数を呼び出しても,結果が変わるために関数のみから,関数の挙動を把握することができません.

一方でHaskellでは,常に同じ関数は,同じ入力に対して,同じ返り値を返します. このような特性を**参照透過性(Referential Transparency)**と呼び,これによってプログラムの挙動を把握しやすくしています.

また,上記のPythonのプログラムは,関数を実行するたびに,関数の外にある,`counter`という変数の状態が変化しています. このような,関数が実行されることで単に値を返す以外に何らかの｢外部の状態を変化させる｣ことを関数の**副作用(Side Effect)**といいます. これは言い換えれば,関数の実行によるプログラム全体への影響が,関数以外の外部の状態に依存していることを意味しており,プログラムの挙動を予測することを難しくします.

参照透過性と副作用は相互に結びついた概念ですが,Haskellでは参照透過性を保ち,副作用を排除するようにプログラムが設計されています.
このように, **｢参照透過性｣** と **｢副作用の排除｣** の両方を持った関数型言語を **純粋関数型言語** と呼びHaskellの大きな特徴の一つです.

Haskellにおいて,変数への再代入が禁止されていることのメリットは理解していただけたかと思いますが,Haskellにも変数自体はあります.

Haskellにおける変数は主に,**トップレベル変数**及び**ローカル変数**に大別されます.

### トップレベル変数

先程の `x=1`のように,独立して宣言される変数を`トップレベル変数`と呼びます. トップレベル変数は,Pythonなどの言語における`グローバル変数`と同様に,スクリプト内のどの場所からでも利用することができます.


~~~ haskell
x = 1

someFunc :: Int -> Int
someFunc y = x + y

main = do
    print $ someFunc 1 --  2
~~~

### ローカル変数
手続き型言語においてスコープが制限された変数のように,特定の関数内でのみ参照可能な局所変数として,**ローカル変数**が存在します. Haskellにおけるローカル変数は, `let式`,`where節`の2つのパターンが用意されています(ラムダ式内の引数も見方によってはローカル変数かもしれません.)

#### let式
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

#### where節
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

## 練習問題(関数総合)

本章で学んだ **パターンマッチ・ガード・再帰・where** を組み合わせて解く総合問題です. ここではまだ次章で扱う高階関数やラムダ式は使わず, 基礎的な道具だけで解くことを意識してください.

::: note

### Exercise CH5-5

**成績の評価ランク変換**

整数の点数 `score` (0〜100 を想定) を, 以下のルールでランク文字列に変換する関数 `grade :: Int -> String` を **ガード** で実装してください.

- `90` 以上 → `"A"`
- `80` 以上 `90` 未満 → `"B"`
- `70` 以上 `80` 未満 → `"C"`
- `60` 以上 `70` 未満 → `"D"`
- それ未満 → `"F"`

さらに, 点数のリストを受け取り, 各要素をランクに変換した文字列のリストを返す関数 `grades :: [Int] -> [String]` を **再帰** で実装してください (高階関数 `map` は使わないこと).

~~~ haskell
-- 実行例
main :: IO ()
main = do
    print $ grade 95              -- "A"
    print $ grade 72              -- "C"
    print $ grade 40              -- "F"
    print $ grades [95,82,71,55]  -- ["A","B","C","F"]
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
-- ガードで点数をランクへ変換
grade :: Int -> String
grade score
  | score >= 90 = "A"
  | score >= 80 = "B"
  | score >= 70 = "C"
  | score >= 60 = "D"
  | otherwise   = "F"

-- 再帰でリストの各要素を変換 (map を使わない)
grades :: [Int] -> [String]
grades []     = []
grades (x:xs) = grade x : grades xs

-- 実行例
main :: IO ()
main = do
    print $ grade 95              -- "A"
    print $ grade 72              -- "C"
    print $ grade 40              -- "F"
    print $ grades [95,82,71,55]  -- ["A","B","C","F"]
~~~

- `grade` は上から順にガードを判定するため, `score >= 90` を最初に置けば各範囲の上限を改めて書く必要はありません.
- `grades` は `total` や `fizzBuzz` と同じく, 空リストを終了状態とし, 先頭要素を変換して残りを再帰処理する典型的な再帰パターンです.

</details>

:::

::: note

### Exercise CH5-6

**パーセプトロン (OR 回路の発火関数)**

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

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
perceptronOR :: Bool -> Bool -> Bool
perceptronOR x1 x2
  | threshold >= 0 = True
  | otherwise      = False
  where
    g True  = 1
    g False = 0
    threshold = 0.5 * g x1 + 0.5 * g x2 - 0.2

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


## エラー修正演習

ここからは本章で扱った **関数定義** に関わる典型エラーを解決する演習です. 関数を書き始めると, fp4 で学んだエラー以外に **関数特有のエラーパターン** も頻出します. 実エラーを読み, 原因を答えて修正してください.

::: note

### Exercise CH5-7

**関数定義中の型不一致**

以下の関数は `Int` を受け取って `Int` を返すと宣言されているが, コンパイルが通りません. 原因を答えて修正してください.

~~~ haskell
-- ch5-7.hs (誤りあり)
f :: Int -> Int
f x = x ++ "!"

main :: IO ()
main = print (f 1)
~~~

実エラー:

~~~ sh
app/Main.hs:2:7: error: [GHC-83865]
    • Couldn't match expected type ‘Int’ with actual type ‘[Char]’
    • In the expression: x ++ "!"
      In an equation for ‘f’: f x = x ++ "!"
  |
2 | f x = x ++ "!"
  |       ^^^^^^^^
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

**原因**: 型注釈で `f :: Int -> Int` と宣言したのに, 本体の `x ++ "!"` は **`String` (= `[Char]`)** を返している. 期待型 `Int` と実際の型 `[Char]` が一致しない.

修正方針は 2 通り.

**修正 A**: 結果を文字列にする (型注釈を実装に合わせる).

~~~ haskell
f :: Int -> String
f x = show x ++ "!"        -- "1!"

main = putStrLn (f 1)
~~~

**修正 B**: 整数として返したい場合は, 結果型を変えずに数値演算で実装する.

~~~ haskell
f :: Int -> Int
f x = x + 1                 -- 2

main = print (f 1)
~~~

「型注釈」と「実装」のどちらが正しいかを意識しながら型を揃えるのがポイント.

</details>

:::

::: note

### Exercise CH5-8

**関数の引数不足 (No instance for Show)**

以下のコードは `add 5` の結果を表示しようとしていますが, コンパイルが通りません. 原因を答えて修正してください.

~~~ haskell
-- ch5-8.hs (誤りあり)
add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = print (add 5)
~~~

実エラー:

~~~ sh
app/Main.hs:5:8: error: [GHC-39999]
    • No instance for ‘Show (Int -> Int)’ arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In the expression: print (add 5)
      In an equation for ‘main’: main = print (add 5)
  |
5 | main = print (add 5)
  |        ^^^^^
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

**原因**: `add :: Int -> Int -> Int` は **2 引数関数** だが, `add 5` は **1 引数しか与えていない** ので結果は `Int -> Int` (部分適用された関数). 関数自体は `Show` のインスタンスを持たないため `print` できない.

GHC のヒント `(maybe you haven't applied a function to enough arguments?)` が決定的.

修正: 引数を全部与える.

~~~ haskell
add :: Int -> Int -> Int
add x y = x + y

main :: IO ()
main = print (add 5 10)   -- 15
~~~

部分適用を意図的に行いたい場合は, 別の関数に束縛してから利用する (本章 「部分適用」参照).

~~~ haskell
add5 :: Int -> Int
add5 = add 5

main = print (add5 10)    -- 15
~~~

</details>

:::

::: note

### Exercise CH5-9

**関数定義のパターン非網羅 (実行時エラー)**

以下のコードはコンパイルは通りますが (警告は出る), 実行すると途中で停止します. 原因を答えて修正してください.

~~~ haskell
-- ch5-9.hs (誤りあり)
describe :: Int -> String
describe 0 = "zero"
describe 1 = "one"

main :: IO ()
main = do
    putStrLn (describe 0)
    putStrLn (describe 2)
~~~

実行時出力:

~~~ sh
zero
ch5-9: app/Main.hs:(2,1)-(3,18): Non-exhaustive patterns in function describe
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

**原因**: `describe` のパターンは `0` と `1` の 2 つしか定義されていない. `describe 2` のように **どのパターンにも該当しない値** を渡すと `Non-exhaustive patterns in function describe` で停止する.

エラーメッセージの `(2,1)-(3,18)` は **問題の関数定義の範囲** (ソース 2 行 1 列〜3 行 18 列). 修正は **網羅的にパターンを書く** か **ワイルドカード `_` で残りを受ける** .

修正 A: ワイルドカードでフォールバック.

~~~ haskell
describe :: Int -> String
describe 0 = "zero"
describe 1 = "one"
describe _ = "many"

main = do
    putStrLn (describe 0)   -- zero
    putStrLn (describe 2)   -- many
~~~

修正 B: ガードで網羅性を担保.

~~~ haskell
describe :: Int -> String
describe n
  | n == 0    = "zero"
  | n == 1    = "one"
  | otherwise = "many"
~~~

`stack build` 時に **`Pattern match(es) are non-exhaustive` の警告** が出るので, 警告を放置せず常に網羅するクセを付けると安全.

</details>

:::
