---
title: 代数プログラミング入門 Ch3 Haskellを使ってみよう
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
previousChapter: iap2.html
nextChapter: iap4.html
---

# Haskellを使ってみよう

## ghci

前節では, Stackを利用した,プロジェクトの作成と実行に関して扱いましたが, Haskellにも対話環境が存在します.
`stack ghci`コマンドを打つことで, Haskellの対話環境(`REPL`)が立ち上がります.


この節では,Haskellの基礎について学びますが,ghciの紹介も併せて,いくつかの基礎的な仕様については,ghci上で確認してみましょう.

## 終了

ghciではコマンドを`:`の後に入力します. ghciの終了コマンドは`:q`です.

~~~ sh
❯ stack ghci
ghci>:q
Leaving GHCi.
~~~

## コメントアウト

Haslellではコメントアウトは `--` です. 複数行に渡る場合は `{- -}` で囲みます.

::: warn
Haskellのプログラムを読んでいると `--|` や `--^` というタイプのコメントを良く見ますが, こちらはHaskellのドキュメント生成ライブラリにおいて, ドキュメント中に説明として記述するための記号です.
またコメント中に `>>>` と記述することでテストが実装できるなどいろいろなものがありますが,本資料では扱いません.
:::

~~~ haskell
ghci> -- コメント
ghci> {- コメント-}
~~~

## 複数行モード

ghci上で複数行のプログラムを書く場合には `:{ :}` でプログラムを囲います. 例えば,先程のフィボナッチ数のプログラムをghci上で実行する場合,位置行ずつ定義すると,定義が更新されてき最後の `f n = f (n-1) + f (n-2)`のみが記憶されます. この場合,`n`は無限にマイナスに続いていくため,`Stack Overflow`エラーが表示されます.

~~~ haskell
ghci> fib 0 = 1 -- fの定義が上書きされる
ghci> fib 1 = 1 -- fの定義が上書きされる
ghci> f n = f (n-1) + f (n-2)
ghci> f 12
*** Exception: stack overflow
~~~

`:{ :}`で囲むことでひとまとまりの定義として認識されます.

~~~ haskell
ghci> :{
ghci| fib :: Int -> Int
ghci| fib 0 = 1
ghci| fib 1 = 1
ghci| fib n = fib (n-1) + fib (n-2)
ghci| :}
ghci> fib 12
233
~~~

なお,スクリプトの場合は,`:{ :}`なしでそのまま改行すれば問題ありません.

## データ型

型に関しては,かなり奥が深い,というよりHaskellの面白さは自分で型を作っていくことにあります. ただ,いきなりそれをすると,わけがわからなくなるのでまずは代数的データ型などには触れず以下の基礎的な型に関して説明します.

::: note

- 数値型
    - 整数 (Int, Integer)
    - 実数 (Float,Double)

- タプル
- リスト (List)
- 文字,文字列 (Char,String,Text)
- 論理型(Bool)

:::

Haskellにおいて,値のデータ型はある程度自動推論されますが,特定のデータ型を明示したい場合には,値の後ろに`:: データ型`をつけます.

~~~ haskell
ghci> 1 :: Int
1
ghci> 1 :: Double
1.0
~~~

`ghci`において形の確認は`:t`あるいは`:type`コマンドの後ろに確認したいデータを入力することで行えます.

~~~ haskell
ghci> :t 'c'
'c' :: Char
ghci> :type (1 :: Int)
(1 :: Int) :: Int
~~~

### 数値型

Haskellの基本的な数値型には以下の4つがあります. クラスに関しては後に扱うので,今はデータ型の更に大きな分類程度に考えておいてください.

| クラス            |  データ型  | 意味             |
| ------            | ------     | ---------------- |
| Integer (整数)    | `Int`      | 固定長整数型     |
| Integer (整数)    | `Integer`  | 多倍長整数型     |
| Fractional (小数) | `Float`    | 単精度浮動小数型 |
| Fractional (小数) | `Double`   | 倍精度浮動小数型 |


`Int`と`Integer`は`整数`, `Float`と`Double`は`実数`を表しています.

::: note

`固定長/多倍長`, `単精度/倍精度` というのはどういう意味でしょうか?

コンピューターでは,データはすべて`0`と`1`のいずれかを表す`bit`の集まりによって表現されます. ちなみに`8bit`で`1byte`, `1024byte`で`1Kbyte`です.したがって,プログラミングで扱うデータに使用できるデータ量には制限があり,無限の長さの整数や少数を利用することはできません.

コンピューターの計算は主に中央演算処理装置(CPU)で行われますが,その計算過程でデータを一時的に記録するCPU内部の装置のことを汎用レジスタといい,現在では`64bit`以下の汎用レジスタを持った`64bit CPU`が良く利用されています.

現在一般的な`64bit CPU`においてHaskellは整数と小数を表すのに一般的に最大`64bit`の領域を確保します. したがって,整数では64bitで表せるデータ量(`-9223372036854775808 ~ 9223372036854775807`)を超えた整数を扱うことはできません.

ちなみにIntの最大値,最小値はghciで以下のように確認できます(
使用しているコンピューターによっては結果が変わる可能性があります).

~~~ haskell
ghci> minBound :: Int
-9223372036854775808
ghci> maxBound :: Int
9223372036854775807
~~~

最大(小)値を超えるとオーバーフローします.

~~~ haskell
ghci> 9223372036854775807 :: Int
9223372036854775807
ghci> (9223372036854775807 + 1) :: Int
-9223372036854775808
~~~
:::


### 数値型の演算

Haskellにおける数値型の基本的な演算子は以下のように定義されています. 実数と整数で挙動が異なるものがあるので注意が必要です.

演算子には優先順位が設定されており,数字が大きいものから順に適用されます(最小0,最大9).
また,式を`()`で囲むことで,その内部が優先的に計算されます.

**また,`()`が式の最後に来る場合には`$`記号以下が`()`に囲まれているとみなすことができます.**


|  計算      | 記号 | 優先順 |
| ------     | ---- | -----  |
| 足し算     | `+`  | 6      |
| 引き算     | `-`  | 6      |
| 掛け算     | `*`  | 7      |
| 割り算     | `/`  | 7      |
| 冪乗(整数) | `^`  | 8      |
| 冪乗(実数) | `**` | 8      |


~~~ haskell
ghci> 1 + 1
2
ghci> 2 - 1
1
ghci> 3 * 3
9
ghci> 9 / 3
3.0
ghci> 3 ^ 3
27
ghci> 3 ** 3
27.0
ghci> 3 ^ (1/2) -- エラー

<interactive>:7:3: error: [GHC-39999]

ghci> 3 ** (1/2)
1.7320508075688772
ghci> 2 * 3 + 1
7
ghci> 2 * (3 + 1)
8
ghci> 2 * $ 3 + 1
8
~~~

これらは中置演算子として定義されていますが演算子を`()`で囲むことによって前置(逆ポーランド記法)で利用することができます.

~~~ haskell
ghci> (+) 3 4
7
ghci> (*) ((+) 3 4) 2
14
ghci> (*) 2 $ (+) 3 4
14
~~~


また, 2引数関数として定義された前置の演算子は ` `` ` (バッククオート)で囲むことで, 中置演算子として利用できます.

|  計算      | 記号  | 優先順 |
| ------     | ----  | -----  |
| 整数除算   | `div` | 7      |
| 剰余       | `mod` | 6      |

~~~ haskell
ghci> 5 /2
2.5
ghci> div 5 2
2
ghci> 5 `div` 2
2
ghci> 5 `mod` 2
1
~~~

### 数値型の変換

`Integral(整数)`から`Fractional(小数)`への変換は, `fromIntegral`を利用します.

~~~ sh
ghci> fromIntegral (1 :: Int) :: Double
1.0
ghci> div 5 2
2
ghci> 2 ** (div 5 2)

<interactive>:6:1: error: [GHC-39999]
    • Ambiguous type variable ‘a0’ arising from a use of ‘print’
      prevents the constraint ‘(Show a0)’ from being solved.
      Probable fix: use a type annotation to specify what ‘a0’ should be.
      Potentially matching instances:
        instance [safe] Show Version -- Defined in ‘Data.Version’
        instance Show Exception.ArithException
          -- Defined in ‘GHC.Exception.Type’
        ...plus 39 others
        ...plus 20 instances involving out-of-scope types
        (use -fprint-potential-instances to see them all)
    • In a stmt of an interactive GHCi command: print it
ghci> 2 ** (fromIntegral (div 5 2))
4.0
~~~

`Fractional(小数)`から`Integral(整数)`への変換は,基本的に何かしらの**切り捨て**を実施します.

| 切り捨て関数名 | 意味                       | 例                                                       |
| -------------- | -------------------------- | ----------------------------------                       |
| ceiling        | 小数点以下を切り上げる     | `ceiling 3.2 → 4`, `ceiling (-3.2) → -3`               |
| floor          | 小数点以下を切り下げる     | `floor 3.8 → 3`, `floor (-3.8) → -4`                   |
| truncate       | 小数部分を単純に切り捨てる | `truncate 3.8 → 3`, `truncate (-3.8) → -3`             |
| round          | 最も近い整数に丸める       | `round 3.5 → 4`, `round 3.4 → 3`, `round (-3.5) → -4` |

~~~ haskell
2 ^ (2 / 1)

<interactive>:8:3: error: [GHC-39999]
    • Could not deduce ‘Integral b0’ arising from a use of ‘^’
      --- 省略

ghci> 2 ^ (truncate (2 / 1))
4
~~~


::: note

練習問題

以下の問題をREPLを使って自分で解いてみましょう.
問題自体は小学生でも解けますが,重要なのはHaskellの挙動を確認することです.
どのように計算したかを併せて説明してください.

- 飴が40個あります.7人で同じ数ずつ分けると1人分は何個で何個あまりますか?

- 底辺5cm,高さ4cmの三角形の面積はいくつですか?

- 2の8乗はいくつですか?

- 累乗と掛け算の計算順序を丸括弧を使った計算で確かめてください.

:::


### リスト

複数のデータをまとめる方法はいくつかありますが,データを1列に並べた`List`型は代表的なデータ型です. Haskellには配列(`Array`や`Vector`)もありますが,まずは`List`について学習しましょう.
リストの操作にはここで扱う以外にも`リスト内包表記`や`高階関数`など様々なものがありますが,ここでは最も基本的ないくつかの機能のみに絞って,後ほど詳細を扱います.

Listは**リストリテラル**`[]`の中に要素を記入して,`,`(コンマ)で区切ることで宣言できます.

::: warn
Haskellにおいて,リテラルとは,**特定のデータ型の値を直接記述する構文**のことを指します.

- リストリテラル`[]`は,`[]`内の記述をリスト型として扱うリテラル

- 数値を記入するとそれは数値型として扱われる数値リテラル

- `""`で囲まれた記述は文字列型として扱われる文字列リテラル

などがあります.

Haskellでは,自作したデータ型にリテラルを定めるなど様々な用法がありますが,ここでは扱いません.
:::

~~~ haskell
ghci> [1,2,3]
[1,2,3]
~~~

`[]`のみで空のリストが生成されます.

~~~ haskell
ghci> []
[]
~~~

注意点として,HaskellはPythonなどの言語のように`ダックタイピング`が許されていないため異なるデータを単一のリストの要素に含めることはできません.

~~~ haskell
ghci> [1,2.0,3]
[1.0,2.0,3.0]
ghci> [1::Int,2::Double,3::Int]

<interactive>:22:9: error: [GHC-83865]
    • Couldn't match expected type ‘Int’ with actual type ‘Double’
    • In the expression: 2 :: Double
      In the expression: [1 :: Int, 2 :: Double, 3 :: Int]
      In an equation for ‘it’: it = [1 :: Int, 2 :: Double, 3 :: Int]
~~~

リストのデータ型は,要素のデータ型をリストリテラル`[]`で囲んだ形で表されます.

~~~ haskell
ghci> :type ([1::Int,2::Int])
([1::Int,2::Int]) :: [Int]
ghci> :type ['a','b','c']
['a','b','c'] :: [Char]
~~~

また,リストは`先頭要素 : リスト` によって宣言することも可能です. `:`を`cons 構築子`といいます. 構築子の意味については後ほど`代数的データ型`の説明とともに扱います.

~~~ haskell
ghci> 1 : []
[1]
ghci> 1 : [2,3]
[1,2,3]
ghci> 1 : 2 : [3]
[1,2,3]
~~~

リストの要素のインデックスによる取得は `!!`演算子を用いて`xs !! インデックス`の形で行います. インデックスは0から始まります. インデックスが超過した場合はエラーとなります.

~~~ haskell
ghci> [1,2,3] !! 0
1
ghci> [1,2,3] !! 1
2
ghci> [1,2,3] !! 2
3
ghci> [1,2,3] !! 3
*** Exception: Prelude.!!: index too large
CallStack (from HasCallStack):
  error, called at libraries/base/GHC/List.hs:1366:14 in base:GHC.List
  tooLarge, called at libraries/base/GHC/List.hs:1376:50 in base:GHC.List
  !!, called at <interactive>:37:9 in interactive:Ghci15
~~~


`m~n`までの連続したリストを生成する場合には,`[m..n]`と記述します.これを`数列表記`といいます.

~~~ haskell
ghci> [1 .. 10]
[1,2,3,4,5,6,7,8,9,10]
~~~

コンマと併用することで階差数列などを表現することも可能です.

~~~ haskell
ghci> [1,3..10]
[1,3,5,7,9]
~~~

`[x..]`と終わりを指定しないことで,無限数列も作成できます. ghciでそのまま実行すると永遠に表示が止まりません(`ctrl+C`で止まります). ここでは,`[1,3,5,...]`の10番目と100番目の値を取り出してみます.

~~~ haskell
ghci> [1,3..] !! 10
21
ghci> [1,3..] !! 100
201
~~~

::: warn

Pythonなど言語では,値が宣言/生成されたタイミングでコンピュータがその値を評価する`正格(strict)評価`が一般的です. 一方HaskellはDefaultでは,実際にその値が呼び出された際に評価される`遅延(lazy)評価`を採用しており,それによりこのような無限の値を実現することができます.
正格評価で無限に値が続くリストを生成した場合, 生成した時点で永遠に計算が終わりませんが,遅延評価では無限のリストの中の具体的な値を利用するさいにその値が利用されます.

この機能はHaskellの大きな特徴の一つですが,一方でメモリリークや,速度の低下の原因になることがあります. したがって,ある程度大きなプログラムを書く場合には,正格評価と,遅延評価を明示的に切り替えることが推奨されています.

最初は気にする必要はありませんが,パッケージなどの提供するHaskellのデータ型には,strictなものとlazyなものの両方が用意されていることが多いので,違いを覚えておくと後々役に立ちます.

:::

Haskellでリストを扱う際には,暗黙に`x`などの単一のアルファベットが要素,`xs`などの複数形がリストを表している場合が多く`x:xs`などと記述してリストの最初の要素と残りのリストを表します.

詳細は後ほど扱いますが,`束縛`されたリストから`パターンマッチ`によって値を取り出す場合によく利用されます.

~~~ haskell
ghci> x:xs = [1,2,3]
ghci> x
1
ghci> xs
[2,3]
~~~

リスト同士の結合は`++`演算子によって行います.

~~~ haskell
ghci> [1] ++ [2,3]
[1,2,3]
~~~

リストの長さは`length` 関数で取得できます.

~~~ haskell
ghci> length []
0
ghci> length [1,2,3]
3
~~~

### タプル

Haskellではデータの組み合わせを表す方法として,後述の`直積型`がありますが,タプルも良く利用されます.タプルを利用するには要素を`()`(丸括弧)で囲い,`,`(コンマ)で区切ります. 要素数に制限はありません.

~~~ haskell
ghci> (1,2)
(1,2)
ghci> (1,2,3)
(1,2,3)
~~~

リストと同様に要素数の異なるタプルや,要素のデータ型の異なるタプルは別のデータ型として区別され,同一のリストなどに入れることはできません.

~~~ haskell
ghci> [(1,2),(1,2,3)]

<interactive>:60:8: error: [GHC-83865]
    • Couldn't match expected type: (a, b)
                  with actual type: (a0, b0, c0)
    • In the expression: (1, 2, 3)
      In the expression: [(1, 2), (1, 2, 3)]
      In an equation for ‘it’: it = [(1, 2), (1, 2, 3)]
    • Relevant bindings include
        it :: [(a, b)] (bound at <interactive>:60:1)
~~~

要素数が2つのリストに限定して,要素を取り出す関数 `fst`,`snd`が用意されていますが,値の取り出しはパターンマッチがよく利用されます.

~~~ haskell
ghci> fst (1,2)
1
ghci> snd (1,2)
2
ghci> (x,y) = (1,2)
ghci> x
1
ghci> y
2
~~~

### 文字列型

Haskellの文字列型は歴史的に少し複雑な状況になっており,Preludeにおける`String`型の使い勝手があまり良くありません. なので, `text`パッケージの提供する`Text`型を利用するのが一般的です. なので,後ほどTextを導入しますが,一旦String型に関して見てみましょう.

Haskellでは1文字を表す `Char`型と文字列を表す`String`型を区別し,`Char`は`''`(シングルクォーテーション),`String`は`""`(ダブルクオーテーション)で囲みます.

~~~ haskell
ghci> 'c'
'c'
ghci> :t 'c'
'c' :: Char
ghci> "String"
ghci> :t "String"
"String" :: String
~~~

Haskellにおける文字型`Char`のリスト`[Char]`の別名(`型シノニム`)です. `型シノニム`は型に別の名前をつけることで,形の用途などを区別する機能です.
型シノニムは,以下のように, `type 型シノニム = 元のデータ型`という形で定義します.

~~~ haskell
type String = [Char]
~~~

したがって,String型にはListの演算が適用できます.

~~~ haskell
ghci> "String" !! 1
't'
ghci> '!' : "String"
"!String"
ghci> "!!" ++  "String"
"!!String"
ghci> length "String"
6
~~~

ただし,`String`型は非効率なため,現在ではあまり使われておらず,基本的に`text`パッケージの提供する `Data.Text`を利用することが推奨されています.

`package.yaml`の`dependencies`に以下のように`text`を追加します.

~~~ yaml
dependencies:
- base >= 4.7 && < 5
- text
~~~

スクリプトの最初に以下のように,記述することで文字列リテラル`""`が`Text`型に利用できるようになります.

~~~ haskell
{-# LANGUAGE OverloadedStrings #-}
import Data.Text
~~~

`{-# LANGUAGE OverloadedStrings #-}`は言語拡張を表しており,Haskellの処理系に機能を追加する宣言です. `OverloadedString`は文字列リテラルをTextなどの他の文字列を表すデータ型に適用できるようにする拡張です.

`ghci`で言語拡張を導入するには,`:set `に続けて `-X言語拡張名`を記述します.

~~~ haskell
ghci> :type "Text"
"Text" :: String
ghci> :set -XOverloadedStrings
ghci> import Data.Text
ghci> :type "Text"
"Text" :: Data.String.IsString a => a
~~~

## 論理型(Bool)

それが正しいか間違っているか判別できる文を**命題**といいます. 命題の結果を表すものとして真(正しい),偽(間違っている)という値を用います. 真と偽を併せて**真偽値**といいます.

例えば,`1は2より大きい`という命題は,間違っているので**偽**となります. `人間は必ず死ぬ`という命題は,今のところ不老不死の人間がいないので**真**です.


真偽値を表すデータ型として`Bool`があります. `Bool`は`True`(真),`False`(偽)のいずれかです.

Haskellには命題の判定を行う`関係演算子`として,以下のようなものが準備されています.

|記号       | 意味         |
| :---:     | :---:        |
| `>`       |  より大きい  |
| `>=`      |  以上        |
| `<`       |  より小さい  |
| `<=`      |  以下        |
| `==`      |  等しい      |
| `/=`      |  等しくない  |

数値などの大小関係を調べるときには,比較演算子 `>`,`>=`.`<`,`<=`を利用します. 演算子の左右に数値を書くと,結果に応じて真偽値が帰ってきます.

~~~ haskell
ghci> 1 > 2
False
ghci> 1 < 1.5
True
~~~

値が等しいか/等しくないかを判定するには,`==`と`!=`を利用します.

~~~ haskell
ghci> 4 == 4
True
ghci> "cat" /= "cat"
False
~~~

`True` や `False`などの`Bool`値は, `AND`(かつ),`OR`(または),`NOT`という演算で計算することができます(`XOR`というのもあるが省略).
HaskellではAND は `&&`, OR は `||`, NOT は `not` という演算子が提供されています.

A,Bが命題だとして,`A && B`は両方`True`のときに,`True`となります. `A || B`は片方どちらかが`True`のときに`True`となります.

例えば,

- `1は2より大きい かつ 2は0より大きい` という命題は,`2は0より大きい`は正しいですが,`1は2より大きい`が間違っているので全体として,`False`です.

- `ネコは哺乳類である または ネコは鳥類である`という命題は `ネコは鳥類である`が間違っていますが全体としては`True`です.

演算の結果は,それぞれ以下のようになります. これを真偽値表といいます. ここでは,最低限の例だけを紹介しますが,より深く理解したい人は論理学などの講義を受講しましょう.

| 命題Aの値 | Bの値 | `A && B` | `A || B`|
| :---:     | :---: | :---:   | :---:  |
| True      | False | True    | True   |
| False     | True  | False   | True   |
| True      | False | False   | True   |
| False     | False | False   | True   |


~~~ haskell
ghci> 1 > 2
False
ghci> 2 > 0
True
ghci> 1 > 2 && 2 > 0
False
ghci> 1 > 2 || 2 > 0
True
~~~

`not` は命題の否定を表しており `True`が`False`,`False`が`True`になります.`not`は命題の前に書きます.

~~~ sh
ghci> 1 > 2
False
ghci> not (1 > 2)
True
~~~

::: note

**演習**

ある値が偶数かどうかは,2で割った余りが0かどうかを判定することで判定できます.
`x=101`,`y=202`として, 以下の命題の真偽を判定してください.

- xが偶数
- yが偶数
- xが偶数かつyが偶数
- xが偶数またはyが偶数
- x + y が奇数

:::


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
    putStrLn "practice1" -- "practice1"
    putStrLn "practice2" -- "practice2"
    putStrLn "practice3" -- "practice3"
~~~

`stack run practice`の結果を確認すると以下のようになります.

~~~ sh
> stack run practice
"practice1"
"practice2"
"practice3"
~~~

また,ghciと異なって,出力結果が同じ画面に現れないので,
以降のコード例では, その行の結果をコメントで書くこととします. コメント部分は,記述しなくても結果は変わらないので,省略しても構いません.
