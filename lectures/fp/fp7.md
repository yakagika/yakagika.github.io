---
title: 関数型プログラミング Ch7 代数的データ型と関係
description: 資料
tags:
    - algebra
    - lecture
    - statistics
    - haskell
featured: true
katex: true
date: 2026-07-10
tableOfContents: true
previousChapter: fp6.html
nextChapter: fp8.html
open: true
---

# 代数的データ型と関係 (集合論)

本章では, Haskell の `data` 宣言で作る **代数的データ型** を扱います. 代数的データ型には, **列挙型**, **直積型**, **直和型** があり, 構文として **レコード構文** などが存在します.

代数的データ型は文字通り, 数学における代数の構造を参照したデータ型であり, 代数的な定義と対応させることで様々なことが可能となります. 代数学を理解するためにはまず, 集合論の基礎を理解している必要があります. 本章では, 集合論と対応させる形で, 代数的データ型とは何であるかを理解することを目指します.

本章は 2 部構成です. **前半** では型を集合とみなし, 集合を **作る** 道具 (列挙・直積・直和・再帰) と Haskell のデータ型定義を対応させます. **後半** では集合の **間** のつながり, すなわち **関係** と, その特別な場合としての関数・同値関係・順序関係を扱います. 後半は次章の型クラス (Eq・Ord) の数学的な土台になります.

本章の地図として, 集合論と Haskell の対応をまとめておきます (それぞれ対応する節で説明します).

| 集合論 | Haskell | 節 |
| --- | --- | --- |
| 集合 / 要素 ($x \in S$) | 型 / 値 (`x :: S`) | 集合と列挙型 |
| 述語・全称 $\forall$・存在 $\exists$ | `Bool` を返す関数, `and` / `or` | 述語と量化 |
| 内包表記 (述語による部分集合の切り出し) | リスト内包表記 | 内包表記 |
| 直積 $A \times B$ | 直積型・レコード構文 | 直積型 |
| 直和 $A + B$ | 直和型 | 直和型 |
| 帰納的に定義された集合 | 再帰的データ型 | 再帰的データ型 |
| 関係 ($R \subseteq A \times B$) | 判定関数 `a -> b -> Bool` | 関係 |
| 関数 (全域・右一意な関係) | Haskell の関数 | 関数 |
| 演算 ($A^n \to A$ = 台の中で閉じた関数) | `a -> a -> a` などの関数 | 演算 |

::: note
**講義で辿る道筋**

**今日の到達点:** 型を「値の集まり」として読み, 直積・直和・再帰で対象の構造を記述できる. さらに, 対象間の関係を法則で区別できる.

板書と本文では, 次の順に進みます.

1. [集合と列挙型](#集合と列挙型): 型・値・パターンマッチ. [Exercise CH7-1](#exercise-ch7-1)
2. [直積型](#直積型)と[直和型](#直和型): フィールドを併せ持つ対象と, 場合によって形が異なる対象.
3. [再帰的データ型](#再帰的データ型): 有限の定義から列や木のように大きくなれる対象を作る.
4. [関係](#関係-直積の部分集合)・[同値関係](#同値関係-同じの一般化)・[順序関係](#順序関係-並べるの一般化): `Eq` / `Ord` の数学的な前提. [Exercise CH7-6](#exercise-ch7-6), [Exercise CH7-7](#exercise-ch7-7)

必要になったときに本文へ戻る節: [述語と量化](#述語と量化-集合を条件で語る), [内包表記](#内包表記-述語で部分集合を切り出す), レコードの細部, `type` / `newtype`, スマートコンストラクタ, 演算の一般のアリティ.
:::

::: warn
Haskell は代数学の一部である **圏論** と強い結びつきがあり, プログラムのデータ構造は圏論的に解釈することも可能となります. 特に Haskell の高度な機能, 多相型 (ポリモーフィズム), モナド, 状態系などは集合論的な理解よりも圏論的な理解のほうが適しています.
そこで, ここでは一旦集合論的に概要を把握し, 後の章 ([第9章](fp9.html) 以降) で圏論的な解釈を試みます.
:::

<!-- ==== 集合部 ==== -->

## 集合と列挙型

Haskell ではデータ型を集合と **みなすこと** ができます. Haskell の型はあくまで型であり, 厳密には集合ではありません. また, 後の節で出てくるリストを使った `内包表記` などの **集合論的な書き方** も数学における集合ではありません.
あくまで類似したものです.

しかし, Haskell を集合とみなすことで, 関数型プログラミングや, 代数的データ型の意味がより直感的に理解できるようになります.

このみなしが説明するのは型の **構造**, すなわち各型がどんな値の集まりとして作られているかです. 一方, [第1章](fp1.html)で見た静的型付けの保証 (プログラムを実行する前に型の不整合を検出できること) は, 値を集合に分類する見方からは出てきません. 型検査は値の分類ではなく, プログラムの式に **型付け規則** で型を割り当て, 「この式を実行するとどの型の値になるか」を実行せずに裏づける仕組みだからです. 集合とみなす読みの限界は本章の中にも現れます (例: [再帰的データ型](#再帰的データ型)の但し書きで見る無限の値, 「[関数: 特別な関係](#関数-特別な関係)」の節で見る型検査を通る部分関数). この保証のしくみを主題として扱う分野が型システムの理論で, 入門書に『型システムのしくみ』(遠藤侑介, ラムダノート, 2025) があります.

しばらく, 集合論と Haskell の対応について考えてみましょう.

::: note
特定の対象がそこに属するかどうかが明確に定められた, 対象の集まりを集合という.
:::

集合の細かな定義は置いておいて, この講義では取り敢えずこのくらいの認識で問題ありません. ここで「明確に定められた」とは, 所属を判定するアルゴリズムが必要だという意味ではなく, 何を要素とするかが定義によって定まっているという意味です.

例えば, ｢頭の良い人の集合｣のようなものは, ｢頭が良い基準｣が人によって異なるので, 集合とはみなせません.

ノーベル賞受賞者の集合, フィールズ賞受賞者の集合, メンサ会員の集合, XX模試の偏差値が70以上の人の集合, 特定の科目で85点以上取った人の集合などは, 何を要素とするかを基準によって定められます.

集合の表記法には, **外延(的)表記** 及び **内包(的)表記** という2通りが存在します.

外延表記とは, 特定の集合に含まれる要素を全て記述する方法です.
私が過去に飼ったことのある犬の種類の集合を `MyDogs` という名前で呼ぶと, `MyDogs` に属するモノたちを記号 `{ }` を使った外延表記によって以下のように書くことができます.


\begin{align*}
MyDogs = & \{ GoldenRetriever \\
         &, BlackRetriever    \\
         &, ShetlandSheepdog \\
         &, StandardPoodle \\
         &, Beagle \}
\end{align*}


このとき, `GoldenRetriever` や, `ShetlandSheepdog` は `MyDogs` の `要素` であるといい, 要素が特定の集合に属するとき,

$$ GoldenRetriever \in MyDogs $$ の様に書きます. 要素に属さないことは $Chihuahua \notin MyDogs$ と書きます.

集合には順番は関係ないため, $\{x,y\}=\{y,x\}$ となります. また, 一つの集合に同じ要素は2つ以上属することができず, $\{x,x\}$ のような集合は定義できません.

Haskell において, 集合に属する要素をすべて書き出す (列挙する) データ型を `列挙型` として定義できます.
データ型の宣言は, `data` のあとに続いて, `データ型の名前(型構築子)` を書き, `=` の後ろにその `中身(コンストラクタ/データ構築子)` を書きます.
型構築子やデータ構築子は, 大文字の英字で始めるのが規則です.

~~~ haskell
data MyDogs = GoldenRetriever
            | BlackRetriever
            | ShetlandSheepdog
            | StandardPoodle
            | Beagle
            deriving Show
~~~

::: warn

ちなみに, 大文字の英字で始まってさえいれば UTF-8 の文字や絵文字, 記号は使用できるので, 以下のような記述も可能ですが, あまりおすすめしません.

~~~ haskell
data My🐶   = Pゴールデンレトリーバー
            | Pブラックレトリーバー
            | Pシェットランドシープドッグ
            | Pスタンダードプードル
            | Pビーグル
            deriving Show
~~~
:::

`deriving Show` はコンストラクタを文字列に変換する関数 `show` を自動で導入するための記法です. 自分で定義することも可能ですが, 詳細に関しては後ほど扱います.

`deriving Show` を入れていない状態で

~~~ haskell
print GoldenRetriever
~~~

などを実行すると, 以下のエラーがでますが, `deriving Show` を追加することで, 表示することが可能となります.

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

なお, `print` の[実装](https://hackage.haskell.org/package/base-4.19.1.0/docs/src/System.IO.html#print)は

~~~ haskell
print :: Show a => a -> IO ()
print x = putStrLn (show x)
~~~

となっています.


要素が一つも属さない集合を `空集合` といい, 記号 $\varnothing$ または $\{\}$ によって表されます.
Haskell では, 値を持たない型として `Data.Void` に定義された `Void` があります. `Void` は空集合に対応する型です. 一方, 記号 $\bot$ で表す **ボトム** は型ではなく, 停止しない計算や実行時エラーを表す意味論上の値です. `undefined` のように, ボトムはどの型にも現れ得ます.

`Void` と同じ値を持たないデータ型は, コンストラクタを記述しないことで自分で実装することもできます. 例えばある人が犬を今までに一匹もかったことがない場合を想定し, その人の飼った犬の集合を `EmptyDogs` と呼ぶことにすると, $$ \mathrm{EmptyDogs} = \varnothing $$ となり, データ型としては以下のように定義されます. 値が存在しない空集合と対応していることが分かります.

~~~ haskell
data EmptyDogs
~~~

::: note

- Void の利用例

`Void` 型を使う場面は非常に限定的ですが, 「値が存在しないこと」を型で明示したい場合に利用されます. 関連して, 未実装の部分を仮置きするための値として `undefined` があります. `undefined` はどんな型の場所にも書ける特殊な値で, コンパイルは通りますが, 評価すると実行時エラーになります.

~~~ haskell
someFunc :: Int -> Int
someFunc = undefined

main :: IO ()
main = print (someFunc 1)   -- 実行時エラー: Prelude.undefined
~~~

「値が無い場合」の実用的な扱い (空リストに対する先頭要素など) は, `error` によるエラー送出や, 後の章で扱う `Maybe` 型を使うのが一般的です.
:::

単一の値だけを持つ **ユニット型** `()` も用意されています. 型の名前と唯一の値がどちらも `()` です.

::: note

### Exercise CH7-1

**曜日列挙型 `Weekday` と `isWeekend` / `nextDay`**

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


## 述語と量化: 集合を条件で語る

外延表記は, 要素を書き並べる方法でした. しかし「1 以上 10 以下の偶数」のように, 要素を **条件** で指定したいことも多くあります. その基礎になるのが **述語** です.

要素 $x$ について真偽が定まる主張を **述語(predicate)** といいます. たとえば「$x$ は偶数である」「$x \leq 5$ である」は, $x$ が決まれば真偽の定まる述語です. 述語 $p(x)$ は $x$ が決まらなければ真偽が定まりませんが, 集合 $X$ の要素すべて / いずれかについて述べると, 真偽の定まる **命題** になります. その作り方が次の 2 つです.

- **全称命題(universal proposition)**: ｢$X$ の **すべての** 要素が $p$ を満たす｣. 記号で $\forall x \in X.\ p(x)$ と書きます ($\forall$ は ｢すべての(for all)｣ を表す全称記号).
- **存在命題(existential proposition)**: ｢$p$ を満たす要素が $X$ に **少なくとも 1 つ存在する**｣. 記号で $\exists x \in X.\ p(x)$ と書きます ($\exists$ は ｢存在する(there exists)｣ を表す存在記号).

たとえば ｢すべての非負実数は $0$ 以上である｣ は $\forall x \in \mathbb{R}^+.\ x \geq 0$ (全称命題), ｢平方して $2$ になる実数が存在する｣ は $\exists x \in \mathbb{R}.\ x^2 = 2$ (存在命題) です.

この $\forall$ / $\exists$ を使うと, 言葉で述べてきた集合の性質を厳密に書けます. たとえば先に ｢集合には順序がなく $\{x, y\} = \{y, x\}$｣ と述べた **集合の等しさ** は, ｢属する要素がまったく同じ｣こと, すなわち

$$A = B \quad\Longleftrightarrow\quad \forall x.\ (x \in A \iff x \in B)$$

と定義できます. 後の章([第8章](fp8.html) 以降)では, 代数構造の **法則** (結合律や単位元の存在など) もこの全称・存在命題で正確に記述します. また本章の後半でも, 関係の性質 (反射律・対称律など) を $\forall$ で書きます.

### 部分集合

集合には集合が属することも可能で, 集合 $S$ が $T$ に属するとき $S \in T$ が成り立ちます. また, 集合 $S$ の要素を幾つか取り出した集合 $T$ を $S$ の **部分集合** といい, $T \subseteq S$ と表記します. いま導入した全称命題を使うと, これは ｢$T$ のすべての要素が $S$ にも属する｣こと, すなわち

$$T \subseteq S \quad\Longleftrightarrow\quad \forall x.\ (x \in T \implies x \in S)$$

と厳密に定義できます. この定義では, 空集合 $\varnothing$ と $S$ 自身も $S$ の部分集合に含まれます.

$S = \{x, y, z\}$ のとき, $S$ の部分集合は

$$\{x\},\ \{y\},\ \{z\},\ \{x, y\},\ \{x, z\},\ \{y, z\},\ \{x, y, z\},\ \varnothing$$

となります.

この包含関係を **ベン図** で見ると次のようになります. たとえば $T = \{x, y\}$ は $S = \{x, y, z\}$ の部分集合の 1 つです.

<svg viewBox="0 0 460 252" width="100%" style="max-width: 460px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="部分集合のベン図. 大きな楕円 S の内側に小さな楕円 T が完全に含まれている. T の中に要素 x と y があり, T の外だが S の中に要素 z がある. T のすべての要素は S にも属するので T ⊆ S となる.">
  <g stroke="currentColor" stroke-width="1.5">
    <ellipse cx="230" cy="112" rx="172" ry="90" fill="currentColor" fill-opacity="0.03"/>
    <ellipse cx="172" cy="124" rx="88" ry="52" fill="currentColor" fill-opacity="0.10"/>
  </g>
  <g fill="currentColor" font-family="serif" font-style="italic" font-size="17">
    <text x="350" y="62">S</text>
    <text x="116" y="94">T</text>
  </g>
  <g fill="currentColor">
    <circle cx="146" cy="120" r="3.2"/>
    <circle cx="205" cy="138" r="3.2"/>
    <circle cx="330" cy="140" r="3.2"/>
  </g>
  <g fill="currentColor" font-family="serif" font-style="italic" font-size="14" text-anchor="middle">
    <text x="146" y="108">x</text>
    <text x="205" y="126">y</text>
    <text x="330" y="128">z</text>
  </g>
  <g fill="currentColor" font-size="12" text-anchor="middle">
    <text x="230" y="228">T の要素 (x, y) はどれも自動的に S の要素 (T ⊆ S)</text>
    <text x="230" y="246" opacity="0.72">z のように「S にはあるが T にはない」要素があってもよい</text>
  </g>
</svg>

集合 $X$ と述語 $p$ が与えられると, ｢$X$ の要素のうち $p$ を満たすもの｣という部分集合がひとつ定まります. この部分集合を書き下すための記法が, 次節の **内包表記** です.


## 内包表記: 述語で部分集合を切り出す

列挙型において見た **外延表記** に対して, **内包表記** とは, 集合を述語によって指定する方法です.

$x$ の属する集合を $X$, 条件式 (述語) を $p(x)$ とすると, 内包表記では

$$S = \{x \mid x \in X, p(x)\}$$

という記法で, ｢**$X$ の要素のうち $p(x)$ を満たす要素のみからなる集合 $S$**｣, すなわち述語 $p$ で切り出した $X$ の部分集合を定義します.

例として, $\mathbb{R}^+$ を非負の実数としたとき, $5$ 以下の非負の実数は

$$\{x \mid x \in \mathbb{R}^+, x \leq 5\}$$

と書けます.

Haskell の代数的データ型では, 内包表記に基づくデータ型の定義そのものは提供されていません. しかし, **値のレベル** では **リスト内包表記** によって擬似的な集合計算が可能です. また **型のレベル** で述語による絞り込みを実現するイディオムとして **スマートコンストラクタ** があります (本章後半の「スマートコンストラクタ (発展)」で扱います).

### リスト内包表記による計算

**リスト内包表記** は Haskell の標準構文で, 集合論の内包表記と非常に似た形でリストを構成できます. 集合論では

$$S = \{x \mid x \in X, p(x)\}$$

と書くところを, Haskell では

~~~ haskell
s = [ x | x <- xs, p x ]
~~~

と書きます. `x <- xs` が $x \in X$ に対応する **ジェネレータ**, `p x` が述語 $p(x)$ に対応する **ガード** です.

例として, 集合論における「1 以上 10 以下の整数のうち偶数のみ」

$$E = \{x \mid x \in \mathbb{Z}, 1 \leq x \leq 10, x \bmod 2 = 0\}$$

は, リスト内包表記では以下のように記述できます.

~~~ haskell
evens :: [Int]
evens = [ x | x <- [1..10], x `mod` 2 == 0 ]
-- evens == [2,4,6,8,10]
~~~

ジェネレータやガードは複数書くこともでき, 集合論における直積 (複数の変数の並行走査) や条件の連言 (AND) に自然に対応します.

~~~ haskell
-- {(x,y) | x ∈ [1..3], y ∈ [1..3], x /= y}
pairs :: [(Int,Int)]
pairs = [ (x,y) | x <- [1..3], y <- [1..3], x /= y ]
-- [(1,2),(1,3),(2,1),(2,3),(3,1),(3,2)]
~~~

ただし, リスト内包表記は **値のレベル** での構成であり, 得られるのはあくまで `[Int]` などのリストです. 「型として偶数のみからなる新しいデータ型」を定義しているわけではない点に注意してください.

::: warn
ここではリスト内包表記を集合の内包表記の **比喩** として紹介していますが, リストと集合は別物です. リストには **順序があり, 同じ要素を重複して含むことができ, 要素の等価性も個別に比較する必要があります**. 一方, 数学における集合は順序を持たず, 同じ要素を重複して含むこともありません. リストが **再帰的データ型** としてどう定義されるかは本章の「[再帰的データ型](#再帰的データ型)」で, それを任意の要素型へ一般化し代数構造 (モノイド) を載せる話は [第9章](fp9.html) で扱います.

したがって, 実際に **集合としての操作** (要素の存在判定, 和集合 $\cup$, 積集合 $\cap$, 差集合 $\setminus$, 重複の自動排除など) が必要な場合は, リストではなく `containers` パッケージの提供する `Data.Set` の `Set` 型を利用するほうが適切であり, 計算量の面でも有利です (リストの要素検索は $O(n)$, `Set` は $O(\log n)$ であり, この差の読み方は [補足A](fp_a1.html) 参照).

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

リスト内包表記は「集合の内包表記と構文が似ていて直感的に書ける」という **記法の便利さ** のために使うもので, 集合演算そのものを扱いたい場合は `Set` を用いるのが Haskell における一般的な流儀です.
:::

::: note

### Exercise CH7-2

**約数とピタゴラス数(リスト内包表記)**

1. 正の整数 `n` を受け取り, `n` の正の約数(nを割って余りのでない数)をすべて昇順に並べたリストを返す関数 `divisors :: Int -> [Int]` をリスト内包表記を用いて実装してください.

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

-- 2. ピタゴラス数: a <= b <= c <= n かつ a^2 + b^2 = c^2
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

### 有限集合での ∀・∃ の機械検査

述語と量化の節で導入した $\forall$ / $\exists$ は, 集合が **有限** であれば Haskell で機械的に検査できます. 道具は 2 つの標準関数です.

- `and :: [Bool] -> Bool`: リストの要素が **すべて** `True` のとき `True`
- `or  :: [Bool] -> Bool`: リストの要素の **どれかが** `True` のとき `True`

[第6章](fp6.html)の fold の言葉で言えば `and = foldr (&&) True`, `or = foldr (||) False` です. これをリスト内包表記と組み合わせると, 全称命題と存在命題の有限版がそのまま書けます.

$$\forall x \in X.\ p(x) \;\approx\; \texttt{and [ p x | x <- xs ]}, \qquad \exists x \in X.\ p(x) \;\approx\; \texttt{or [ p x | x <- xs ]}$$

この 2 つを関数として切り出しておきます (`even` は偶数判定の標準関数で, `x mod 2 == 0` と同じです).

~~~ haskell
-- ∀x ∈ xs. p x の有限版
forallOn :: [a] -> (a -> Bool) -> Bool
forallOn xs p = and [ p x | x <- xs ]

-- ∃x ∈ xs. p x の有限版
existsOn :: [a] -> (a -> Bool) -> Bool
existsOn xs p = or [ p x | x <- xs ]

main :: IO ()
main = do
    print $ forallOn [2,4,6,8,10] even            -- True  (すべて偶数)
    print $ forallOn [1..10] even                 -- False (奇数が混ざる)
    print $ existsOn [1..10] (\x -> x * x == 25)  -- True  (5 がある)
    print $ existsOn [1..10] (\x -> x * x == 26)  -- False
~~~

標準ライブラリには同じ働きをする `all` / `any` という関数もあります (`forallOn xs p = all p xs`, `existsOn xs p = any p xs`).

::: warn
これは **有限の全数検査** であって **証明ではありません**. 検査できるのは与えた有限リストの範囲だけで, $\mathbb{Z}$ 全体のような無限の集合については何も保証しません (無限リストに `and` を適用すると, 反例が見つかれば `False` を返しますが, すべて `True` の場合は止まりません). それでも「定義した述語や関係が意図した性質を持つか」を手元で素早く確かめる道具としては非常に有効です. 性質をランダムな入力で検査する本格的な仕組み (QuickCheck) は [第8章](fp8.html)のコラムで紹介します.
:::

本章の後半では, この `forallOn` / `existsOn` を使って **関係の法則** (反射律・対称律・推移律など) を全数検査します.


## 直積型

$A \times B = \{(a, b) \mid a \in A, b \in B\}$ を $A$ と $B$ の **直積(Cartesian Product)** といいます. 直積は $A$ と $B$ から要素を一つずつ選んで並べた組 $(a, b)$ 全体からなる集合です. 日本語では **積集合(intersection)** と字面が似ていますが異なる概念なので注意しましょう. 両者の違いを図で対比すると次のようになります (積集合の正式な定義は直和型の節で扱います).

<svg viewBox="0 0 560 262" width="100%" style="max-width: 560px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="積集合と直積の対比図. 左は 2 つの円 A と B が重なるベン図で, 重なった部分が積集合 A ∩ B (両方に属する要素の集合). 右は A = {x, y, z} と B = {1, 2} の直積を格子で表した図で, 6 個の組 (x,1) から (z,2) が並ぶ. 積集合は共通の要素の集合, 直積は組の集合であり別物.">
  <defs>
    <clipPath id="fp7v2-venn-a"><circle cx="88" cy="122" r="54"/></clipPath>
  </defs>
  <text x="125" y="34" fill="currentColor" font-size="13" text-anchor="middle" font-weight="600">積集合 A ∩ B</text>
  <circle cx="152" cy="122" r="54" fill="currentColor" fill-opacity="0.16" clip-path="url(#fp7v2-venn-a)" stroke="none"/>
  <g stroke="currentColor" stroke-width="1.5" fill="none">
    <circle cx="88" cy="122" r="54"/>
    <circle cx="152" cy="122" r="54"/>
  </g>
  <g fill="currentColor" font-family="serif" font-style="italic" font-size="15">
    <text x="42" y="72">A</text>
    <text x="188" y="72">B</text>
  </g>
  <text x="120" y="205" fill="currentColor" font-size="11" text-anchor="middle" opacity="0.72">A と B の両方に属する要素の集合</text>
  <line x1="255" y1="44" x2="255" y2="210" stroke="currentColor" stroke-width="1" opacity="0.15"/>
  <text x="425" y="34" fill="currentColor" font-size="13" text-anchor="middle" font-weight="600">直積 A × B</text>
  <g stroke="currentColor" stroke-width="1" opacity="0.22">
    <line x1="340" y1="88" x2="340" y2="178"/>
    <line x1="425" y1="88" x2="425" y2="178"/>
    <line x1="510" y1="88" x2="510" y2="178"/>
    <line x1="322" y1="106" x2="528" y2="106"/>
    <line x1="322" y1="166" x2="528" y2="166"/>
  </g>
  <g fill="currentColor" font-family="serif" font-style="italic" font-size="14" text-anchor="middle">
    <text x="340" y="76">x</text>
    <text x="425" y="76">y</text>
    <text x="510" y="76">z</text>
    <text x="304" y="110">1</text>
    <text x="304" y="170">2</text>
  </g>
  <g fill="currentColor">
    <circle cx="340" cy="106" r="3.2"/><circle cx="425" cy="106" r="3.2"/><circle cx="510" cy="106" r="3.2"/>
    <circle cx="340" cy="166" r="3.2"/><circle cx="425" cy="166" r="3.2"/><circle cx="510" cy="166" r="3.2"/>
  </g>
  <g fill="currentColor" font-family="serif" font-style="italic" font-size="12" text-anchor="middle">
    <text x="340" y="126">(x,1)</text>
    <text x="425" y="126">(y,1)</text>
    <text x="510" y="126">(z,1)</text>
    <text x="340" y="186">(x,2)</text>
    <text x="425" y="186">(y,2)</text>
    <text x="510" y="186">(z,2)</text>
  </g>
  <text x="425" y="205" fill="currentColor" font-size="11" text-anchor="middle" opacity="0.72">A = {x, y, z} と B = {1, 2} の組 (|A × B| = 3 × 2 = 6)</text>
  <text x="280" y="240" fill="currentColor" font-size="12" text-anchor="middle">字面は似ているが別物 (積集合は「共通の要素」の集合, 直積は「組 (ペア)」の集合)</text>
</svg>

事例として $\mathrm{MyDogs}$ と整数の集合 $\mathbb{Z}$ の直積を考えると,

$$\mathrm{MyDogs} \times \mathbb{Z} = \{(d, n) \mid d \in \mathrm{MyDogs}, n \in \mathbb{Z}\}$$

となり, 「犬種と整数(たとえば年齢)のペア」全体の集合を表します. たとえば $(\mathrm{GoldenRetriever}, 3), (\mathrm{Beagle}, 7) \in \mathrm{MyDogs} \times \mathbb{Z}$ です.

Haskell では既に定義した `MyDogs` と `Int` の直積に相当する型を, 次のように定義できます. ここでは「犬種と年齢のペアを持つデータ型 `DogAge`」を定義します.

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

### 数え上げ: 「代数的」の意味 (その1)

列挙型は要素を数えられます. 型 $A$ の要素数 (集合の大きさ) を $|A|$ と書くと, $|\mathrm{MyDogs}| = 5$, $|\mathrm{Bool}| = 2$ です. では直積の大きさはどうなるでしょうか. 組 $(a, b)$ の作り方は「$a$ の選び方 $|A|$ 通り × $b$ の選び方 $|B|$ 通り」なので,

$$|A \times B| = |A| \cdot |B|$$

となります. **直積の大きさは掛け算** です. 実際に Haskell で確かめてみましょう. 全要素の列挙には, リスト内包表記のジェネレータ 2 本 (= 直積の走査) がそのまま使えます.

~~~ haskell
data Size = Small | Medium | Large deriving Show

allDogs :: [MyDogs]
allDogs = [GoldenRetriever, BlackRetriever, ShetlandSheepdog, StandardPoodle, Beagle]

allSizes :: [Size]
allSizes = [Small, Medium, Large]

-- MyDogs × Size の全要素
allPairs :: [(MyDogs, Size)]
allPairs = [ (d, s) | d <- allDogs, s <- allSizes ]

main :: IO ()
main = do
    print $ length allDogs   -- 5
    print $ length allSizes  -- 3
    print $ length allPairs  -- 15  (= 5 * 3)
~~~

::: note
数え上げが素直に効くのは `MyDogs` や `Bool` のような有限の型です. `Int` も実は有限ですが ($2^{64}$ 通り), 巨大すぎて列挙には向きません. `Integer` (多倍長整数) や `String` は無限の集合に対応します.
:::

### パターンマッチによる更新と as パターン

直積型の値から **一部だけを変えた値** が欲しいことはよくあります. Haskell では値は不変 (再代入できない) なので, ここでの「更新」とは元の値を書き換えることではなく, **一部を変えた新しい値を作って返す** ことを指します.

レコード構文を導入する前に, まず素朴な方法を見ておきましょう. **パターンマッチで全フィールドを取り出し, コンストラクタで組み立て直す** やり方です. `DogAge` の年齢を 1 増やす関数は次のように書けます.

~~~ haskell
growOlder :: DogAge -> DogAge
growOlder (MkDogAge b n) = MkDogAge b (n + 1)
~~~

変えたいのは年齢 `n` だけですが, **変えないフィールド `b` まで明示的に取り出して並べ直す** 必要があります. フィールド数が増えるほど記述は長くなり, さらに `MkDogAge b (n + 1)` は **フィールドの順序に依存する** ため, 将来 `data` 定義のフィールドを並べ替えると意味が静かに壊れます. この「全部書き直す」「順序に縛られる」という弱点は, 次節の **レコード更新構文** で解消されます.

**as パターン.** 値を分解して組み立て直すのではなく, **元の値全体をそのまま使いたい** こともあります. たとえば「年齢が一定以上ならそれ以上歳を取らせず元の値をそのまま返し, そうでなければ 1 歳増やす」関数では,「分解した部品 `n`」と「元の値全体」の両方が必要です. このとき, **パターン全体に名前を束縛する** `名前@(パターン)` という記法 (**as パターン**) が使えます.

~~~ haskell
growOlder' :: DogAge -> DogAge
growOlder' dog@(MkDogAge b n)
  | n >= 20   = dog                 -- 元の値 dog をそのまま返す
  | otherwise = MkDogAge b (n + 1)  -- 1 歳増やした新しい値
~~~

`dog@(MkDogAge b n)` は, 引数全体を `dog` に束縛しつつ, 同時に中身を `MkDogAge b n` に分解して各フィールドを `b`, `n` で取り出します. これにより `dog` (全体) と `b`, `n` (部品) を一つのパターンで同時に扱え, 最初の分岐のように **値をそのまま返す** ことも, 2 番目の分岐のように **作り直す** こともできます.

as パターンは「全体と部分の両方が要る」場面のための記法であり, **更新そのものの道具ではない** 点に注意してください. 1 フィールドだけ変えて残りをコピーする更新は, 次節のレコード更新構文の方が簡潔です (値を分解せず, 変えたいフィールドだけ書けます).

### レコード構文

直積型では **レコード構文(record syntax)** を用いて各フィールドに名前を付けることができます. 直積型ではコンストラクタが一つのため, レコード構文の恩恵(アクセサ関数の自動生成やフィールド名による値の生成)が特に活きます.

~~~ haskell
data DogAge = MkDogAge { breed :: MyDogs
                       , age   :: Int
                       }
            deriving Show
~~~

この定義により, 以下のアクセサ関数が **自動的に** 定義されます.

- `breed :: DogAge -> MyDogs`
- `age   :: DogAge -> Int`

これらの **アクセサ関数(accessor function)** は, レコード構文の定義から **GHCが自動生成するトップレベル関数** で, レコード値からフィールドの値を取り出す役割を持ちます. たとえば `breed` は手書きすれば

~~~ haskell
breed :: DogAge -> MyDogs
breed (MkDogAge b _) = b
~~~

と書いたものと等価で, パターンマッチによる取り出しを関数として包んだものに過ぎません. レコード構文の利点は, このような取り出し関数を **フィールドごとに自前で書く手間が省ける** ことにあります.

アクセサ関数は他の関数と全く同様に **第一級の値(first-class value)** として扱えるため, 高階関数に渡したり, 関数合成 `(.)` で他の関数と繋げたり, 部分適用したりできます. パターンマッチによる取り出しを関数の中に書く場合と比べ, 簡潔に「フィールドを取り出してから何かする」という処理を組み立てられるのが大きな利点です.

~~~ haskell
dogs :: [DogAge]
dogs = [ MkDogAge GoldenRetriever 3
       , MkDogAge Beagle           7
       , MkDogAge StandardPoodle   5
       ]

-- 高階関数に渡す: 全ての犬の犬種だけを取り出したリスト
allBreeds :: [MyDogs]
allBreeds = map breed dogs
-- [GoldenRetriever, Beagle, StandardPoodle]

-- 高階関数に渡す: 年齢が 5 歳以上の犬を絞り込む
matureDogs :: [DogAge]
matureDogs = filter ((>= 5) . age) dogs
-- [MkDogAge Beagle 7, MkDogAge StandardPoodle 5]

-- 関数合成: 「犬種を取り出して show する」を一つの関数として定義
showBreed :: DogAge -> String
showBreed = show . breed
~~~

`filter ((>= 5) . age) dogs` のように, アクセサ関数 `age` と比較 `(>= 5)` を `(.)` で合成すれば「年齢を取り出して 5 以上か判定する」という関数を一行で表現できます. もしレコード構文を使わずにパターンマッチで毎回書いていたら, この箇所は `filter (\d -> case d of MkDogAge _ n -> n >= 5) dogs` のように冗長になります.

なお, アクセサ関数名はモジュール内のトップレベルにそのまま展開されるため, 同じモジュールで別のレコード型に同名のフィールドを定義すると衝突します(回避には `DuplicateRecordFields` 言語拡張や, `dogAge` / `personAge` のような接頭辞付きの命名規約などが用いられます).

::: note

集合論では, 直積集合 $A \times B$ から各成分を取り出す関数を **射影(projection)** といい, $\pi_1, \pi_2$ で表します. 形式的には

$$\pi_1 : A \times B \to A, \quad \pi_1((a, b)) = a$$

$$\pi_2 : A \times B \to B, \quad \pi_2((a, b)) = b$$

と定義され, 順序対 $(a, b) \in A \times B$ から第1成分・第2成分をそれぞれ取り出す操作を表します. これは $n$ 個の集合の直積 $A_1 \times A_2 \times \dots \times A_n$ に対しても自然に一般化され, 第 $i$ 成分を取り出す射影 $\pi_i : A_1 \times \dots \times A_n \to A_i$ が同様に定義されます.

:::

レコード構文によって自動生成されるアクセサ関数は, この射影に対応します. すなわち $\mathrm{DogAge} = \mathrm{MyDogs} \times \mathrm{Int}$ と見なせば, `breed` は第1成分への射影 $\pi_1$, `age` は第2成分への射影 $\pi_2$ にあたり,

$$\mathrm{breed}((d, n) \in \mathrm{DogAge}) = d$$

$$\mathrm{age}((d, n) \in \mathrm{DogAge}) = n$$

と書けます. このように, 直積型のレコード構文は集合論における直積と射影の構造を, そのまま Haskell の構文として写したものになっています.

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

また, レコード構文では **一部のフィールドのみを更新した新しい値** を作る記法も利用できます. 先に見たとおり値は不変なので, ここでも「更新」とは元の値を変更するのではなく, 一部を書き換えた **新しい値を返す** ことを意味します.

~~~ haskell
olderGolden :: DogAge
olderGolden = goldenAge { age = 10 }
-- MkDogAge { breed = GoldenRetriever, age = 10 }
~~~

先ほどパターンマッチで全フィールドを書き直していた `growOlder` も, レコード更新構文なら **変えたい `age` だけ** を書けば済みます. `breed` は自動的にコピーされ, フィールドを並べ替えても壊れません.

~~~ haskell
-- 位置パターンで書き直していた growOlder が, これだけ簡潔になる
growOlder :: DogAge -> DogAge
growOlder d = d { age = age d + 1 }
~~~

直和型の場合と異なり, 直積型のレコードフィールドは単一のコンストラクタに属しているため **アクセサは全域関数** であり, 実行時エラーの心配はありません.

::: warn
直和型(複数のコンストラクタを持つ型)でレコード構文を使うと, **一部のコンストラクタにしか存在しないフィールドのアクセサは部分関数になる** 点に注意が必要です. たとえば `MyDogs` を使って, 犬種だけを持つコンストラクタと, 犬種と年齢の両方を持つコンストラクタを併せ持つ型を考えると以下のようになります.

~~~ haskell
data DogInfo = JustBreed { dogBreed :: MyDogs }
             | WithAge   { dogBreed :: MyDogs, dogAge :: Int }
             deriving Show

ghci> dogAge (JustBreed GoldenRetriever)
*** Exception: No match in record selector dogAge
~~~

`dogAge` は `WithAge` のコンストラクタにしか定義されていないため, `JustBreed` の値に適用すると実行時エラーになります. 直和型でレコード構文を使う場合は, アクセサで直接取り出すのではなく **パターンマッチで取り出す** か, 後の章で扱う `Maybe` を返す形に包み直すのが安全です. この「一部の入力で結果が定義されない関数」については, 本章後半の「関数: 特別な関係」の節でもう一度立ち返ります.
:::

::: note

### Exercise CH7-3

**人物型 `Person` のレコード操作(更新構文・アクセサ)**

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

## 直和型

集合 $A$, $B$ の **和集合(union)** を $A \cup B$, **積集合(intersection)** を $A \cap B$ と表し, それぞれ以下で定義されます.

$$A \cup B = \{x \mid x \in A \lor x \in B\}$$

$$A \cap B = \{x \mid x \in A \land x \in B\}$$

$A \cap B = \varnothing$ のとき, $A \cup B$ を $A$ と $B$ の **直和(Direct sum)** といいます.

3 つの概念をベン図で並べると次のとおりです.

<svg viewBox="0 0 560 232" width="100%" style="max-width: 560px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="和集合・積集合・直和のベン図による対比. 左: 重なった 2 円の全体が塗られた和集合 A ∪ B (A または B に属する要素). 中央: 重なり部分だけが塗られた積集合 A ∩ B (両方に属する要素). 右: 離れた 2 円がともに塗られた直和 (共通部分が無い場合の和集合). Haskell の直和型はタグで共通部分を強制的に無くした和集合にあたる.">
  <defs>
    <clipPath id="fp7v2-venn-b"><circle cx="256" cy="110" r="42"/></clipPath>
  </defs>
  <g fill="currentColor" font-size="13" text-anchor="middle" font-weight="600">
    <text x="100" y="30">和集合 A ∪ B</text>
    <text x="280" y="30">積集合 A ∩ B</text>
    <text x="460" y="30">直和 (A ∩ B = φ)</text>
  </g>
  <g opacity="0.16" fill="currentColor">
    <circle cx="76" cy="110" r="42"/>
    <circle cx="124" cy="110" r="42"/>
  </g>
  <circle cx="304" cy="110" r="42" fill="currentColor" fill-opacity="0.18" clip-path="url(#fp7v2-venn-b)" stroke="none"/>
  <g opacity="0.16" fill="currentColor">
    <circle cx="414" cy="110" r="42"/>
    <circle cx="506" cy="110" r="42"/>
  </g>
  <g stroke="currentColor" stroke-width="1.5" fill="none">
    <circle cx="76" cy="110" r="42"/>
    <circle cx="124" cy="110" r="42"/>
    <circle cx="256" cy="110" r="42"/>
    <circle cx="304" cy="110" r="42"/>
    <circle cx="414" cy="110" r="42"/>
    <circle cx="506" cy="110" r="42"/>
  </g>
  <g fill="currentColor" font-family="serif" font-style="italic" font-size="14" text-anchor="middle">
    <text x="52" y="62">A</text>
    <text x="148" y="62">B</text>
    <text x="232" y="62">A</text>
    <text x="328" y="62">B</text>
    <text x="414" y="58">A</text>
    <text x="506" y="58">B</text>
  </g>
  <g fill="currentColor" font-size="11" text-anchor="middle" opacity="0.72">
    <text x="100" y="182">A または B に属する要素</text>
    <text x="280" y="182">A と B の両方に属する要素</text>
    <text x="460" y="182">共通部分の無い和集合</text>
  </g>
  <text x="280" y="216" fill="currentColor" font-size="12" text-anchor="middle">Haskell の直和型 = タグ (コンストラクタ) で共通部分を強制的に無くした和集合 (常に直和)</text>
</svg>

事例として $A, B \subseteq \mathrm{MyDogs}$, $A = \{\mathrm{GoldenRetriever}, \mathrm{BlackRetriever}, \mathrm{ShetlandSheepdog}\}$, $B = \{\mathrm{BlackRetriever}, \mathrm{StandardPoodle}\}$ のとき, 和集合 $A \cup B$ と積集合 $A \cap B$ はそれぞれ

$$A \cup B = \{\mathrm{GoldenRetriever}, \mathrm{BlackRetriever}, \mathrm{ShetlandSheepdog}, \mathrm{StandardPoodle}\}$$

$$A \cap B = \{\mathrm{BlackRetriever}\}$$

となります. 和集合は「$A$ または $B$ のいずれかに属する要素」を集めた集合, 積集合は「$A$ と $B$ の両方に属する要素」のみを集めた集合です. このとき $A \cap B = \{\mathrm{BlackRetriever}\} \neq \varnothing$ なので, $A$ と $B$ は直和にはなりません. 一方, $A = \{\mathrm{GoldenRetriever}, \mathrm{ShetlandSheepdog}\}$, $B = \{\mathrm{BlackRetriever}, \mathrm{StandardPoodle}\}$ のように共通要素がない場合は $A \cap B = \varnothing$ となり, $A \cup B$ は $A$ と $B$ の直和となります.

Haskell では既に定義した `Int` と `MyDogs` の直和に相当する型を, 次のように定義できます. ここでは「整数または `MyDogs` のいずれかの値を持てるデータ型 `IntOrDog`」を定義します.

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

$$\mathrm{IntOrDog} = \mathrm{Int} \cup \mathrm{MyDogs} \quad (\mathrm{Int} \cap \mathrm{MyDogs} = \varnothing)$$

という **直和** になります. 一般に, 代数的データ型の `|` で分けたコンストラクタは必ずタグで区別されるため, Haskell の直和型は常に直和の構造を持ちます.

なお, データ構築子は任意個の引数を取ることができ, `MkPair Int MyDogs` のように複数の型を並べると, その部分は **直積** になります. つまり直和型の各コンストラクタは「いくつかの直積をタグ付きで合流させたもの」として解釈できます.


このように, 直和にすることで互いに異なる型の値を一つの型に集約でき, 型安全性を保ったまま「複数の型のいずれかをとる値」を表現できるようになります. 動的型付け言語におけるダックタイピングに似た柔軟さを, 型システムの保証を壊さずに実現する手段と考えることができます.

### 数え上げ: 「代数的」の意味 (その2)

直和の大きさも数えてみましょう. タグで区別されるため要素が重複することはなく, 選び方は「$A$ から選ぶ $|A|$ 通り, または $B$ から選ぶ $|B|$ 通り」なので,

$$|A + B| = |A| + |B|$$

となります (直和を $A + B$ とも書きます). **直和の大きさは足し算** です.

~~~ haskell
data DogOrSize = ADog  MyDogs
               | ASize Size
               deriving Show

-- DogOrSize の全要素: タグ付きで合流させる
allDogOrSize :: [DogOrSize]
allDogOrSize = [ ADog d | d <- allDogs ] ++ [ ASize s | s <- allSizes ]

main :: IO ()
main = print $ length allDogOrSize   -- 8  (= 5 + 3)
~~~

さらに, 本章冒頭で見たユニット型 `()` は要素がちょうど 1 つ ($|()| = 1$), `Void` は要素が 0 個 ($|\mathrm{Void}| = 0$) でした. これで型の世界に **0, 1, 足し算 (直和), 掛け算 (直積)** が揃ったことになります. これが「**代数的** データ型」という名前の由来です. 型は数のように「計算」できる対象なのです.

::: note
発展として, 関数型 `A -> B` の要素数は $|B|^{|A|}$ (指数) になります. たとえば `Bool -> Bool` の関数は $2^2 = 4$ 通りしかありません (恒等, 反転, 常に `True`, 常に `False`). また $|A \times ()| = |A| \cdot 1 = |A|$, $|A + \mathrm{Void}| = |A| + 0 = |A|$ のように, 数の代数法則 (1 を掛けても 0 を足しても変わらない) がそのまま型の対応として現れます.
:::

::: note

### Exercise CH7-4

**図形型 `Shape` と面積計算(ヘロンの公式)**

1. 図形を表す直和型 `Shape` を以下の3つのコンストラクタで定義してください.
    - `Circle` : 半径(`Double`)を1つ持つ
    - `Rectangle` : 幅と高さ(`Double` 2つ)を持つ
    - `Triangle` : 3辺の長さ(`Double` 3つ)を持つ

2. `Shape` の値を受け取り, その図形の面積を返す関数 `area :: Shape -> Double` をパターンマッチで実装してください. 三角形の面積はヘロンの公式

    $$s = \frac{a + b + c}{2}, \quad S = \sqrt{s(s-a)(s-b)(s-c)}$$

    を用います. 各変数の意味は以下の通りです.

    - $a, b, c$: 三角形の **3 辺の長さ** (`Triangle` コンストラクタの 3 つの `Double` に対応)
    - $s$: **半周長 (semi-perimeter)**. すなわち 3 辺の長さの和の半分 $\frac{a+b+c}{2}$
    - $S$: 求める **三角形の面積**

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


## 再帰的データ型

ここまでのデータ型 (直積・直和) は, **すでにある型を有限個組み合わせる** ものでした. もう 1 つ強力な作り方があります. **データ型が自分自身を使って定義される** もので, これを **再帰的データ型 (recursive data type)** といいます. 有限の定義で, いくらでも大きくなりうるデータ (列や木など) を表せます.

代表例として **自然数** を作ってみます. 「自然数とは, $0$ であるか, またはある自然数の **次の数 (successor)** である」と定義できます. これをそのまま直和型にすると, 次のようになります.

~~~ haskell
data Nat = Zero | Succ Nat deriving (Show, Eq)
~~~

コンストラクタ `Succ` が引数に **`Nat` 自身** を取っている点が再帰です. 値は `Zero`, `Succ Zero`, `Succ (Succ Zero)`, … と続き, それぞれ $0, 1, 2, \dots$ を表します. たとえば $3$ は `Succ (Succ (Succ Zero))` です. (`Succ` を何段も重ねるのは煩雑ですが, [第8章](fp8.html)で `Nat` に数値リテラルを使えるようにし, `3 :: Nat` と書けるようにします.)

### 構成子は「型を生成する演算」

この `Zero` と `Succ` を, 数学の目で見直してみましょう.

- `Zero` は引数を取らず, それ自体が `Nat` の値を 1 つ作ります. 数学的には **0 項演算** (引数を 0 個取る演算), すなわち **定数** です.
- `Succ :: Nat -> Nat` は `Nat` を 1 つ受け取って `Nat` を返します. 数学的には **1 項演算** です.

一般に, $n$ 個の引数を取る構成子は $n$ **項演算** とみなせます (「演算」という言葉の正式な定義は, 本章後半の「演算: 台の上で閉じた関数」の節で与えます). 遡れば, 本章で書いてきた構成子はすべてこの意味の演算でした. `GoldenRetriever` は 0 項演算 (定数), `MkInt :: Int -> IntOrDog` は 1 項演算, `MkDogAge :: MyDogs -> Int -> DogAge` は 2 項演算です. つまり **`data` 宣言とは, 「この型の値をどの演算で生成するか」の宣言** だったのです.

この見方をすると, `Nat` の定義が数学で自然数を定める **ペアノの公理 (Peano axioms)** とまったく同じ構成であることがはっきりします. ペアノの公理は, 自然数を「台となる集合 $\mathbb{N}$ と, 定数 $0$ と, 1 項演算 $S$ (**後者関数** successor, 「次の数」を返す) の **組** $(\mathbb{N},\ 0,\ S)$」として特徴づけます. `(Zero, Succ)` は, この $(0, S)$ にそのまま対応します.

ここで 1 つ, 取り違えやすい点に注意します. 演算が働く土台の集合 (**台集合** といいます) は **型 `Nat` 全体 = 無限集合** であって, `Zero` はその中の **1 要素** にすぎません. `Nat` のすべての値は `Zero` という唯一の **素材** から `Succ` で生成されますが, 「素材が 1 個であること」と「集合が 1 点であること」は別物です. 前節の数え上げの言葉で言えば $|\mathrm{Nat}|$ は無限です. **有限の規則 (0 項演算 1 つと 1 項演算 1 つ) だけで, 無限個の要素をもつ集合を記述できる** のが再帰的データ型の力です.

「集合とその上の演算の **組**」というこの見方は, 次章の主役になります ([第8章](fp8.html)では, 組にさらに **法則** を課したものとして代数構造を定義します).

### どこにも循環はない: 集合を下から立ち上げる

ところで, `Nat` の定義には気になる点があります. 定義の右辺に, **定義しようとしている `Nat` 自身** が現れています. これは循環ではないのでしょうか.

集合論的に見ると, `Nat` は

- $\text{Zero} \in \mathrm{Nat}$
- $n \in \mathrm{Nat}$ ならば $\text{Succ}\ n \in \mathrm{Nat}$

を満たす **最小の集合** です. このように「自分自身を使った規則で定まる集合」を **帰納的に定義された集合** といいます. そして, この「最小の集合」が循環なしに作れることは, 本章で揃えた道具 (空集合・和集合・内包表記・部分集合) だけで確かめられます.

上の 2 つの規則を, 「集合 $X$ を受け取り, 構成子を **1 段だけ** 適用して作れる値の集合を返す」対応 $\Phi$ (**生成作用素**) として書き直します.

$$\Phi(X) \;=\; \{\texttt{Zero}\}\ \cup\ \{\,\texttt{Succ}\ n \mid n \in X\,\}$$

右半分は, まさに本章の内包表記です. $\Phi$ を空集合から繰り返し適用すると,

$$\Phi(\emptyset) = \{\texttt{Zero}\},\qquad \Phi^2(\emptyset) = \{\texttt{Zero},\ \texttt{Succ Zero}\},\qquad \Phi^3(\emptyset) = \{\texttt{Zero},\ \texttt{Succ Zero},\ \texttt{Succ (Succ Zero)}\},\ \dots$$

と 1 段ごとに新しい値が 1 つずつ加わり, 集合が下から育っていきます.

$$\emptyset \;\subseteq\; \Phi(\emptyset) \;\subseteq\; \Phi^2(\emptyset) \;\subseteq\; \cdots, \qquad \mathrm{Nat} \;=\; \bigcup_{n \ge 0} \Phi^n(\emptyset)$$

どの値も **有限ステップ目で必ず現れます**. 定義が使っているのは「出来上がった `Nat` 全体」ではなく「**前の段までに作れた値**」だけです. 見かけは自己言及でも, 実際は完全にボトムアップの構成であり, 循環はどこにもありません.

この立ち上がりは, Haskell でそのまま実行できます. 集合をリストで表せば, $\Phi$ はリスト内包表記そのものです.

~~~ haskell
data Nat = Zero | Succ Nat deriving (Show, Eq)

-- 生成作用素 Φ: 構成子を 1 段だけ適用して作れる値の集合 (リスト表現)
step :: [Nat] -> [Nat]
step xs = Zero : [ Succ n | n <- xs ]

main :: IO ()
main = do
    print (step [])                         -- [Zero]                              = Φ(∅)
    print (step (step []))                  -- [Zero,Succ Zero]                    = Φ²(∅)
    print (step (step (step [])))           -- [Zero,Succ Zero,Succ (Succ Zero)]   = Φ³(∅)
    print (length (iterate step [] !! 10))  -- 10   (Φ^10(∅) は 10 個の値を持つ)
~~~

(`iterate f x` は `[x, f x, f (f x), ...]` という適用列を返す標準関数で, `!! 10` で 10 段目を取り出しています.)

<svg viewBox="0 0 720 150" width="100%" style="max-width: 730px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="生成作用素 Φ による下からの立ち上がりの図. 空集合から始めて Φ を 1 回適用すると Zero だけの集合, 2 回で Succ Zero が加わり, 3 回で Succ (Succ Zero) が加わる. 繰り返しの和集合が Nat になる.">
  <defs>
    <marker id="phi-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="6.5" markerHeight="6.5" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <g fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75">
    <rect x="10" y="40" width="60" height="56" rx="8"/>
    <rect x="128" y="40" width="118" height="56" rx="8"/>
    <rect x="304" y="40" width="150" height="56" rx="8"/>
    <rect x="512" y="40" width="196" height="56" rx="8"/>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="10" text-anchor="middle">
    <text x="40" y="72">∅</text>
    <text x="187" y="72">Zero</text>
    <text x="379" y="66">Zero,</text>
    <text x="379" y="80">Succ Zero</text>
    <text x="610" y="66">Zero, Succ Zero,</text>
    <text x="610" y="80">Succ (Succ Zero)</text>
  </g>
  <g fill="currentColor" font-size="10" text-anchor="middle" opacity="0.8">
    <text x="40" y="30">出発点</text>
    <text x="187" y="30">Φ(∅)</text>
    <text x="379" y="30">Φ²(∅)</text>
    <text x="610" y="30">Φ³(∅)</text>
  </g>
  <g stroke="currentColor" stroke-width="1.2" opacity="0.75">
    <line x1="74" y1="68" x2="122" y2="68" marker-end="url(#phi-arrow)"/>
    <line x1="250" y1="68" x2="298" y2="68" marker-end="url(#phi-arrow)"/>
    <line x1="458" y1="68" x2="506" y2="68" marker-end="url(#phi-arrow)"/>
  </g>
  <g fill="currentColor" font-size="9.5" text-anchor="middle" opacity="0.75">
    <text x="98" y="60">Φ</text><text x="274" y="60">Φ</text><text x="482" y="60">Φ</text>
  </g>
  <text x="360" y="130" fill="currentColor" font-size="11.5" text-anchor="middle" font-weight="600">⊆ が 1 段ずつ増えていき, ⋯ すべての和集合が Nat = ⋃ Φⁿ(∅)</text>
</svg>

::: note
発展として, 名前と但し書きを 3 つ添えます.

- $\Phi(X) = X$ を満たす最小の $X$ (それが $\bigcup_n \Phi^n(\emptyset)$ です) を $\Phi$ の **最小不動点 (least fixed point)** といいます. 「再帰的データ型 = 生成作用素の最小不動点」です. これが `data` 宣言の `=` の数学的な中身です.
- この構成が安全なのは, 自己言及が **正** の形で現れているからです ($X$ を大きくすると $\Phi(X)$ も大きくなる). 負の自己言及は本当に壊れます. 有名な **ラッセルのパラドックス** $\{x \mid x \notin x\}$ が矛盾を起こすのは, $\notin$ という負の形の自己言及のためです. 型の言葉では **関数矢印の左側** が負の位置にあたり, `data Bad = MkBad (Bad -> Bool)` のように矢印の左に自分自身を書く宣言は, 集合の方程式 $X \cong (X \to \mathrm{Bool})$ を要求する危険な形です (Haskell はこれを受理しますが, 矛盾は「停まらない計算」として現れます).
- 但し書き: 遅延評価の Haskell では, 実は `ones = 1 : ones` のような **無限の値** も書けてしまうため, 「型 = 有限ステップで作れる値の集合」は理想化した読みです. なお, この「構成子で生成される最小の集合」という構造は, [第9章](fp9.html)で関手を学んだ先の言葉では **始代数 (initial algebra)** と呼ばれます (ここでは名前だけ挙げておきます).
:::

**再帰的データ型は, 再帰関数で処理します.** [第5章](fp5.html)で学んだ関数の再帰を思い出してください. 関数の場合分けが型のコンストラクタ (`Zero` / `Succ n`) に一対一で対応し, `Succ n` の枝で自分自身を再帰呼び出しします.

~~~ haskell
data Nat = Zero | Succ Nat deriving (Show, Eq)

-- Nat を Int に変換する (構造をたどって 1 ずつ数える)
toInt :: Nat -> Int
toInt Zero     = 0
toInt (Succ n) = 1 + toInt n

-- 自然数の足し算 (m の上に Succ を n 個積み直す)
add :: Nat -> Nat -> Nat
add Zero     m = m
add (Succ n) m = Succ (add n m)

main :: IO ()
main = do
  let three = Succ (Succ (Succ Zero))
      two   = Succ (Succ Zero)
  print (toInt three)            -- 3
  print (toInt (add three two))  -- 5
~~~

`toInt` も `add` も, **基底ケース (`Zero`)** と **再帰ケース (`Succ n`)** に分けて書かれており, 関数の再帰構造が型の再帰構造をそのままなぞっています. これが再帰的データ型を扱うときの基本パターンです. 見方を変えると, `toInt` は **構成子の置き換え** です. 値の中の `Zero` を `0` に, `Succ` を `(+1)` に置き換えると結果が得られます (`add n m` は `Zero` を `m` に置き換え, `Succ` はそのまま). この「構成子を別の演算で置き換える」という読み方は, [第8章](fp8.html)の準同型 (構造を保つ写像) につながる伏線です.

実は, これまで使ってきた **リストも再帰的データ型** です. 「リストとは, 空リスト `[]` であるか, または先頭要素 `x` と残りのリスト `xs` をつないだ `x : xs` である」という再帰的な構造を持ち, 概念的には `[] | x : xs` と定義されます. 構成子の言葉で言えば, `[]` が 0 項演算, `(:)` が 2 項演算です. [第5章](fp5.html)以降で `(x : xs)` をパターンマッチして再帰処理してきたのは, まさにこの再帰型をたどる操作でした. リストや木を **任意の要素型** について扱えるようにする一般化 (型引数) と, それらに代数構造 (モノイド) を載せる話は [第9章](fp9.html) で扱います.

## type

これまで新しいデータを作成するためには `data` を利用してきました. しかし, 新しい型を作るほどではないものの, **同じ表現型 (たとえば `Double`) に文脈上の意味を持つ名前を与えて, 可読性を高めたい** 場合があります.
既存のデータ型に別名を与える最も簡単な方法が, `type` を用いて **型シノニム (type synonym)** と呼ばれる別名を導入する方法です.

例えば,

~~~ haskell
data Rectangle = Rectangle Double Double
    deriving Show

mkRectangle :: Double -> Double -> Rectangle
mkRectangle b h = Rectangle b h
~~~

としたとき, 最初の引数と2つ目の引数のどちらが底辺で, どちらが高さなのかが判別できません.

~~~ haskell
data Rectangle = Rectangle {bottom :: Double
                           ,height :: Double}
    deriving Show

mkRectangle :: Double -> Double -> Rectangle
mkRectangle b h = Rectangle {bottom = b, height = h}
~~~

のようにレコード構文を用いることでフィールド名による区別は可能ですが, 型注釈側にも意味のある名前が現れる方が, さらに可読性と解釈可能性が上がります.

`type 新しい型名 = 既存のデータ型`

の形式で記述することで, 以下のように同じ `Double` に別名を付けることができます.

~~~ haskell
type Bottom = Double
type Height = Double

data Rectangle = Rectangle {bottom :: Bottom
                           ,height :: Height}
    deriving Show

mkRectangle :: Bottom -> Height -> Rectangle
mkRectangle b h = Rectangle {bottom = b, height = h}
~~~

これは集合論的には, **既知の集合 $\mathbb{R}$ に対して別名 $\text{Bottom}, \text{Height}$ を与えたもの** に等しく, 集合としては $\text{Bottom} = \text{Height} = \mathbb{R}$ で完全に同一です. つまり `type` は **新しい集合 (型) を構成しているわけではなく, 既存の型に別名 (alias) を導入しているだけ** であり, 役割は **可読性のための注釈** に限られます.

::: warn
そのため, 型レベルで $\text{Bottom}$ と $\text{Height}$ を区別したい (例えば底辺と高さを取り違えるとコンパイルエラーになるようにしたい) 場合には, `type` では不十分です. その場合には次節で扱う `newtype` や, 専用のコンストラクタを持つ `data` を用いる必要があります.
:::


## newtype

`type` は既存の型に別名を付けるだけなので, 型検査上は元の型と完全に同じものとして扱われます. そのため, 前節の

~~~ haskell
type Bottom = Double
type Height = Double
~~~

では, `Bottom` と `Height` はどちらも `Double` の別名にすぎません. 型注釈を読む人間にとっては意味が分かりやすくなりますが, コンパイラは底辺と高さを別の型として区別しません.

型レベルで $\text{Bottom}$ と $\text{Height}$ を区別したい場合には, `newtype` を使います. `newtype` は既存の型を 1 つだけ包んで, 新しい独立した型を作るための構文です.

`newtype 新しい型名 = コンストラクタ名 既存の型`

例えば, 底辺と高さをそれぞれ独立した型として定義するには, 次のように書きます.

~~~ haskell
newtype Bottom = Bottom Double
newtype Height = Height Double
~~~

ここで左辺の `Bottom`, `Height` は **型構築子** です. 一方, 右辺の `Bottom`, `Height` は **データ構築子** です. 名前は同じでも, 型の名前と値を作る関数の名前として別の名前空間に属しています.

データ構築子としての型は, それぞれ次のようになります.

~~~ haskell
Bottom :: Double -> Bottom
Height :: Double -> Height
~~~

つまり, `Bottom 3.0` は `Double` の `3.0` を `Bottom` 型として包んだ値であり, `Height 3.0` とは型が異なります.

この性質を使うと, 長方形の底辺と高さの取り違えを型検査で防げます.

~~~ haskell
newtype Bottom = Bottom Double deriving Show
newtype Height = Height Double deriving Show

data Rectangle = Rectangle Bottom Height
    deriving Show

mkRectangle :: Bottom -> Height -> Rectangle
mkRectangle b h = Rectangle b h

main :: IO ()
main = print (mkRectangle (Bottom 3.0) (Height 4.0))
~~~

このとき, `mkRectangle` は第一引数に `Bottom`, 第二引数に `Height` を要求します. そのため, 次のように底辺と高さを逆に渡そうとすると, コンパイルエラーになります.

~~~ haskell
-- mkRectangle (Height 4.0) (Bottom 3.0)
~~~

`type Bottom = Double` では `Bottom` も `Height` も実体は同じ `Double` なので, このような取り違えをコンパイラは検出できません. 一方, `newtype` では `Bottom` と `Height` は別々の型なので, 取り違えは型エラーになります.

`newtype` で包んだ値を計算に使うには, パターンマッチで中身を取り出します. 例えば, 長方形の面積を求める関数は次のように書けます.

~~~ haskell
newtype Bottom = Bottom Double deriving Show
newtype Height = Height Double deriving Show

data Rectangle = Rectangle Bottom Height
    deriving Show

area :: Rectangle -> Double
area (Rectangle (Bottom b) (Height h)) = b * h
~~~

`Rectangle (Bottom b) (Height h)` というパターンにより, `Bottom` の中の `Double` を `b` に, `Height` の中の `Double` を `h` に取り出しています. `newtype` で作った値は元の `Double` と同じように自動で計算に使えるわけではありません. **別の型として包んだので, 必要な場所で明示的に取り出す** 必要があります.

集合論的に見ると, `type` は既存の集合に別名を付けているだけです. したがって,

$$\text{Bottom} = \text{Height} = \mathbb{R}$$

とみなせます. 一方, `newtype` は既存の集合と同じ要素数を持つ **タグ付きのコピー** を作っていると考えられます. `Bottom` も `Height` も中身は `Double` ですが, Haskell の型検査上は,

$$\text{Bottom} \neq \text{Height}$$

として扱われます. つまり, `Double` と `Bottom` は互いに変換可能なほどよく似ていますが, プログラム上では明示的に包む / 取り出す操作を通じて区別されます.

::: warn
`newtype` は, **コンストラクタが 1 つで, そのコンストラクタが包むフィールドも 1 つ** の場合にだけ使えます. 複数のフィールドを持つ直積型や, 複数のコンストラクタを持つ直和型を作りたい場合は, これまで通り `data` を使います.
:::

::: warn
`newtype` は新しい型を作りますが, 値が条件を満たすことまでは自動では保証しません. 例えば,

~~~ haskell
newtype Positive = Positive Int

badPositive :: Positive
badPositive = Positive (-1)
~~~

と定義しても, `Positive (-1)` は作れてしまいます. 「正の整数だけを作れるようにしたい」場合には, 次節のスマートコンストラクタのように, コンストラクタをモジュール外へ公開せず, 検査付きの関数を経由して値を作る設計が必要です.
:::

## スマートコンストラクタ (発展)

内包表記の節で予告した, **型のレベル** で「他のデータ型から述語で絞り込んだ新しいデータ型」を作る方法が, **スマートコンストラクタ** というイディオムです. 代数的データ型の `data` / `newtype` 宣言そのものには述語で絞り込む機能はないため, 前節の `newtype` とモジュールシステムを組み合わせて事実上の絞り込みを実現します. `module` など今後の学習内容を先取りしたかなり発展的な内容になるので, 対象は興味のある人のみとします.

たとえば, `Int` のうち「3の倍数」のみを要素とするデータ型 `Mult3`

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

ポイントは, `module` 宣言の出力リストで **コンストラクタ `Mult3` をエクスポートしていない** ことです. これにより, `Mult3` 型の値はモジュール外からは必ず `mkMult3` を経由してしか作れず, かつ `mkMult3` は 3 の倍数のときしか値を返しません. したがって `Mult3` 型の要素は必然的に $\{n \in \mathrm{Int} \mid n \bmod 3 = 0\}$ という述語付きの集合に対応する, という保証が値の生成経路のレベルで得られます.

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
スマートコンストラクタは, 代数的データ型そのものの機能ではなく, **代数的データ型とモジュールシステムの組み合わせ** によって間接的に内包表記的な絞り込みを実現する実用的なイディオムです. 型システムそのものにもっと踏み込んだ絞り込みを型レベルで与えたい場合は, GADTs や Refinement Types (Liquid Haskell) などの拡張機能がありますが, 本講義の範囲では扱いません.
:::

<!-- ==== 関係部 ==== -->

## 関係: 直積の部分集合

ここまでは, 集合 (型) を **作る** 道具 (列挙, 述語による部分集合, 直積, 直和, 再帰) を見てきました. 本章の残りでは, 集合の **間** のつながりを扱います.

「3 は 12 を割り切る」「$x$ は $y$ 以下である」「文字列 $s$ と $t$ は同じ長さである」. これらはどれも, **2 つのものの間で** 真偽が定まる主張です. 述語と量化の節で見た述語 $p(x)$ は変数が 1 つでしたが, これらは変数を 2 つ持つ述語 $p(x, y)$ とみなせます. こうした「2 つのものの間の結びつき」を数学では **関係 (relation)** と呼び, 集合論では次のように定義します.

::: note
集合 $A$ と $B$ の間の (2 項) **関係 (relation)** $R$ とは, 直積 $A \times B$ の **部分集合** のことである: $R \subseteq A \times B$.
:::

つまり関係とは, 「結びついている対 $(a, b)$ をすべて集めた集合」です. $(a, b) \in R$ のとき, $a$ と $b$ は関係 $R$ にあるといい, **中置記法** で $a \mathrel{R} b$ とも書きます (たとえば $3 \leq 5$ は, 対 $(3, 5)$ が関係 $\leq$ に属することの中置記法です).

部分集合は述語で切り出せるのでした (内包表記). 関係も同じで, 2 変数の述語 $p(a, b)$ から

$$R = \{(a, b) \mid (a, b) \in A \times B,\ p(a, b)\}$$

と内包表記で定義できます. 本章前半の道具が, そのまま使えるわけです.

例として, 正の整数の間の **割り切る関係** を考えます. 「$a$ が $b$ を割り切る」($a \mid b$ と書きます) とは, $b$ を $a$ で割った余りが $0$ であることです.

$$D = \{(a, b) \mid (a, b) \in \mathbb{Z}^+ \times \mathbb{Z}^+,\ b \bmod a = 0\}$$

$(3, 12) \in D$ (3 は 12 を割り切る) ですが, $(5, 12) \notin D$ です.

Haskell では, 有限の範囲であればこの関係をリスト内包表記でそのまま計算できます. また, 「対 $(a, b)$ が関係にあるか」の判定は **`Bool` を返す 2 引数関数** として書けます.

~~~ haskell
-- 判定関数: a は b を割り切るか
divides :: Int -> Int -> Bool
divides a b = b `mod` a == 0

-- 関係 D の {1..n} の範囲での全体像 (内包表記そのまま)
dividesPairs :: Int -> [(Int, Int)]
dividesPairs n = [ (a, b) | a <- [1..n], b <- [1..n], a `divides` b ]

main :: IO ()
main = do
    print $ 3 `divides` 12   -- True
    print $ 5 `divides` 12   -- False
    print $ dividesPairs 4   -- [(1,1),(1,2),(1,3),(1,4),(2,2),(2,4),(3,3),(4,4)]
~~~

ここで ``a `divides` b`` のようにバッククォートで囲むと 2 引数関数を中置で書けるのでした ([第5章](fp5.html)). 数学の中置記法 $a \mid b$ と Haskell の ``a `divides` b`` がきれいに対応します.

<svg viewBox="0 0 460 290" width="100%" style="max-width: 460px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="関係を直積の部分集合として表した図. 4 かける 4 の直積 A×A の格子のうち, a が b を割り切る対 (1,1) から (4,4) までの 8 マスだけが塗られている. 塗られたマスの集合が関係 D = dividesPairs 4 である.">
  <g>
    <text x="150" y="30" fill="currentColor" font-size="11" text-anchor="middle">b=1</text>
    <text x="196" y="30" fill="currentColor" font-size="11" text-anchor="middle">b=2</text>
    <text x="242" y="30" fill="currentColor" font-size="11" text-anchor="middle">b=3</text>
    <text x="288" y="30" fill="currentColor" font-size="11" text-anchor="middle">b=4</text>
    <text x="112" y="64" fill="currentColor" font-size="11" text-anchor="middle">a=1</text>
    <text x="112" y="110" fill="currentColor" font-size="11" text-anchor="middle">a=2</text>
    <text x="112" y="156" fill="currentColor" font-size="11" text-anchor="middle">a=3</text>
    <text x="112" y="202" fill="currentColor" font-size="11" text-anchor="middle">a=4</text>
  </g>
  <g>
    <circle cx="150" cy="60" r="13" fill="rgba(13,148,136,0.30)" stroke="currentColor" stroke-width="1.3"/>
    <text x="150" y="63.5" fill="currentColor" font-size="8.5" text-anchor="middle">(1,1)</text>
    <circle cx="196" cy="60" r="13" fill="rgba(13,148,136,0.30)" stroke="currentColor" stroke-width="1.3"/>
    <text x="196" y="63.5" fill="currentColor" font-size="8.5" text-anchor="middle">(1,2)</text>
    <circle cx="242" cy="60" r="13" fill="rgba(13,148,136,0.30)" stroke="currentColor" stroke-width="1.3"/>
    <text x="242" y="63.5" fill="currentColor" font-size="8.5" text-anchor="middle">(1,3)</text>
    <circle cx="288" cy="60" r="13" fill="rgba(13,148,136,0.30)" stroke="currentColor" stroke-width="1.3"/>
    <text x="288" y="63.5" fill="currentColor" font-size="8.5" text-anchor="middle">(1,4)</text>
    <circle cx="150" cy="106" r="13" fill="none" stroke="currentColor" stroke-width="0.8" opacity="0.3"/>
    <text x="150" y="109.5" fill="currentColor" font-size="8.5" text-anchor="middle" opacity="0.35">(2,1)</text>
    <circle cx="196" cy="106" r="13" fill="rgba(13,148,136,0.30)" stroke="currentColor" stroke-width="1.3"/>
    <text x="196" y="109.5" fill="currentColor" font-size="8.5" text-anchor="middle">(2,2)</text>
    <circle cx="242" cy="106" r="13" fill="none" stroke="currentColor" stroke-width="0.8" opacity="0.3"/>
    <text x="242" y="109.5" fill="currentColor" font-size="8.5" text-anchor="middle" opacity="0.35">(2,3)</text>
    <circle cx="288" cy="106" r="13" fill="rgba(13,148,136,0.30)" stroke="currentColor" stroke-width="1.3"/>
    <text x="288" y="109.5" fill="currentColor" font-size="8.5" text-anchor="middle">(2,4)</text>
    <circle cx="150" cy="152" r="13" fill="none" stroke="currentColor" stroke-width="0.8" opacity="0.3"/>
    <text x="150" y="155.5" fill="currentColor" font-size="8.5" text-anchor="middle" opacity="0.35">(3,1)</text>
    <circle cx="196" cy="152" r="13" fill="none" stroke="currentColor" stroke-width="0.8" opacity="0.3"/>
    <text x="196" y="155.5" fill="currentColor" font-size="8.5" text-anchor="middle" opacity="0.35">(3,2)</text>
    <circle cx="242" cy="152" r="13" fill="rgba(13,148,136,0.30)" stroke="currentColor" stroke-width="1.3"/>
    <text x="242" y="155.5" fill="currentColor" font-size="8.5" text-anchor="middle">(3,3)</text>
    <circle cx="288" cy="152" r="13" fill="none" stroke="currentColor" stroke-width="0.8" opacity="0.3"/>
    <text x="288" y="155.5" fill="currentColor" font-size="8.5" text-anchor="middle" opacity="0.35">(3,4)</text>
    <circle cx="150" cy="198" r="13" fill="none" stroke="currentColor" stroke-width="0.8" opacity="0.3"/>
    <text x="150" y="201.5" fill="currentColor" font-size="8.5" text-anchor="middle" opacity="0.35">(4,1)</text>
    <circle cx="196" cy="198" r="13" fill="none" stroke="currentColor" stroke-width="0.8" opacity="0.3"/>
    <text x="196" y="201.5" fill="currentColor" font-size="8.5" text-anchor="middle" opacity="0.35">(4,2)</text>
    <circle cx="242" cy="198" r="13" fill="none" stroke="currentColor" stroke-width="0.8" opacity="0.3"/>
    <text x="242" y="201.5" fill="currentColor" font-size="8.5" text-anchor="middle" opacity="0.35">(4,3)</text>
    <circle cx="288" cy="198" r="13" fill="rgba(13,148,136,0.30)" stroke="currentColor" stroke-width="1.3"/>
    <text x="288" y="201.5" fill="currentColor" font-size="8.5" text-anchor="middle">(4,4)</text>
  </g>
  <text x="380" y="202" fill="currentColor" font-size="11" text-anchor="middle">⊆ A × A</text>
  <g fill="currentColor" text-anchor="middle">
    <text x="230" y="262" font-size="12" font-weight="600">関係 D = 塗られた対の集合 (直積の部分集合)</text>
    <text x="230" y="280" font-size="10.5" opacity="0.75">塗り = a | b (dividesPairs 4 の 8 対と一致)</text>
  </g>
</svg>

対応関係を整理します. 集合としての関係 $R \subseteq A \times B$ と, 判定関数 `r :: a -> b -> Bool` は,

$$(a, b) \in R \quad\Longleftrightarrow\quad \texttt{r a b} = \texttt{True}$$

によって行き来できます. 実は, これまで使ってきた `==`, `<=`, `<` などの比較演算子は, すべてこの意味での「関係の判定関数」です. 関係を `Bool` への **演算** として本格的に扱い直すのは [第8章](fp8.html)の仕事です.

なお, 変数を 3 つ以上持つ関係も同様に定義できます ($n$ 項関係 = $A_1 \times \dots \times A_n$ の部分集合) が, 本章では最もよく使う 2 項関係のみを扱います.

::: note

### Exercise CH7-5

**互いに素: 関係を内包表記で計算する**

正の整数 $a, b$ が **互いに素 (coprime)** であるとは, $a$ と $b$ の最大公約数が $1$ であることをいいます (最大公約数は標準関数 `gcd` で計算できます).

1. 判定関数 `coprime :: Int -> Int -> Bool` を実装してください.

2. 関係 $C_n = \{(a, b) \mid (a, b) \in \{1, \dots, n\} \times \{1, \dots, n\},\ \gcd(a, b) = 1\}$ を計算する関数 `coprimePairs :: Int -> [(Int, Int)]` をリスト内包表記で実装してください.

3. `coprime` が **対称** であること ($\forall a, b.\ \mathrm{coprime}(a, b) \implies \mathrm{coprime}(b, a)$) を, $1$ から $20$ の範囲の全数検査で確認する式を書いてください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
    print $ coprime 8 15     -- True
    print $ coprime 6 9      -- False (gcd 6 9 == 3)
    print $ coprimePairs 4   -- [(1,1),(1,2),(1,3),(1,4),(2,1),(2,3),(3,1),(3,2),(3,4),(4,1),(4,3)]
    print $ and [ coprime b a | a <- [1..20], b <- [1..20], coprime a b ]  -- True (対称)
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
coprime :: Int -> Int -> Bool
coprime a b = gcd a b == 1

coprimePairs :: Int -> [(Int, Int)]
coprimePairs n = [ (a, b) | a <- [1..n], b <- [1..n], coprime a b ]

main :: IO ()
main = do
    print $ coprime 8 15
    print $ coprime 6 9
    print $ coprimePairs 4
    print $ and [ coprime b a | a <- [1..20], b <- [1..20], coprime a b ]
~~~

対称性の検査では, ガード `coprime a b` で「関係にある対」だけを走査し, 本体 `coprime b a` で「逆向きも関係にあるか」を確かめています. つまり `and [ 結論 | 変数の走査, 仮定 ]` という形で, $\forall a, b.\ (\text{仮定} \implies \text{結論})$ の有限版が書けます.

ちなみに `coprime` は **反射的ではありません**: `coprime 1 1` は `True` ですが, `coprime 2 2` は `False` です ($\gcd(2,2) = 2$). 「すべての $x$ で $x \mathrel{R} x$」が成り立つ関係 (反射律) については, 同値関係の節で扱います.

</details>

提出ファイル名: `ch7-5.hs`

:::

## 関数: 特別な関係

関係を定義すると, 実は **関数も関係の一種** として捉え直せます.

関数 $f : A \to B$ が与えられたとき, 「入力と出力の対」をすべて集めた集合

$$G_f = \{(x, f(x)) \mid x \in A\} \subseteq A \times B$$

を $f$ の **グラフ (graph)** といいます. グラフは直積の部分集合なので, まさに関係です. 中学・高校で $y = x^2$ の「グラフ」を座標平面 (= $\mathbb{R} \times \mathbb{R}$ の直積) に描いてきたのは, 文字通りこの意味のグラフ, すなわち関係 $\{(x, y) \mid y = x^2\}$ の図示でした.

Haskell でも, 有限の範囲なら関数のグラフをリスト内包表記で作れます.

~~~ haskell
square :: Int -> Int
square x = x * x

-- xs の範囲での f のグラフ {(x, f x) | x ∈ xs}
graphOn :: [a] -> (a -> b) -> [(a, b)]
graphOn xs f = [ (x, f x) | x <- xs ]

main :: IO ()
main = print $ graphOn [1..5] square   -- [(1,1),(2,4),(3,9),(4,16),(5,25)]
~~~

では逆に, どんな関係 $G \subseteq A \times B$ も何かの関数のグラフになっているでしょうか. そうではありません. 関係が **関数のグラフ** であるためには, 次の 2 条件が必要です.

- **全域性 (totality)**: どの入力にも出力がある.
  $$\forall x \in A.\ \exists y \in B.\ (x, y) \in G$$
- **右一意性 (right-uniqueness)**: 1 つの入力に対する出力は 1 通りしかない.
  $$\forall x \in A.\ \forall y, y' \in B.\ \big((x, y) \in G \land (x, y') \in G\big) \implies y = y'$$

この 2 条件を満たす関係は, ちょうど 1 つの関数 $f : A \to B$ を定めます. すなわち, **関数とは全域かつ右一意な関係** のことです. ここでも $\forall$ と $\exists$ が定義の骨格を担っていることに注目してください.

この見方の効用の 1 つは, 「関数になりそこねた関係」, すなわち **部分関数 (partial function)** を正確に語れることです. 部分関数とは, 右一意だが **全域性を欠く** 関係, つまり「出力が定義されない入力がある」対応のことです. 実は, 既にいくつも登場しています.

- `head :: [a] -> a`: 空リスト `[]` に対する出力がありません ([第5章](fp5.html)で見た実行時エラーの代表例).
- 直和型のレコードアクセサ: 「レコード構文」節の警告で見た `dogAge` は, `WithAge` の値にしか出力を持たず, `JustBreed` の値に適用すると `No match in record selector` になります.

Haskell の型 `A -> B` は, この全域性までは保証してくれません. `head` のような部分関数も型検査は通り, 定義されていない入力に出会ったときに **実行時エラー** として破綻します. 対処の方向は 2 つあります. (1) **定義域を型で絞る**: 「空でないリスト」のような型を作り, 危険な入力をそもそも渡せなくします (スマートコンストラクタはその道具の 1 つでした). (2) **出力を広げる**: 「出力が無いかもしれない」ことを型で表す `Maybe` を使います (詳しくは [第9章](fp9.html)で扱います).

なお, [第5章](fp5.html)で学んだパターンマッチによる関数定義は, この見方では「グラフのどの部分に対 $(x, f(x))$ を置くか」を場合分けで指定する記法だと読めます. すべての入力パターンを尽くせば全域な関数になり, 尽くさなければ (`head` のように) 部分関数になります.

## 演算: 台の上で閉じた関数

関数を正式に定義できたので, 本章でここまで非公式に使ってきた **演算 (operation)** という言葉も, ここで整理しておきます. 再帰的データ型の節で構成子を「0 項演算」「1 項演算」と呼びました. ここでその一般定義を与えます.

::: note
集合 $A$ 上の **$n$ 項演算 ($n$-ary operation)** とは,

$$f : \underbrace{A \times \cdots \times A}_{n \text{ 個}} \to A$$

という形の **関数** のことをいう. $n$ (引数の個数) を演算の **アリティ (arity)** という.
:::

ポイントは 2 つです.

- **演算は関数の特別な場合** であって, 別種の概念ではありません. 出発点が台 $A$ の直積で, **行き先も同じ $A$** (つまり **台の中で閉じている**) という **形の制約** が付いた関数の呼び名です.
- 演算は **アリティで種類分け** されます. $n = 2$ が最も身近ですが, $n = 1$, $n = 0$ も演算です.

| アリティ | 呼び名 | 形 | 例 |
| --- | --- | --- | --- |
| 2 | 2 項演算 | $A \times A \to A$ | `(+) :: Int -> Int -> Int`, `(++)` |
| 1 | 1 項演算 | $A \to A$ | `negate :: Int -> Int`, `Succ :: Nat -> Nat` |
| 0 | 0 項演算 = **定数** | $A^0 \to A$ | `0 :: Int`, `Zero :: Nat`, `[] :: [a]` |

**0 項演算が定数である理由.** $A^0$ (0 個の直積) は空集合ではなく, **空タプル `()` だけを要素とする 1 点集合** です (直和型の数え上げで見た $\lvert () \rvert = 1$ の集合がこれです). したがって $A^0 \to A$ という関数は, 「唯一の入力 `()` に対して $A$ の要素を 1 つ返す」, すなわち **$A$ の要素を 1 つ指定すること** と同じです. 「引数が無い」のではなく「情報を持たない引数が 1 つある」と読むのが正確で, これが「0 項演算 = 定数」の中身です.

**演算でない関数.** 行き先が台の **外** に出る関数は, その台の上の演算ではありません.

~~~ haskell
-- 2 項演算: 台 Int の中で閉じている
plus :: Int -> Int -> Int
plus x y = x + y

-- 1 項演算
neg :: Int -> Int
neg x = negate x

-- 0 項演算 = 定数
zero :: Int
zero = 0

-- 演算ではない関数: 台 [Int] から外 (Int) へ出ていく
len :: [Int] -> Int
len xs = length xs

main :: IO ()
main = do
    print (plus 3 4)     -- 7
    print (neg 5)        -- -5
    print zero           -- 0
    print (len [1,2,3])  -- 3
~~~

`len` (= `length`) はリストの世界から **出ていく** ので, リストの上の演算ではありません. こうした「世界から世界へ渡る関数」は, [第8章](fp8.html)で **準同型 (構造を保つ写像)** として主役になります. また, 関係の判定関数 `divides :: Int -> Int -> Bool` も行き先が `Bool` なので `Int` の上の演算ではありません (ただし `Bool` をもう 1 つの台と認めて「`Bool` への 2 項演算」と読む立場もあり, [第8章](fp8.html)でこの読み替えを使います).

この対比を, 3 枚つづきの図にしておきます. いちばん左の **図0** は, まだ何も載っていない **集合だけの世界** です. 点 (型 = 集合) が 1 つあるだけで, 射はありません. そこに演算を載せたのが **図1**, すなわち **演算の世界** です. 点はやはり 1 つ (台) のままで, 射 (矢印) はすべて台自身へ戻ってきます. 図の下には, この世界を組として書き出した成分の内訳を添えています. 台 $= \mathrm{Type}$, 演算 $= \bullet$ (2 項), $e$ (0 項) です. いちばん右の **図2** が **関数の世界** です. 点 (型) は複数あり, 射は型から型へ自由に渡れます. こちらの内訳は $\mathrm{Ob} = \{A, B, C\}$, $\mathrm{Mor} = \{f, g, g \circ f, \mathrm{id}, \dots\}$ であり, **点の集合と射の集合** の 2 本立てで, 成分の座席に「射の集まり」が入っています (この $\mathrm{Ob}$ / $\mathrm{Mor}$ という成分名は, [第9章](fp9.html)の圏の定義でそのまま登場します). 演算とは, 図2 の射のうち「出発点と行き先が同じ点になるよう制約されたもの」だけを図1 の世界として取り出したものです (図1 のループはアリティを省いて描いています. $\bullet$ の域は $\mathrm{Type} \times \mathrm{Type}$, $e$ の域は $\mathrm{Type}^0$ ですが, 行き先が台に戻る点は同じです).

<svg viewBox="0 0 772 246" width="100%" style="max-width: 780px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="集合・演算・関数の 3 枚つづきの図. 図0: 四角の中に点 Type だけがある集合のみの世界 (台 = Type, 演算はまだ無い). 演算を載せると図1: 代数. 台 = Type, 演算 = ● と e がループの射として描かれ, 射が台の外へ出ない. 俯瞰 (台を射の集まりに) すると図2: 圏. Ob = {A, B, C}, Mor = {f, g, g∘f, id, …} をもつ, 複数の型の間を射が渡る関数の世界. 演算は行き先が同じ台に制約された特別な関数である.">
  <defs>
    <marker id="op-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="6.5" markerHeight="6.5" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <!-- 図0: 集合だけの世界 -->
  <rect x="12" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="26" y="46" fill="currentColor" font-size="10" opacity="0.8">図0  集合</text>
  <text x="110" y="106" fill="currentColor" font-family="monospace" font-size="12.5" font-weight="600" text-anchor="middle">Type</text>
  <g fill="currentColor" text-anchor="middle">
    <text x="110" y="193" font-size="12" font-weight="600">点 = 集合 (型)</text>
    <text x="110" y="208" font-size="9.5" opacity="0.85">台 = Type</text>
    <text x="110" y="222" font-size="9.5" opacity="0.85">演算 = (まだ無い)</text>
    <text x="110" y="237" font-size="10" opacity="0.7">まだ何も載っていない</text>
  </g>
  <line x1="214" y1="100" x2="282" y2="100" stroke="currentColor" stroke-width="1.2" marker-end="url(#op-arrow)" opacity="0.75"/>
  <g fill="currentColor" text-anchor="middle">
    <text x="248" y="78" font-size="10" font-weight="600">演算を</text>
    <text x="248" y="92" font-size="8.5" opacity="0.8">載せる</text>
  </g>
  <!-- 図1: 代数 -->
  <rect x="288" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="302" y="46" fill="currentColor" font-size="10" opacity="0.8">図1  代数</text>
  <text x="386" y="106" fill="currentColor" font-family="monospace" font-size="12" font-weight="600" text-anchor="middle">Type</text>
  <path d="M 400 94 C 452 52, 320 52, 372 94" fill="none" stroke="currentColor" stroke-width="1.3" marker-end="url(#op-arrow)"/>
  <text x="386" y="79" fill="currentColor"  font-size="11" text-anchor="middle">●</text>
  <path d="M 400 116 C 452 158, 320 158, 372 116" fill="none" stroke="currentColor" stroke-width="1.3" marker-end="url(#op-arrow)"/>
  <text x="386" y="142" fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="11" text-anchor="middle">e</text>
  <g fill="currentColor" text-anchor="middle">
    <text x="386" y="193" font-size="12" font-weight="600">点 = 台 (集合), 射 = 演算</text>
    <text x="386" y="208" font-size="9.5" opacity="0.85">台 = Type</text>
    <text x="386" y="222" font-size="9.5" opacity="0.85">演算 = ●, e</text>
    <text x="386" y="237" font-size="10" opacity="0.7">射が台の外へ出ない (閉じている)</text>
  </g>
  <line x1="490" y1="100" x2="558" y2="100" stroke="currentColor" stroke-width="1.2" marker-end="url(#op-arrow)" opacity="0.75"/>
  <g fill="currentColor" text-anchor="middle">
    <text x="524" y="78" font-size="10" font-weight="600">俯瞰</text>
    <text x="524" y="92" font-size="8.5" opacity="0.8">台を射の集まりに</text>
  </g>
  <!-- 図2: 関数の世界 -->
  <rect x="564" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="578" y="46" fill="currentColor" font-size="10" opacity="0.8">図2  圏 Hask</text>
  <g fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="13" text-anchor="middle">
    <text x="604" y="71">A</text>
    <text x="714" y="71">B</text>
    <text x="660" y="151">C</text>
  </g>
  <g stroke="currentColor" stroke-width="1.3" fill="none">
    <line x1="620" y1="67" x2="698" y2="67" marker-end="url(#op-arrow)"/>
    <line x1="708" y1="80" x2="672" y2="136" marker-end="url(#op-arrow)"/>
    <line x1="608" y1="80" x2="646" y2="136" marker-end="url(#op-arrow)"/>
  </g>
  <g fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="11" text-anchor="middle">
    <text x="659" y="59">f</text>
    <text x="704" y="112">g</text>
    <text x="612" y="112">g∘f</text>
  </g>
  <g fill="currentColor" text-anchor="middle">
    <text x="662" y="193" font-size="12" font-weight="600">点 = 型 (集合), 射 = 関数</text>
    <text x="662" y="208" font-size="9.5" opacity="0.85">Ob = {A, B, C}</text>
    <text x="662" y="222" font-size="9.5" opacity="0.85">Mor = {f, g, g∘f, id, …}</text>
    <text x="662" y="237" font-size="10" opacity="0.7">射は型から型へ渡れる</text>
  </g>
</svg>

図1 から図2 へ移る操作 (図のいちばん右の矢印) を **俯瞰** と呼ぶことにします. 台を 1 つの集合に固定して眺めるのをやめ, **射 (関数) の集まりそのもの** を視野に入れる, 見方の転換です. この図の系列はこのあとの章でそのまま再登場します. **図1 の世界に法則を課したものが代数** ([第8章](fp8.html)) であり, **俯瞰した図2 の世界 (射の集まり) を新しい台とみなしたものが圏** ([第9章](fp9.html)) です.

**カリー化との関係.** 数学では 2 項演算を $A \times A \to A$ (引数はペア 1 つ) と書きますが, Haskell では `a -> a -> a` (カリー化) が普通です. [第5章](fp5.html)のカリー化で見たとおり, 両者は同じ情報の別表現です.

::: note
再帰的データ型の節では, `MkDogAge :: MyDogs -> Int -> DogAge` のような構成子も「2 項演算」と呼びました. 厳密には出発点の型が行き先と異なるため, これは「台が複数ある (**多ソート**) 世界での演算」です. `Zero` / `Succ` のような再帰型の構成子は, ちょうど単一の台で閉じた演算になっています. Haskell はもともと型 (台) がたくさんある世界なので, 本講義では多ソートの意味でも「演算」と呼びます.
:::

[第8章](fp8.html)では, この演算を台に載せ, $\forall$ / $\exists$ で書いた法則を課したもの, すなわち **構造** を扱います. 本章の同値関係・順序関係 (関係) と, この節の演算が, そこで「組 + 法則」という 1 つの形に合流します.

## 同値関係: 「同じ」の一般化

ここからは, 同じ集合 $A$ の上の関係 $R \subseteq A \times A$ に注目し, 特に重要な 2 種類 (**同値関係** と **順序関係**) を見ます. どちらも「関係が満たすべき法則」を $\forall$ で書くことで定義されます. この「対象に法則を課す」という発想は次章の中心テーマになるので, ここで感触を掴んでおきましょう.

まず「等しい」の一般化から始めます. 集合 $A$ 上の関係 $R$ が次の 3 つの法則を満たすとき, $R$ を **同値関係 (equivalence relation)** といいます.

- **反射律 (reflexivity)**: $\forall x \in A.\ x \mathrel{R} x$ (どの要素も自分自身と関係にある).
- **対称律 (symmetry)**: $\forall x, y \in A.\ x \mathrel{R} y \implies y \mathrel{R} x$ (向きを入れ替えても成り立つ).
- **推移律 (transitivity)**: $\forall x, y, z \in A.\ (x \mathrel{R} y \land y \mathrel{R} z) \implies x \mathrel{R} z$ (関係が連鎖する).

もっとも基本的な例は「等しい」($=$) そのものです: $x = x$ (反射), $x = y \implies y = x$ (対称), $x = y \land y = z \implies x = z$ (推移).

面白いのは, 「完全に等しい」わけではないのに同値関係になる例です. 整数の間の, **7 で割った余りが等しい** という関係を考えます.

$$x \equiv y \pmod 7 \quad\Longleftrightarrow\quad x \bmod 7 = y \bmod 7$$

たとえば $0 \equiv 7 \equiv 14 \pmod 7$ です. 「0 日目と 7 日目と 14 日目は同じ曜日」と読めば, これは「**同じ曜日である**」という関係にほかなりません. 日付として等しくなくても, 曜日という観点では「同じ」です. 同値関係とは, このように **何かの観点で「同じ」とみなす** ことの数学的な定式化です.

3 つの法則が本当に成り立つか, 有限範囲の全数検査で確かめてみましょう (`forallOn` は内包表記の節で定義したものです). 比較のために, 同値関係に **ならない** 例 (「差が 1 以下」という関係) も検査します.

~~~ haskell
-- ∀x ∈ xs. p x の有限版 (内包表記の節より)
forallOn :: [a] -> (a -> Bool) -> Bool
forallOn xs p = and [ p x | x <- xs ]

-- 7 で割った余りが等しい (「同じ曜日」)
congruent7 :: Int -> Int -> Bool
congruent7 x y = x `mod` 7 == y `mod` 7

-- 差が 1 以下 (「近い」)
nearBy :: Int -> Int -> Bool
nearBy x y = abs (x - y) <= 1

main :: IO ()
main = do
    -- congruent7 は 0..30 の範囲で 3 法則をすべて満たす
    print $ forallOn [0..30] (\x -> congruent7 x x)                 -- True (反射律)
    print $ and [ congruent7 y x | x <- [0..30], y <- [0..30]
                , congruent7 x y ]                                  -- True (対称律)
    print $ and [ congruent7 x z | x <- [0..30], y <- [0..30], z <- [0..30]
                , congruent7 x y, congruent7 y z ]                  -- True (推移律)
    -- nearBy は反射・対称だが, 推移律が破れる
    print $ nearBy 1 2 && nearBy 2 3   -- True  (1 と 2, 2 と 3 は近い)
    print $ nearBy 1 3                 -- False (しかし 1 と 3 は近くない)
~~~

対称律・推移律の検査に使った `and [ 結論 | 変数の走査, 仮定 ]` という形に注目してください. ガードを **仮定**, 本体を **結論** とすることで, $\forall x, y.\ (\text{仮定} \implies \text{結論})$ という形の法則が, そのまま全数検査になります.

`nearBy` の例は, 「なんとなく等しさに似ている」だけでは同値関係にならないことを示しています. 「近い」は連鎖させると少しずつずれていくため, 推移律が破れるのです. 法則を $\forall$ で明示して検査できる形にしておくことで, こうした直感の穴を機械的に発見できます.

<svg viewBox="0 0 460 250" width="100%" style="max-width: 470px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="同値関係 congruent7 による類別の図. 整数 0 から 20 が, 7 で割った余りごとに 7 つの互いに素なブロック [0] から [6] に分かれている. 各ブロックが同値類で, 台全体が過不足なく分割される.">
  <rect x="26" y="30" width="408" height="162" rx="12" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.6"/>
  <text x="36" y="22" fill="currentColor" font-size="10" opacity="0.7">台 = 整数 (0..20 を図示)</text>
  <rect x="40" y="60" width="45" height="120" rx="8" fill="rgba(13,148,136,0.12)" stroke="currentColor" stroke-width="1"/>
  <text x="62.5" y="50" fill="currentColor" font-size="11" text-anchor="middle">[0]</text>
  <text x="62.5" y="88" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">0</text>
  <text x="62.5" y="120" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">7</text>
  <text x="62.5" y="152" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">14</text>
  <rect x="95" y="60" width="45" height="120" rx="8" fill="rgba(13,148,136,0.12)" stroke="currentColor" stroke-width="1"/>
  <text x="117.5" y="50" fill="currentColor" font-size="11" text-anchor="middle">[1]</text>
  <text x="117.5" y="88" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">1</text>
  <text x="117.5" y="120" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">8</text>
  <text x="117.5" y="152" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">15</text>
  <rect x="150" y="60" width="45" height="120" rx="8" fill="rgba(13,148,136,0.12)" stroke="currentColor" stroke-width="1"/>
  <text x="172.5" y="50" fill="currentColor" font-size="11" text-anchor="middle">[2]</text>
  <text x="172.5" y="88" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">2</text>
  <text x="172.5" y="120" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">9</text>
  <text x="172.5" y="152" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">16</text>
  <rect x="205" y="60" width="45" height="120" rx="8" fill="rgba(13,148,136,0.12)" stroke="currentColor" stroke-width="1"/>
  <text x="227.5" y="50" fill="currentColor" font-size="11" text-anchor="middle">[3]</text>
  <text x="227.5" y="88" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">3</text>
  <text x="227.5" y="120" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">10</text>
  <text x="227.5" y="152" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">17</text>
  <rect x="260" y="60" width="45" height="120" rx="8" fill="rgba(13,148,136,0.12)" stroke="currentColor" stroke-width="1"/>
  <text x="282.5" y="50" fill="currentColor" font-size="11" text-anchor="middle">[4]</text>
  <text x="282.5" y="88" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">4</text>
  <text x="282.5" y="120" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">11</text>
  <text x="282.5" y="152" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">18</text>
  <rect x="315" y="60" width="45" height="120" rx="8" fill="rgba(13,148,136,0.12)" stroke="currentColor" stroke-width="1"/>
  <text x="337.5" y="50" fill="currentColor" font-size="11" text-anchor="middle">[5]</text>
  <text x="337.5" y="88" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">5</text>
  <text x="337.5" y="120" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">12</text>
  <text x="337.5" y="152" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">19</text>
  <rect x="370" y="60" width="45" height="120" rx="8" fill="rgba(13,148,136,0.12)" stroke="currentColor" stroke-width="1"/>
  <text x="392.5" y="50" fill="currentColor" font-size="11" text-anchor="middle">[6]</text>
  <text x="392.5" y="88" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">6</text>
  <text x="392.5" y="120" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">13</text>
  <text x="392.5" y="152" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">20</text>
  <g fill="currentColor" text-anchor="middle">
    <text x="230" y="218" font-size="12" font-weight="600">同値類 = 互いに素なブロック (台が過不足なく分割される)</text>
    <text x="230" y="236" font-size="10.5" opacity="0.75">congruent7 x y ⟺ 同じブロック. ブロックの集合 = 商集合 (第8章)</text>
  </g>
</svg>

同値関係は, [第8章](fp8.html)で **Eq 型クラス** として Haskell に登場します. `==` を自分のデータ型に定義するとき, それが満たすべき法則が, まさにこの反射・対称・推移の 3 法則です.

::: note

### Exercise CH7-6

**同値関係の判定と全数検査**

1. 2 つの文字列の **長さが等しい** ことを判定する関数 `sameLength :: String -> String -> Bool` を実装してください (`length` が使えます).

2. リスト `words0 = ["a", "ab", "abc", "xy", "z"]` の上で, `sameLength` が反射律・対称律・推移律をすべて満たすことを全数検査する式を書き, 3 つとも `True` になることを確認してください.

3. 関数 `evenProd :: Int -> Int -> Bool` を `evenProd x y = even (x * y)` (積が偶数) と定義します. この関係は $\{1, \dots, 10\}$ 上の同値関係でしょうか. 破れる法則を全数検査で特定し, 反例を 1 つ挙げてください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
    print $ sameLength "ab" "xy"   -- True
    print $ sameLength "ab" "abc"  -- False
    -- 2. の検査 3 つがすべて True になること
    -- 3. の検査で破れる法則が見つかること
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
sameLength :: String -> String -> Bool
sameLength x y = length x == length y

evenProd :: Int -> Int -> Bool
evenProd x y = even (x * y)

words0 :: [String]
words0 = ["a", "ab", "abc", "xy", "z"]

main :: IO ()
main = do
    print $ sameLength "ab" "xy"    -- True
    print $ sameLength "ab" "abc"   -- False
    -- 2. sameLength は同値関係 (3 法則すべて True)
    print $ and [ sameLength x x | x <- words0 ]                        -- True (反射律)
    print $ and [ sameLength y x | x <- words0, y <- words0
                , sameLength x y ]                                      -- True (対称律)
    print $ and [ sameLength x z | x <- words0, y <- words0, z <- words0
                , sameLength x y, sameLength y z ]                      -- True (推移律)
    -- 3. evenProd は反射律が破れる (対称律は成り立つ)
    print $ and [ evenProd x x | x <- [1..10] ]   -- False
    print $ evenProd 1 1                          -- False (反例: x = 1. 1*1 = 1 は奇数)
~~~

`evenProd` は対称律こそ満たしますが ($xy = yx$), 反射律が破れます: 奇数 $x$ に対して $x \cdot x$ は奇数なので, `evenProd x x = False` です. さらに推移律も破れます (例: `evenProd 1 2` と `evenProd 2 3` は `True` ですが, `evenProd 1 3` は `False`). 「積が偶数」は一見それらしく見えても, 同値関係ではありません.

</details>

提出ファイル名: `ch7-6.hs`

:::

## 順序関係: 「並べる」の一般化

同値関係が「等しい」の一般化だったのに対し, **順序関係** は「大小で並べる」($\leq$) の一般化です. 集合 $A$ 上の関係 $R$ が次の 3 つの法則を満たすとき, $R$ を **半順序 (partial order)** といいます.

- **反射律**: $\forall x \in A.\ x \mathrel{R} x$
- **反対称律 (antisymmetry)**: $\forall x, y \in A.\ (x \mathrel{R} y \land y \mathrel{R} x) \implies x = y$ (両向きに成り立つのは同じ要素どうしだけ).
- **推移律**: $\forall x, y, z \in A.\ (x \mathrel{R} y \land y \mathrel{R} z) \implies x \mathrel{R} z$

同値関係の定義と見比べてください. 反射律・推移律は共通で, **対称律が反対称律に置き換わった** だけです. 対称的なら「同じとみなす」方向に, 反対称的なら「並べる」方向に関係の性格が決まります. **どの法則を課すかが構造の性格を決める** わけです.

半順序の例は, 数の $\leq$ だけではありません.

- **部分集合関係 $\subseteq$**: $A \subseteq A$ (反射), $A \subseteq B \land B \subseteq A \implies A = B$ (反対称. これは述語と量化の節で見た集合の等しさの定義そのものです), $A \subseteq B \land B \subseteq C \implies A \subseteq C$ (推移).
- **割り切る関係 $\mid$** (正の整数上): $a \mid a$ (反射), $a \mid b \land b \mid a \implies a = b$ (反対称), $a \mid b \land b \mid c \implies a \mid c$ (推移).

ここで, $\leq$ と, $\subseteq$ や $\mid$ の間には大きな違いが 1 つあります. $\leq$ では **どの 2 要素も比較できます**:

$$\forall x, y \in A.\ (x \mathrel{R} y \lor y \mathrel{R} x)$$

この性質を **全域比較可能性 (totality)** といい, これも満たす半順序を **全順序 (total order)** といいます. 一方 $\mid$ では, たとえば $2$ と $3$ は **どちらも他方を割り切りません**. 比較不能な対が存在するため, 半順序ではあっても全順序ではありません.

割り切る関係で確かめてみましょう.

~~~ haskell
divides :: Int -> Int -> Bool
divides a b = b `mod` a == 0

main :: IO ()
main = do
    -- divides は {1..20} 上で半順序の 3 法則を満たす
    print $ and [ a `divides` a | a <- [1..20] ]                        -- True (反射律)
    print $ and [ a == b | a <- [1..20], b <- [1..20]
                , a `divides` b, b `divides` a ]                        -- True (反対称律)
    print $ and [ a `divides` c | a <- [1..20], b <- [1..20], c <- [1..20]
                , a `divides` b, b `divides` c ]                        -- True (推移律)
    -- しかし全順序ではない: 2 と 3 は比較不能
    print $ 2 `divides` 3 || 3 `divides` 2                              -- False
~~~

<svg viewBox="0 0 460 310" width="100%" style="max-width: 460px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="割り切る関係のハッセ図 (12 の約数). 下から 1, その上に 2 と 3, その上に 4 と 6, 頂上に 12. 線で結ばれた下の数が上の数を割り切る. 2 と 3 の間には上下の道がなく比較不能で, 半順序だが全順序ではない.">
  <line x1="230" y1="221" x2="140" y2="179" stroke="currentColor" stroke-width="1.3"/>
  <line x1="230" y1="221" x2="320" y2="179" stroke="currentColor" stroke-width="1.3"/>
  <line x1="140" y1="151" x2="140" y2="109" stroke="currentColor" stroke-width="1.3"/>
  <line x1="140" y1="151" x2="320" y2="109" stroke="currentColor" stroke-width="1.3"/>
  <line x1="320" y1="151" x2="320" y2="109" stroke="currentColor" stroke-width="1.3"/>
  <line x1="140" y1="81" x2="230" y2="44" stroke="currentColor" stroke-width="1.3"/>
  <line x1="320" y1="81" x2="230" y2="44" stroke="currentColor" stroke-width="1.3"/>
  <circle cx="230" cy="235" r="15" fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.4"/>
  <text x="230" y="239" fill="currentColor" font-family="monospace" font-size="12" font-weight="600" text-anchor="middle">1</text>
  <circle cx="140" cy="165" r="15" fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.4"/>
  <text x="140" y="169" fill="currentColor" font-family="monospace" font-size="12" font-weight="600" text-anchor="middle">2</text>
  <circle cx="320" cy="165" r="15" fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.4"/>
  <text x="320" y="169" fill="currentColor" font-family="monospace" font-size="12" font-weight="600" text-anchor="middle">3</text>
  <circle cx="140" cy="95" r="15" fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.4"/>
  <text x="140" y="99" fill="currentColor" font-family="monospace" font-size="12" font-weight="600" text-anchor="middle">4</text>
  <circle cx="320" cy="95" r="15" fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.4"/>
  <text x="320" y="99" fill="currentColor" font-family="monospace" font-size="12" font-weight="600" text-anchor="middle">6</text>
  <circle cx="230" cy="30" r="15" fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.4"/>
  <text x="230" y="34" fill="currentColor" font-family="monospace" font-size="12" font-weight="600" text-anchor="middle">12</text>
  <line x1="162" y1="165" x2="298" y2="165" stroke="currentColor" stroke-width="1" stroke-dasharray="4 4" opacity="0.5"/>
  <text x="230" y="158" fill="currentColor" font-size="9.5" text-anchor="middle" opacity="0.75">比較不能 (どちらも割り切らない)</text>
  <g fill="currentColor" text-anchor="middle">
    <text x="230" y="278" font-size="12" font-weight="600">半順序 (割り切る関係) のハッセ図. 線 = 割り切る (下 → 上)</text>
    <text x="230" y="296" font-size="10.5" opacity="0.75">2 と 3 のような比較不能な対がある = 全順序ではない</text>
  </g>
</svg>

順序関係は, [第8章](fp8.html)で **Ord 型クラス** として Haskell に登場します. `compare` や `<=` を自分のデータ型に定義するとき, それが満たすべき法則が全順序の法則です. [第5章](fp5.html)で書いた挿入ソートのような並べ替えが常に意味を持つのは, 比較が全順序だからです. 比較不能な対があると「正しい並び順」が定まりません.

::: note

### Exercise CH7-7

**部分集合関係は半順序だが全順序ではない**

`Int` のリストで有限集合を表すことにします (重複はないものとします).

1. 「`xs` のすべての要素が `ys` にも属する」($xs \subseteq ys$) を判定する関数 `subsetOf :: [Int] -> [Int] -> Bool` を, リスト内包表記と `and` を用いて実装してください (``x `elem` ys`` で「`x` が `ys` に含まれるか」を判定できます).

2. 集合 $\{1, 2, 3\}$ の部分集合 8 個を外延表記で列挙したリスト `subsets :: [[Int]]` を定義し, `subsetOf` が **反射律** と **推移律** を満たすことを全数検査してください.

3. **反対称律** について: リスト表現では同じ集合が `[1,2]` と `[2,1]` のように違う並びで書けるため, ここでは「相互に部分集合なら **集合として** 等しい」と読み替えます. `sameSet xs ys = subsetOf xs ys && subsetOf ys xs` を定義し, 反対称律 (相互包含なら `sameSet`) が成り立つことを確認してください.

4. `subsetOf` が **全順序ではない** ことを示す反例 (比較不能な 2 つの部分集合) を挙げてください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
    print $ [1,2] `subsetOf` [1,2,3]   -- True
    print $ [1,4] `subsetOf` [1,2,3]   -- False
    -- 2. 3. の検査がすべて True, 4. の反例が False になること
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
subsetOf :: [Int] -> [Int] -> Bool
subsetOf xs ys = and [ x `elem` ys | x <- xs ]

-- {1,2,3} の部分集合 8 個 (述語と量化の節で数えたとおり)
subsets :: [[Int]]
subsets = [[], [1], [2], [3], [1,2], [1,3], [2,3], [1,2,3]]

sameSet :: [Int] -> [Int] -> Bool
sameSet xs ys = subsetOf xs ys && subsetOf ys xs

main :: IO ()
main = do
    print $ [1,2] `subsetOf` [1,2,3]   -- True
    print $ [1,4] `subsetOf` [1,2,3]   -- False
    -- 2. 反射律と推移律
    print $ and [ s `subsetOf` s | s <- subsets ]                           -- True (反射律)
    print $ and [ a `subsetOf` c | a <- subsets, b <- subsets, c <- subsets
                , a `subsetOf` b, b `subsetOf` c ]                          -- True (推移律)
    -- 3. 反対称律 (sameSet の意味で)
    print $ and [ sameSet a b | a <- subsets, b <- subsets
                , a `subsetOf` b, b `subsetOf` a ]                          -- True (反対称律)
    -- 4. 全順序ではない: [1] と [2] は比較不能
    print $ [1] `subsetOf` [2] || [2] `subsetOf` [1]                        -- False
~~~

`subsetOf` の定義は, 部分集合の定義 $\forall x.\ (x \in T \implies x \in S)$ の有限版そのものです. 反対称律で `sameSet` への読み替えが必要になったのは, リストという **表現** と集合という **概念** の間にずれ (順序・重複) があるためで, これは内包表記の節の warn で述べた「リストは集合の比喩にすぎない」という注意の具体的な現れです.

</details>

提出ファイル名: `ch7-7.hs`

:::

## まとめ: 集合から構造へ

本章では, 型を集合とみなす見方を軸に, 集合を作る道具と, 集合の間・上の関係を整理しました.

| 集合論 | Haskell | 本章の節 |
| --- | --- | --- |
| 集合 / 要素 | 型 / 値 | 集合と列挙型 |
| 述語, $\forall$ / $\exists$ | `Bool` を返す関数, `and` / `or` による有限検査 | 述語と量化, 内包表記 |
| 内包表記 (部分集合の切り出し) | リスト内包表記 (値レベル), スマートコンストラクタ (型レベル) | 内包表記, スマートコンストラクタ |
| 直積 $A \times B$ ($\lvert A \times B \rvert = \lvert A \rvert \cdot \lvert B \rvert$) | 直積型・レコード構文 | 直積型 |
| 直和 $A + B$ ($\lvert A + B \rvert = \lvert A \rvert + \lvert B \rvert$) | 直和型 | 直和型 |
| 帰納的に定義された集合 (生成作用素の最小解) | 再帰的データ型 | 再帰的データ型 |
| 構成子 = 型を生成する $n$ 項演算 | データ構築子 (`Zero` = 0 項, `Succ` = 1 項, …) | 再帰的データ型 |
| 関係 $R \subseteq A \times B$ | 判定関数 `a -> b -> Bool` | 関係 |
| 関数 = 全域・右一意な関係 | (全域な) 関数. 部分関数は実行時エラーの源 | 関数 |
| 演算 = 台の中で閉じた関数 ($n$ 項, 0 項 = 定数) | `(+)`, `negate`, `0` | 演算 |
| 同値関係 (反射・対称・推移) | `Eq` の法則 ([第8章](fp8.html)) | 同値関係 |
| 全順序 (半順序 + 全域比較可能) | `Ord` の法則 ([第8章](fp8.html)) | 順序関係 |

後半で繰り返し現れた形に注目してください. 同値関係も順序関係も, 「**関係という対象** に, $\forall$ で書かれた **法則** を課したもの」として定義されました. そして法則は, 有限の範囲なら `and` とリスト内包表記で機械的に検査できました.

[第8章](fp8.html)では, この「対象に法則を課す」という組み立てを, 関係だけでなく **演算** に対して行います. 集合 (台) の上に演算を載せ, 結合律や単位元の存在といった法則を課したものが **代数構造** であり, Haskell では **型クラス** がその受け皿になります. 本章の同値関係と全順序は, それぞれ `Eq` と `Ord` の法則として, 型クラスの世界にそのまま再登場します.
