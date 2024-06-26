---
title: 代数プログラミング入門
description: 資料
tags:
    - algebra
    - lecture
    - statistics
    - haskell
featured: true
katex: true
tableOfContents: true
---

(執筆準備中)

代数の基礎と,代数による仕様記述,Haskellの基礎に関して書いていく予定です.
現在執筆中のため, 構成及び内容が今後変わります.


# はじめに

<details>
    <summary> 開く/閉じる </summary>

本資料は,正規の大学の科目ではなく, 学内での学生,教員の勉強会において使用する予定のものとなります. したがって,講義形式で作成しますが,通常の講義よりはかなり緩めの記述,内容が含まれます.

本講義では,関数型プログラミング言語Haskellの基礎,使用法,及び設計に関して扱います.
想定する履修者はPythonやJavaScriptなどの手続き型言語の使用経験はあるが,関数型言語を利用したことがない大学学部生です. 関数型言語の特徴を説明する際に手続き型言語の例としてPythonでの記述が出てきますが,Pythonの文法等に関しては既知のものとして扱います.(こちらはもとも官庁用の報告書として執筆したものを(大幅に)改変したものですので,もともとの資料ではVBAやJavaを事例として用いていました.)


また,本講義では代数学を利用したプログラミングの設計に関する方法論も扱います.集合論や代数学に関する知識は前提とせず,初歩から扱いますので,数学に関する前提知識は特に必要ありません. なお,本講義は集合論や代数学の習得を目的としているわけではないので,これらに関してはかなり簡略化した説明になります.専門的に数学を学びたい方向けの講義ではないことを理解したうえで受講してください.

一方で,CLIの操作やディレクトリの概念,ソフトウェアのインストール,テキストエディタの設定などの基本的なPC操作に関しては,扱いません. それらが分からない方は,それらを自分で学習するか,それらを扱っている講義を履修してから受講することをおすすめします.

## 本資料の読み方

(執筆中)


## Haskellとは

[`Haskell`](https://www.haskell.org)は,1987年に生まれた**静的型付けの純粋関数型言語**です. Haskellには,様々な特徴がありますが,本講義では,特に代数的データ型による,代数的なプログラミングに焦点をあてて,代数的な仕様記述とHaskellの関連を中心に議論します.

Haskellがどんな言語で,どのようなメリットがあるのか,という話は今後本講義でも扱いますが,ここでは深入りしません. 取り敢えず,どのような言語かを細かく説明する前に,関数型言語の雰囲気を掴んでもらおうと思います.


## 関数型言語の雰囲気

HaskellはLispやOCamlなどと同じ関数型言語です.関数型言語は関数によってプログラムを構築していく点にありますが,近年ではこのスタイルは関数型言語の専売特許というわけではなくなりつつあり,関数で書くことの特別さは,薄れつつあります. なので,ここでは,関数型言語の細かい機能について見る前に,関数型言語の考え方について,手続き型言語との違いという観点で見ていきましょう.

関数型言語でプログラミングををするとは,**｢それが何か｣**を分解して書いていくことです.
関数型プログラミングが宣言的であると言われる所以はそこにあります.手続き型言語が,｢何をどうするのか｣という手続きを書くのにたいして,｢欲しいものはなにか｣を宣言します.

こちらの(Haskell界隈では)有名な[ブログ](https://bartoszmilewski.com/2014/11/04/category-the-essence-of-composition/)では,関数型言語の考え方について以下のように説明しています.

> Functional programmers have a peculiar way of approaching problems. They start by asking very Zen-like questions. For instance, when designing an interactive program, they would ask: What is interaction? When implementing Conway’s Game of Life, they would probably ponder about the meaning of life.

- 翻訳(DeepL大先生)

> 関数型プログラマーは問題への取り組み方が独特だ. 禅問答のような質問から始めるのだ.例えば,インタラクティブなプログラムを設計するとき,彼らは「インタラクションとは何か?コンウェイの「人生ゲーム」を実装するとき,彼らはおそらく人生の意味について熟考するだろう.

手続き型プログラミングと関数型プログラミングの違いは色々とありますが,取り敢えずここでは,この文章に習って

- 関数型プログラミング: **｢それが何か｣**を問い,**｢それが何か｣**をプログラムする.

という観点に注目します. 例として以下の｢ウサギの問題｣について考えてみましょう.

::: note
**ウサギの問題**

- 1つがいのウサギは,生まれてから2ヶ月後から毎月1つがいずつのウサギを産む

- ウサギが死ぬことはない

- この条件の下で,生まれたばかりの1つがいのウサギは1年の間に何つがいのウサギになるか
:::


これについて,取り敢えず12ヶ月までのつがいの数をプログラムを用いて計算してみましょう.

まずは手続き型の考え方で数を数えてみます. 手続き型言語的には,｢ウサギのつがいの数｣を｢どのように求めるのかという手続き｣をプログラムに記述します.

::: note
<details>
    <summary> note </summary>
学生にプログラミングを教えているとこれくらいのプログラムは,for文,if文,代入などの概念をちらっと読んだだけで簡単にできる人もいれば,数時間教えてもできない人もいます.これが何によって異なるのかというのは,長年の謎で,教育の難しいところです.

しかも,プログラムを教える人間は大抵前者なので,教師も学生も何が分からないのか分からないという事態によくなってしまいますね.

しかし,大抵の場合後者の人に話を聞いていくと,そもそもこの手続きを日本語であっても書けないという人が多いようです. なので,本当に苦労するタイプの人は,パワーポイントでウサギの絵を並べてルールにのっとってウサギが増えていく様子を小学生に教える日本語資料を作ってというような作業を一緒にすることになります.

これを書きながらこういった学生が実は関数型なら簡単だったりしないだろうか,と考えていますが,楽観的に過ぎるだろうなという予感がしています.
</details>
:::

いろいろな方法がありますが手続き型言語っぽいフィボナッチ数の数え方を一つ考えると,例えば

::: note
- つがいは,新生ウサギ(0ヶ月)→子供ウサギ(1ヶ月)→大人うさぎ(2ヶ月)の順で変化する

- 大人うさぎのつがいは毎月1つの新生うさぎのつがいを産む

- 0ヶ月の新生うさぎの(こどもが産めない),子供ウサギ,大人うさぎの数を記録する
    - 新生 1
    - 子供 0
    - 大人 0

- 1月たつと
    - 大人と同じ数だけ新生が生まれる
    - 子供が大人になる
    - 新生が子供になる

- これを12ヶ月繰り返す
:::

というように｢何がどうなる｣という｢手順｣を書いた説明になるかと思います.
授業では大抵,これをフローチャートに書き直させて,フローチャートをプログラムに直すという作業をさせますが,そこは省略します.

これをPythonのプログラムにすると以下のようになり,結果は`233`となります.


~~~ python
# 初期化
months = 12  # シミュレートする月数

#1ヶ月目の状態
new_born_pairs = 0 #新生のつがいの数
young_pairs = 1  # 子供のつがいの数
mature_pairs = 0  # 大人のつがいの数

# 各月におけるうさぎのつがいの数をシミュレート
for month in range(1, months + 1):
    # 大人と同じ数だけ新生が生まれる
    new_born_pairs = mature_pairs
    # 子供が大人になる
    mature_pairs += young_pairs
    # 新生が子供になる
    young_pairs = new_born_pairs


# 成熟したつがいと若いつがいの合計
total_pairs = mature_pairs + young_pairs

print(total_pairs)
~~~

こういった考え方が,いわゆる手続き型的な考え方とプログラミングの方法になります.

では,関数型の考え方とはどのようなものでしょうか. 先ほど引用したように,関数型では,それが何かを考えます.つまり,ここで問われている｢つがいの数｣を抽象化して,その特徴を記述するわけですね.

特定の数がなにかのルールに基づいて段々と増えていくというときに,それを並べてみて,法則性を探るということが一般的に行われます.これは,高校数学で扱う漸化式の考え方ですね.

月ごとのつがいの数を,並べてみると以下のようになります. そして,その増え方を計算してみると一定のルールに基づいていることが分かります.

| 月    | つがいの数 | 計算     |
| :---: | :---:      | :------: |
|0      | 1          |          |
|1      | 1          |          |
|2      | 2          | 1 + 1    |
|3      | 3          | 1 + 2    |
|4      | 5          | 2 + 3    |
|5      | 8          | 3 + 5    |
|6      | 13         | 5 + 8    |
|7      | 21         | 8 + 13   |
|8      | 34         | 13 + 21  |
|9      | 55         | 21 + 34  |
|10     | 89         | 34 + 55  |
|11     | 144        | 55 + 89  |
|12     | 233        | 89 + 144 |


実はこのウサギのつがいの合計どの月でもは,1,1,2,3,5,8という風に前々月と前月のつがいの合計になることが知られています. このような,前の数字と前の前の数字の和によって次の数字を作る数をフィボナッチ数といいます.

::: warn
※
普通フィボナッチ数というと,0から始まりますが,ここではウサギの例で考えたいので1から始まることにします.
:::

フィボナッチ数を漸化式として捉えると,第n月のフィボナッチ数の正体は以下のように得られます.

::: note
$$ F_0 = 1 $$
$$ F_1 = 1 $$
$$ F_n = F_{n-1} + F_{n-2} (n >= 2)  $$
:::

したがって,上の条件での12ヶ月後のウサギの数はなにかという問題は,フィボナッチ数の第12番めの項$F_{12}$がなにかという問題であり,フィボナッチ数とはなにかといえば上の漸化式である,という風に考えることができます.

実際に計算手順を,一つひとつ追っていくのではなく,このように求めたい対象がなにかということを考えて,抽象化し記述するというのが,関数型言語の基本的な考え方になります.

ちなみに,これをHaskellで書くと以下のようになり,上の漸化式の書き方とかなり近い対応関係があることが分かります.

~~~ haskell
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
~~~

::: warn
※メモ化とかそういったことは,取り敢えずここでは置いておきます.
(この辺の数学的定義そのままだと,実用には向かない問題は,後ほど扱います.)
:::

これを実行してみると確かに正しい数が求められています.

~~~ sh
ghci> :{
ghci| fib :: Int -> Int
ghci| fib 0 = 1
ghci| fib 1 = 1
ghci| fib n = fib (n-1) + fib (n-2)
ghci| :}
ghci> fib 12
233
~~~

当然フィボナッチ数の漸化式は広く知られていますし, むしろ最初から漸化式として学習することが多いでしょう. したがって, Pythonでの実装もフィボナッチ数が漸化式であるという前提で,以下のように書くほうが一般的です.

~~~ python
def Fib(n):
    if n == 0:
        return 1
    elif n == 1:
        return 1
    else:
        return Fib(n-1) + Fib(n-2)
~~~

また,最近では,PythonやJavaScriptなどの手続き型の言語にも,関数型の考え方が導入され,内包表記,再帰,ラムダ式などの関数型のシンタックスも一般的に使われるようになっています(これらの詳細についてはこのあとやっていきます).逆にHaskell等の関数型言語においても,手続き型のほうが便利な場合には手続き型の記法を利用します.

したがって,現在では関数型的な考え方と,手続き型の考え方というのは,それほど明確に分かれるものではありません.

ここでは,手続き型の考え方と関数型の考え方の違いを説明するために,Pythonの事例をあえてあまり用いられない方法で書きましたが,大げさに書けば手続き型と関数型の考え方の違いとはこのような考え方,問題へのアプローチの仕方にあります.

## 関数型だと何が嬉しいのか

前節では,関数型の考え方に関して簡単な事例をしましました. 関数型の考え方がしっくり来る人は,それが関数型を使う理由になるでしょうが,しっくり来るという抽象的な話ではなく,具体的な関数型言語のメリット/デメリットをこの節では紹介します. なお,関数型言語と一言でいっても,様々な言語がありますし,前述のように手続き型と関数型が明確に分かれる時代でもありません.

関数型言語の設計仕様は,関数型です. 手続き型言語の仕様定義にもいろいろな種類があります.

(執筆中)
例の論文のまとめ





厳密な仕様記述を書くとプログラムと1体1対応になる.そもそもHaskellで書けばプログラムと仕様が対応関係を持つようになりますし,数式への変換も容易です.

そういった意図もあり,私が内閣府で統計作成を市ていた時代には, 数式による定義,とプログラムのペアを対応付けたOSSとして基幹統計を開発することを提唱していましたが,それは色々な制約でまだ実現していません.

## 設計も関数型で

(執筆中)

### 雑談:なんでHaskell?

(執筆中)

</details>

# Haskellセットアップ

<details>
    <summary> 開く/閉じる </summary>

言語の特徴や意味を色々と説明してきましたが,習うより慣れろということで,そろそろHaskellを利用してみましょう.Haskell Haskellの開発環境には様々なものがありますが,現在良く使われているものとして[Cabal](https://www.haskell.org/cabal/) + [GHCup](https://www.haskell.org/ghcup/)あるいは[Stack](https://docs.haskellstack.org/en/stable/)の2つがあります. CabalとStackはプロジェクトのビルドを行うためのアーキテクチャであり,GHCupは周辺環境のインストーラーです. どちらで開発を行ってもいいのですが,本稿ではStackを用います.

Stackは現在のHaskellの標準的なコンパイラである,Glasgow Haskell Compiler（GHC）に基づいたビルド環境である.(cabalもGHCですが). 他の言語と同様にHaskellでも様々なpackage(ライブラリ)を利用するのですが,package毎に他のpackageや,GHC(Haskellのコンパイラ)との依存関係があります.それらを使用するpackage事に調整することが人間には至難の業であり, 特定のpackageの依存関係を満たせば他のpackageの依存関係が満たされなくなるという試行錯誤を永遠と繰り返すことを`cabal hell`などと呼びます.

Stackにはそのようなpackage間の依存関係を満たすバージョンの組み合わせ(resolver)を利用して,自動で解決してくれる機能があり,Haskellでのブロジェクトの開発を容易にしてくれます. resolverの集まりを[Stackage](https://www.stackage.org)といい, resolverで扱われるpackageをまとめて管理するレポジトリのことを[Hackage](https://hackage.haskell.org)といいます.

## 環境構築

Stackの環境構築の方法は基本的には,[公式サイト](https://docs.haskellstack.org/en/stable/)に従ってください. 使用しているOS毎にインストール方法が異なるので注意しましょう特にMacユーザーはIntel Mac と Apple silliconでインストール方法が異なるので正しい方を選択するようにしてください.

インストールが終わったら,以下のコマンドでstackを最新版にupgradeします.

~~~ sh
stack upgrade
~~~

次に,開発用のディレクトリに移動して,開発用のプロジェクトを作成していきます. Stackでは,新しいプロジェクトの作成は`stack new [project-name]` コマンドで行われます. `stack new [project-name]`コマンドで新しいプロジェクトを作成すると,必要なファイルが含まれた`[project-name]`という名前のディレクトリが作成されます. 作成されたディレクトリに移動しましょう.

~~~ sh
> ls

> stack new hello-world
> ls
hello-world

> cd hello-world
~~~

作成されたディレクトリの構成は以下のようになっています.

~~~
.
├── app
│    └── Main.hs
├── src
│    └── Lib.hs
├── test
│   └── Spec.hs
├── hello-world.cabal
├── package.yaml
└── stack.yaml

~~~

それぞれの用途と意味は以下のとおりです.

::: note

- `app`フォルダの中には,実行可能ファイル用のプログラム

    - プロジェクトをbuildすると,`Main.hs`から実行可能ファイル(executable)が生成されます

    - この後,`Main.hs`の中身を編集して`Hello World`用のプログラムを作成します.

- `src`フォルダ内には,実行可能ファイルで利用するライブラリが格納されます.

    - ここに自分で開発したライブラリを含めることも可能です.

- `package.yaml`ファイルはプロジェクトの設定を記入するファイルです.

    - Hackageなどの外部のライブラリを利用する場合には,`package.yaml`内の`dependencies:`部分に,使用したいライブラリを記述します.

    - Stackは`stack setup`コマンドによって,package.yaml内に記述されたライブラリの依存関係を解決するresolverを自動で選択しますが,
    自分で使いたいresolverを`package.yaml`内の`resolver:`に続けて書くことで,指定することも可能です.

    - その他実行可能ファイルの設定や,コンパイルオプションなどを指定することができます.

    - `package.yaml` の設定に従って,プロジェクトの設定ファイル `test.cabal`が自動で作成されます.
    基本的にstackを使っている範囲では`.cabal`ファイルを自分で編集することはありません.


- `stack.yaml`ファイルは,stackの設定を記入します

    - resolverに含まれないライブラリ(自分のGitHub上にあるライブラリなど)を指定する,あるいはあえてresolverとは異なるバージョンを利用するときなどには
    `extra-deps:`に続けて,使用したいライブラリのレポジトリやバージョンを明示します.

:::

これらの利用法は,今後ライブラリを使用し始めたときに改めて学習すれば大丈夫ですが,取り敢えずプログラムを作成してきましょう.

`app/Main.hs`をテキストエディタで開いて編集していきましょう.

`app/Main.hs`を開くと,以下のようなファイル担っているかと思います. Haskellのプログラムをコンパイルした実行可能ファイルでは,`main =` 内の記述が実行されます.

~~~ haskell
module Main (main) where

import Lib

main :: IO ()
main = someFunc

~~~
現在は`sumFunc`という関数が実行されます. `sumFunc`は `import Lib` の記述によって, `src/Lib.hs`からimportされています. `src/Lib.hs`を開くと,

~~~ haskell
module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
~~~

という風に`someFunc`が定義されています. プログラム内の `someFunc :: IO ()` は`someFunc`の型注釈です. `IO ()` というのは,標準入出力 `IO` において, アクション `()` を実行するという意味ですが,ここではそれぞれの詳細は省きます. `putStrLn` は文字列を引数にとり,標準入出力`IO`に受け取った文字列を出力するというアクション` ()`を返す関数であり,ここでは,`"someFunc"`という文字列が出力されます. この`"someFunc"` 部分を `"Hello World"`に書き換えれば,Hello Worldは実行できます.関数の定義はこのあと徐々に扱いますが, someFuncは,引数を取らないので関数というよりは実際には値です.

`Lib.hs` に`helloWorld`と出力する値`helloWorld`を追加し,全体を以下のように書き換えましょう.

~~~ haskell
module Lib
    ( someFunc
    , helloWorld
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

helloWorld :: IO ()
helloWorld = putStrLn "Hello World"
~~~

`module Lib () where` はモジュール宣言で,他のプログラムから`import Lib`で,`src/Lib.hs`内に定義された関数や値などの内 `()`内に記述されたものを読み込むことができるようにします.
作成した値`helloWorld`を`()`内に`helloWorld`を追加することを忘れないようにしましょう.

併せて `app/Main.hs` を書き換えて,作成した`helloWorld`を実行しましょう.

~~~ haskell
module Main (main) where

import Lib

main :: IO ()
main = helloWorld
~~~

このプログラムをコンパイルして得られる実行可能ファイルの名前などは,`package.yaml`内で定義されています.

~~~ yaml
ghc-options:
- -Wall
- -Wcompat
- -Widentities
- -Wincomplete-record-updates
- -Wincomplete-uni-patterns
- -Wmissing-export-lists
- -Wmissing-home-modules
- -Wpartial-fields
- -Wredundant-constraints

library:
  source-dirs: src

executables:
  hello-world-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - hello-world
~~~

`ghc-options:` 以下の項目はghcのコンパイルオプションであり,`W`で始まるいずれのオプションもコンパイル時の`Warning`を追加するものである. これらのコンパイルオプションがあると,プログラムの品質を高めることができるが, 利用していてWarningが邪魔に感じた場合は,すべて削除しても問題ありません(
その場合は以下のように,`ghc-options:`部分を`#`でコメントアウトしてください.)

~~~ yaml
#ghc-options:

library:
  source-dirs: src

executables:
  hello-world-exe:
    main:                Main.hs
    source-dirs:         app
    ghc-options:
    - -threaded
    - -rtsopts
    - -with-rtsopts=-N
    dependencies:
    - hello-world
~~~

特に,本講義資料では,品質よりも分かりやすさを優先してできるだけシンプルな実装を紹介する他,事例としてあえて間違ったコードを入力する場面も存在する. そのままサンプルを入力すると多数のWarningが表示されることになるので,以下の説明中で登場する出力結果ではこれらのオプションはすべて切った状態のものとなっている点に留意していただきたい.

`library:`以下の記述で,利用するライブラリのPATH,`executables:`以下の記述で実行可能ファイルについて記述されています. ここでは, executableとして'app'フォルダ内にある'Main.hs'が'hello-world-exe'という名称でコンパイルされることが書かれています.`ghc-options:`以下は,コンパイル時のオプションを設定していますが,ここでは詳細は省略します.

`Main.hs`以外のファイルをここに追加すれば,いくらでも実行可能ファイルは増やすことができます.

`hello-world-exe`部分をもっと短い名前に変更することも可能です.なお生成される実行可能ファイルはMacでは`hello-world-exe`,Windowsでは`hello-world-exe.exe`になるので注意してください.

それでは,以下のコマンドでこのプロジェクトをbuildして,実行してみましょう.

~~~ sh
stack build
stack exec hello-world-exe
~~~

`stack build`のあと,プログラムにミスがなければ以下のように出力されるはずです(一部省略しています).

~~~ sh
❯ stack build
hello-world> build (lib + exe) with ghc-9.6.4
Preprocessing library for hello-world-0.1.0.0..
Building library for hello-world-0.1.0.0..
[1 of 2] Compiling Lib [Source file changed]
Preprocessing executable 'hello-world-exe' for hello-world-0.1.0.0..
Building executable 'hello-world-exe' for hello-world-0.1.0.0..
[1 of 2] Compiling Main [Source file changed]
[3 of 3] Linking .stack-work/dist/x86_64-osx/ghc-9.6.4/build/hello-world-exe/hello-world-exe [Objects changed]
hello-world> copy/register
Registering library for hello-world-0.1.0.0..
~~~


どこかで,タイプミスなどがあると例えば以下のようなエラーが表示される可能性もあります(一部省略しています).

~~~ sh
hello-world> build (lib + exe) with ghc-9.6.4
Preprocessing library for hello-world-0.1.0.0..
Building library for hello-world-0.1.0.0..
Preprocessing executable 'hello-world-exe' for hello-world-0.1.0.0..
Building executable 'hello-world-exe' for hello-world-0.1.0.0..
[1 of 2] Compiling Main [Source file changed]

/Users/akagi/Documents/Programs/Haskell/blog/hello-world/app/Main.hs:6:8: error: [GHC-88464]
    Variable not in scope: hellWorld :: IO ()
    Suggested fix: Perhaps use ‘helloWorld’ (imported from Lib)
  |
6 | main = hellWorld
  |        ^^^^^^^^^

Error: [S-7282]
       Stack failed to execute the build plan.

       While executing the build plan, Stack encountered the error:

       [S-7011]
       While building package hello-world-0.1.0.0
       Process exited with code: ExitFailure 1
~~~

上のエラーでは, `Main.hs`の6行目で使用されている,`hellWorld`が定義されていないという意味になります.
`helloWorld`と`o`を追加して正しい名称にしたあともう一度 `stack build`をしてみましょう.

`stack exec hello-world-exe`の後,`Hello World`と出力されていれば成功です.

なお,build と exec を併せて一つのコマンド`stack run` で代替することも可能です.

~~~ sh
❯ stack run hello-world-exe
hello-world> build (lib + exe) with ghc-9.6.4
Preprocessing library for hello-world-0.1.0.0..
Building library for hello-world-0.1.0.0..
Preprocessing executable 'hello-world-exe' for hello-world-0.1.0.0..
Building executable 'hello-world-exe' for hello-world-0.1.0.0..
hello-world> copy/register
Registering library for hello-world-0.1.0.0..
Hello World
~~~

</details>

# Haskellを使ってみよう

<details >
    <summary> 開く/閉じる </summary>

## ghci

前節では, Stackを利用した,プロジェクトの作成と実行に関して扱いましたが, Haskellにも対話環境が存在します.
`stack ghci`コマンドを打つことで, Haskellの対話環境が立ち上がります.


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

Haskellの基本的な数値型には以下の4つがあります.

|  型        | 意味             |
| ------     | ---------------- |
| `Int`      | 固定長整数型     |
| `Integer`  | 多倍長整数型     |
| `Float`    | 単精度浮動小数型 |
| `Double`   | 倍精度浮動小数型 |



`Int`と`Integer`は`整数`, `Float`と`Double`は`実数`を表しています.

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




Haskellにおける数値型の基本的な演算子は以下のように定義されています. 実数と整数で挙動が異なるものがあるので注意が必要です.

演算子には優先順位が設定されており,数字が大きいものから順に適用されます(最小0,最大9).
また,式を`()`で囲むことで,その内部が優先的に計算されます.

また,`()`が式の最後に来る場合には`$`記号以下が`()`に囲まれているとみなすことができます.


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

::: note

練習問題





:::



### リスト

複数のデータをまとめる方法はいくつかありますが,データを1列に並べた`List`型は代表的なデータ型です. Haskellには配列もありますが,Listの方がよく利用されます.
リストの操作にはここで扱う以外にも`リスト内包表記`や`高階関数`など様々なものがありますが,ここでは最も基本的ないくつかの機能のみに絞って,後ほど詳細を扱います.

Listはリストリテラル`[]`の中に要素を記入して,`,`(コンマ)で区切ることで宣言できます.

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
`x=101`,`y=202`として, 以下の命題の真偽をPythonで計算してください.

- xが偶数
- yが偶数
- xが偶数かつyが偶数
- xが偶数またはyが偶数
- x + y が奇数

:::

</details>

# 関数

<details open>
    <summary> 開く/閉じる </summary>

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

</details>

# 代数的データ型
<details open>
    <summary> 開く/閉じる </summary>

Haskellのデータ型はすべて代数的データ型です. 代数的データ型には, **列挙型**,**直積型**,**直和型**があり,構文として**レコード構文**などが存在します.

集合論の説明と対応したコードの書き方.
(圏論で書けというのはそのうちやりたい.)

## 命題と条件式
集合を定義するにあたって,数理的な定義の記法に用いる演算子を導入する. 数理的な定義の内,そこで述べられた言説が,「真か偽のいずれかに分類可能とされるもの」を命題といい,条件が与えられた命題を条件式という.

`x`に関する条件式を
$P(x)≔***$ や $Q(x)$
と書き，`***`の部分に,命題が記述される．

命題の記述には以下の論理演算子が用いられる．

- $P(x) \lor Q(x)$： $P(x)$または$Q(x)$

- $P(x) \land Q(x)$：P(x)かつQ(x)

- $p(x) \Rightarrow q(x)$：$p(x)$ならば$q(x)$

- $p(x) \Leftrightarrow q(x) ∶$ $p(x)$ならば $q(x)$ かつ $q(x)$ ならば $p(x)$

- $ \neg p(x):p(x)$ の否定

なお, $p(x) \Rightarrow q(x) \Leftrightarrow \neg p(x) \lor q(x)$


## 集合

Haskellではデータ型を集合と**みなすこと**ができます(むしろ良く議論されるのは集合の圏 **$\mathbb{Set}$**
ですが,集合論の範囲でしばらく話しを進めます). Haskellの型はあくまで型であり,厳密には集合ではありません. また,このあと出てくるリストを使った`内包表記`などの**集合論的な書き方**も数学における集合ではありません.
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

$$ GoldenRetriever \in MyPets $$ の様に書きます. 要素に属さないことは $Chihuahua \notin MyPet$と書きます.

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
<details>
    <summary> Voidの利用例 開く/閉じる </summary>

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

</details>
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


</details>

# 手続き型Haskell



# 圏論とHaskell


# 発展:会計プログラム

yakagika