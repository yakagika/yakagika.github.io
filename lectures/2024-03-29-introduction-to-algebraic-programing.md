---
title: 代数プログラミング入門
description: 資料
tags: [algebra, lecture, statistics, haskell]
featured: true
katex: true
tableOfContents: true
---

(執筆準備中)

代数の基礎と,代数による仕様記述,Haskellの基礎に関して書いていく予定です.


# はじめに

<details open>
    <summary> 開く </summary>

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


これについて,取り敢えず12ヶ月までのフィボナッチ数をプログラムを用いて計算してみましょう.

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

- 0ヶ月の非成熟な(こどもが産めない)つがいの数,成熟したつがいの数を記録する
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

したがって,上の条件での12ヶ月後のウサギの数はなにかという問題は,フィボナッチ数の第12番めの項$F_12$がなにかという問題であり,フィボナッチ数とはなにかといえば上の漸化式である,という風に考えることができます.

実際に計算手順を,一つひとつ追っていくのではなく,このように求めたい対象がなにかということを考えて,抽象化し記述するというのが,関数型言語の基本的な考え方になります.

ちなみに,これをHaskellで書くと以下のようになり,上の漸化式の書き方とかなり近い対応関係があることが分かります.

~~~ haskell
fib 0 = 0
fib 1 = 1
fib n = f (n - 1) + f (n - 2)
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

# Haskellをはじめよう
<details>
    <summary> 開く </summary>

言語の特徴や意味を色々と説明してきましたが,習うより慣れろということで,そろそろHaskellを利用してみましょう.Haskell Haskellの開発環境には様々なものがありますが,現在良く使われているものとして[Cabal](https://www.haskell.org/cabal/) + [GHCup](https://www.haskell.org/ghcup/)あるいは[Stack](https://docs.haskellstack.org/en/stable/)の2つがあります. CabalとStackはプロジェクトのビルドを行うためのアーキテクチャであり,GHCupは周辺環境のインストーラーです. どちらで開発を行ってもいいのですが,本稿ではStackを用います.

Stackは現在のHaskellの標準的なコンパイラである,Glasgow Haskell Compiler（GHC）に基づいたビルド環境である.(cabalもGHCですが). 他の言語と同様にHaskellでも様々なpackage(ライブラリ)を利用するのですが,package毎に他のpackageや,GHC(Haskellのコンパイラ)との依存関係があります.それらを使用するpackage事に調整することが人間には至難の業であり, 特定のpackageの依存関係を満たせば他のpackageの依存関係が満たされなくなるという試行錯誤を永遠と繰り返すことを`cabal hell`などと呼びます.

Stackにはそのようなpackage間の依存関係を満たすバージョンの組み合わせ(resolver)を利用して,自動で解決してくれる機能があり,Haskellでのブロジェクトの開発を容易にしてくれます. resolverの集まりを[Stackage]((https://www.stackage.org))といい, resolverで扱われるpackageをまとめて管理するレポジトリのことを[Hackage](https://hackage.haskell.org)といいます.

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

という風に`someFunc`が定義されています. プログラム内の `someFunc :: IO ()` は`someFunc`関数の型注釈です. `IO ()` というのは,標準入出力 `IO` において, アクション `()` を実行するという意味ですが,ここではそれぞれの詳細は省きます. `putStrLn` は文字列を引数にとり,標準入出力`IO`に受け取った文字列を出力するというアクション` ()`を返す関数であり,ここでは,`"someFunc"`という文字列が出力されます. この`"someFunc"` 部分を `"Hello World"`に書き換えれば,Hello Worldは実行できます.

`Lib.hs` に`helloWorld`と出力する関数`helloWorld`を追加し,全体を以下のように書き換えましょう.

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

`module Lib () where` はモジュール宣言で,他のプログラムから`import Lib`で,`src/Lib.hs`内に定義された関数などの内 `()`内に記述されたものを読み込むことができるようにします.
作成した`helloWorld`関数を`()`内に`helloWorld`を追加することを忘れないようにしましょう.

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

# Haskellの基礎

<details>
    <summary> 開く </summary>

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

Haslellではコメントアウトは `--` です. 複数行に渡る場合は `{- -}` で囲みます. Haskellのプログラムを読んでいると `--|` や `--^` というタイプのコメントを良く見ますが, こちらはHaskellのドキュメント生成ライブラリにおいて, ドキュメント中に説明として記述するための記号です.
またコメント中に `>>>` と記述することでテストが実装できるなどいろいろなものがありますが,本資料では扱いません.

~~~ sh
ghci> -- コメント
ghci> {- コメント-}
~~~

## 複数行モード

ghci上で複数行のプログラムを書く場合には `:{ :}` でプログラムを囲います. 例えば,先程のフィボナッチ数のプログラムをghci上で実行する場合,位置行ずつ定義すると,定義が更新されてき最後の `f n = f (n-1) + f (n-2)`のみが記憶されます. この場合,`n`は無限にマイナスに続いていくため,`Stack Overflow`エラーが表示されます.

~~~ sh
ghci> fib 0 = 1 -- fの定義が上書きされる
ghci> fib 1 = 1 -- fの定義が上書きされる
ghci> f n = f (n-1) + f (n-2)
ghci> f 12
*** Exception: stack overflow
~~~

`:{ :}`で囲むことでひとまとまりの定義として認識されます.

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

なお,スクリプトの場合は,`:{ :}`なしでそのまま改行すれば問題ありません.

## 基本的な計算と演算子
Haskellにおける基本的な計算のための演算子は以下のように定義されています. 実数と整数で挙動が異なるものがあるので注意が必要です(データ型に関する説明は,後に行うのでここでは単に実数と整数に区別があるという程度の理解で問題ありません.)

演算子には優先順位が設定されており,数字が大きいものから順に適用されます.
また,式を`()`で囲むことで,その内部が優先的に計算されます.


|  計算      | 記号 | 優先順 |
| ------     | ---- | -----  |
| 足し算     | `+`  | 6      |
| 引き算     | `-`  | 6      |
| 掛け算     | `*`  | 7      |
| 割り算     | `/`  | 7      |
| 冪乗(整数) | `^`  | 8      |
| 冪乗(実数) | `**` | 8      |


~~~ sh
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
~~~

これらは中置演算子として定義されていますが演算子を`()`で囲むことによって前置(逆ポーランド記法)で利用することができます.

~~~ sh
ghci> (+) 3 4
7
ghci> (*) ((+) 3 4) 2
14
~~~


また, 2引数関数として定義された前置の演算子は `\`\`` (バッククオート)で囲むことで, 中置演算子として利用できます.



|  計算      | 記号  | 優先順 |
| ------     | ----  | -----  |
| 整数除算   | `div` | 7      |
| 剰余       | `mod` | 6      |

~~~ sh
ghci> 5 /2
2.5
ghci> div 5 2
2
ghci> 5 `div` 2
2
ghci> 5 `mod` 2
1
~~~

練習問題

## 型

型に関しては,かなり奥が深い,というよりHaskellの面白さは自分で型を作っていくことにあります. ただ,いきなりそれをすると,わけがわからなくなるのでここでは代数的データ型などには触れず手続き型言語にもあるような基礎的な型に関して説明します.

## 関数

関数の定義の仕方

演算子のユーザー定義

## 指示関数

## 再帰

## 高階関数

</details>

# 代数的データ型
<details>
    <summary> 開く </summary>

集合論の説明と対応したコードの書き方.
(圏論で書けというのはそのうちやりたい.)

## 命題と条件式
集合を定義するにあたって,数理的な定義の記法に用いる演算子を導入する. 数理的な定義の内,そこで述べられた言説が,「真か偽のいずれかに分類可能とされるもの」を命題といい,条件が与えられた命題を条件式という.

`x`に関する条件式を
$p(x)≔***$ や $q(x)$
と書き，`***`の部分に,命題が記述される．

命題の記述には以下の論理演算子が用いられる．

- $p(x) \lor q(x)$： $p(x)$または$q(x)$

- $p(x) \land q(x)$：p(x)かつq(x)

- $p(x) \Rightarrow q(x)$：$p(x)$ならば$q(x)$

- $p(x) \Leftrightarrow q(x) ∶$ $p(x)$ならば $q(x)$ かつ $q(x)$ ならば $p(x)$

- $ \neg p(x):p(x)$ の否定

なお, $p(x) \Rightarrow q(x) \Leftrightarrow \neg p(x) \lor q(x)$

真偽値表

### Haskellでの真偽値の判定

Boolについて


なんか例題


## 集合
特定のモノ(要素)がそこに｢属するか判定可能なモノの集まり｣を｢集合｣という．

要素xがSに属するとき,

$$x \in S$$

と表記される．なお，xがSに属さないことを x ∉S  と書く．
要素が一つも属さない集合，空集合も存在し記号ϕ または｛｝によって表される．
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



### 内包表記

## 包含

## 積と和

## 代数とクラス

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