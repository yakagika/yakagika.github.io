---
title: 代数プログラミング入門 Ch2 環境構築
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
previousChapter: iap1.html
nextChapter: iap3.html
---

# Haskellセットアップ

言語の特徴や意味を色々と説明してきましたが,習うより慣れろということで,そろそろHaskellを利用してみましょう.Haskellの開発環境には様々なものがありますが,現在良く使われているものとして[`Cabal`](https://www.haskell.org/cabal/) + [`GHCup`](https://www.haskell.org/ghcup/)あるいは[`Stack`](https://docs.haskellstack.org/en/stable/)の2つがあります. CabalとStackはプロジェクトのビルドを行うためのアーキテクチャであり,GHCupは周辺環境のインストーラーです. どちらで開発を行ってもいいのですが,本稿では`Stack`を用います.

Stackは現在のHaskellの標準的なコンパイラである,`Glasgow Haskell Compiler（GHC）`に基づいたビルド環境です(cabalもGHCですが). 他の言語と同様にHaskellでも様々なpackage(ライブラリ)を利用するのですが,package毎に他のpackageや,GHC(Haskellのコンパイラ)との依存関係があります.それらを使用するpackage事に調整することが人間には至難の業であり, 特定のpackageの依存関係を満たせば他のpackageの依存関係が満たされなくなるという試行錯誤を永遠と繰り返すことを`cabal hell`などと呼びます.

Stackにはそのようなpackage間の依存関係を満たすバージョンの組み合わせ(`resolver`)を利用して,自動で解決してくれる機能があり,Haskellでのブロジェクトの開発を容易にしてくれます. resolverの集まりを[`Stackage`](https://www.stackage.org)といい, resolverで扱われるpackageをまとめて管理するレポジトリのことを[`Hackage`](https://hackage.haskell.org)といいます.

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

~~~ sh
❯ tree
.
├── CHANGELOG.md
├── LICENSE
├── README.md
├── Setup.hs
├── app
│   ├── hello.hs
├── hello-world.cabal
├── package.yaml
├── src
│   └── Lib.hs
├── stack.yaml
└── test
    └── Spec.hs
~~~

それぞれの用途と意味は以下のとおりです.

::: note

- `app`フォルダの中には,実行可能ファイル用のプログラム

    - プロジェクトをbuildすると,`Main.hs`から実行可能ファイル(executable)が生成されます

    - この後,`Main.hs`の中身を編集して`Hello World`用のプログラムを作成します.

---

- `src`フォルダ内には,実行可能ファイルで利用するライブラリが格納されます.

    - ここに自分で開発したライブラリを含めることも可能です.

---

- `package.yaml`ファイルはプロジェクトの設定を記入するファイルです.

    - Hackageなどの外部のライブラリを利用する場合には,`package.yaml`内の`dependencies:`部分に,使用したいライブラリを記述します.

    - Stackは`stack setup`コマンドによって,package.yaml内に記述されたライブラリの依存関係を解決するresolverを自動で選択しますが,
    自分で使いたいresolverを`package.yaml`内の`resolver:`に続けて書くことで,指定することも可能です.

    - その他実行可能ファイルの設定や,コンパイルオプションなどを指定することができます.

    - `package.yaml` の設定に従って,プロジェクトの設定ファイル `test.cabal`が自動で作成されます.
    基本的にstackを使っている範囲では`.cabal`ファイルを自分で編集することはありません.

---

- `stack.yaml`ファイルは,stackの設定を記入します

    - resolverに含まれないライブラリ(自分のGitHub上にあるライブラリなど)を指定する,あるいはあえてresolverとは異なるバージョンを利用するときなどには
    `extra-deps:`に続けて,使用したいライブラリのレポジトリやバージョンを明示します.

:::

これらの利用法は,今後ライブラリを使用し始めたときに改めて学習すれば大丈夫なので,取り敢えずプログラムを作成してきましょう.


## Hello World

環境構築が上手くできているかを確認するために,`Hello World`用のプログラムを作成してみましょう.

まずは,`app/Main.hs`をテキストエディタで開いて編集します.

`app/Main.hs`を開くと,以下のようなファイルになっているかと思います. Haskellのプログラムをコンパイルした実行可能ファイルでは,`main =` 内の記述が実行されます.

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

`ghc-options:` 以下の項目はghcのコンパイルオプションであり,`W`で始まるいずれのオプションもコンパイル時の`Warning`を追加するものです. これらのコンパイルオプションがあると,プログラムの品質を高めることができますが, 利用していてWarningが邪魔に感じた場合は,すべて削除しても問題ありません(
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

特に,本講義資料では,品質よりも分かりやすさを優先してできるだけシンプルな実装を紹介する他,事例としてあえて間違ったコードを入力する場面も存在します. そのままサンプルを入力すると多数のWarningが表示されることになるので,以下の説明中で登場する出力結果ではこれらのオプションはすべて切った状態のものとなっている点に留意してください.

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
