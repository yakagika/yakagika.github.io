---
title: 関数型プログラミング Ch4 スクリプトファイルとエラー対応
description: 資料
tags:
    - algebra
    - lecture
    - haskell
featured: true
katex: true
date: 2024-10-18
tableOfContents: true
previousChapter: fp3.html
nextChapter: fp5.html
---

# スクリプトファイルの実行 (復習)

::: warn

**コラム: Windows / VSCode 環境でよくある引っかかり**

Haskell コード自体ではなく **開発環境** の設定が原因で `stack ghci` や `stack build` がうまく動かないケースが多発します. 「コードは合っているのに動かない」「エラーメッセージが文字化けして読めない」といった症状が出たら, **Haskell ではなく環境を疑って** 以下を確認してください.

### 1. VSCode で **階層が一つ上のディレクトリ** を開いていないか

VSCode の「Open Folder」で例えば `~/Projects` を開いてしまい, 実際のプロジェクト本体 `~/Projects/myproject` をターミナルから扱おうとすると, Stack が `stack.yaml` や `package.yaml` を見つけられず混乱します.

→ **`stack.yaml` / `package.yaml` がある階層を VSCode のルートとして開き直す** こと. ターミナルの `pwd` で現在地と一致しているかを確認しましょう.

参考 (VSCode 公式 — 英語): [Workspaces in Visual Studio Code](https://code.visualstudio.com/docs/editor/workspaces)

### 2. VSCode で **複数のウィンドウから同じディレクトリを開いている** ことはないか

同一プロジェクトを複数 VSCode ウィンドウで同時に開くと, Haskell Language Server や Stack の状態が二重に走り, ビルドキャッシュの食い違いやメモリ大量消費の原因になります.

→ **使っていないウィンドウは閉じて 1 つに揃える** こと. 「Window メニュー」や Cmd/Ctrl+` で開いているウィンドウ一覧を確認できます.

### 3. ターミナルの **コードページ (`chcp`) が UTF-8 になっているか** (Windows のみ)

Windows の PowerShell / cmd は既定で **Shift_JIS (コードページ `932`)** を使います. Haskell の標準出力は **UTF-8** なので, 日本語を含む文字列を扱うと文字化けや出力エラーが発生することがあります.

応急処置としては, ターミナルで UTF-8 (`65001`) に切り替えるコマンドを打てば解消します:

~~~ sh
chcp 65001
~~~

ただし `chcp 65001` は **そのターミナル 1 回限り** の効果しか持たないため, ターミナルを開き直すたびに毎回打ち直す必要があります. **VSCode で Haskell を学習するなら, 統合ターミナルが起動した瞬間に UTF-8 になるよう `settings.json` に書いておくのが圧倒的に楽** です. 以下を推奨します.

#### 推奨: VSCode の `settings.json` で起動時に自動で UTF-8 にする

1. VSCode で **`Ctrl + Shift + P`** → 「**Preferences: Open User Settings (JSON)**」 を選んで `settings.json` を開く.
2. 次の内容を追記する (既にある設定とマージする形で OK):

~~~ json
{
    "terminal.integrated.defaultProfile.windows": "PowerShell (UTF-8)",
    "terminal.integrated.profiles.windows": {
        "PowerShell (UTF-8)": {
            "source": "PowerShell",
            "args": [
                "-NoExit",
                "-Command",
                "chcp 65001 | Out-Null"
            ]
        }
    }
}
~~~

3. VSCode の統合ターミナル (`` Ctrl + ` ``) を一度閉じて開き直す.

これで, **新しくターミナルを開くたびに自動で UTF-8 に切り替わった状態で立ち上がる** ようになります.

各引数の役割:

| 引数 | 役割 |
|---|---|
| `-NoExit` | `chcp 65001` を実行した後も PowerShell を終了させずに対話モードを継続 |
| `-Command "chcp 65001 \| Out-Null"` | 起動時に `chcp 65001` を実行. `Out-Null` で「Active code page: 65001」の出力を消す |
| `source: "PowerShell"` | VSCode が検出した PowerShell 実行ファイルを使う (Windows PowerShell 5.1 / PowerShell 7 のどちらでも OK) |

#### 補足: より恒久的な対処 (任意)

- **OS 全体で UTF-8 にする**: Windows の「地域」設定 → 「管理」タブ → 「システム ロケールの変更」で **「ベータ: ワールドワイド言語サポートで Unicode UTF-8 を使用」** にチェック. これで PowerShell / cmd / 他のアプリすべてが UTF-8 で動作するようになりますが, 一部の旧 Windows アプリで文字化けする可能性があるため **ベータ機能扱い**.
- **PowerShell プロファイルに書く**: VSCode 以外のターミナル (Windows Terminal など) でも自動 UTF-8 にしたい場合は, `notepad $PROFILE` でプロファイルを開いて `chcp 65001 | Out-Null` を追記する方法もあります.

参考:

- [VSCodeの統合ターミナルでPowerShell起動時、文字コードをUTF-8に変える方法 (Qiita)](https://qiita.com/s4i/items/d6d19c24b9d8ff193f72)
- [Windowsでのよくある落とし穴 (Haskell-jp ブログ)](https://haskell.jp/blog/posts/2017/windows-gotchas.html)

### 4. **Smart App Control (Windows 11) が Stack をブロック** している (Windows のみ)

Windows 11 の **Smart App Control (スマートアプリコントロール, 略 SAC)** が有効になっていると, Stack が GHC のビルドのために内部で起動する **`Cabal-simple_*.exe` (`C:\sr\setup-exe-cache\...`)** が「未署名」と判定され, 起動自体が阻まれて `stack build` が以下のような cryptic なエラーで失敗します.

~~~ sh
PS C:\Users\user\Haskell\test2> stack build
test2> configure (lib + exe)

Error: [S-7282]
       Stack failed to execute the build plan.

       While executing the build plan, Stack encountered the error:

       C:\sr\setup-exe-cache\x86_64-windows\Cabal-simple_O_vy6YIf_3.12.1.0_ghc-9.10.3.exe: startProcess: invalid argument (Invalid argument)
~~~

メッセージは `startProcess: invalid argument` という Haskell 由来の文言で **原因が極めて分かりにくい** のが特徴です. 実体は SAC によるブロックなので, 修正は Windows 側の設定で行います.

→ **対策: スマートアプリコントロールを OFF にする**

`設定` → `プライバシーとセキュリティ` → `Windows セキュリティ` → `アプリとブラウザーの制御` → `スマートアプリコントロールの設定` → **オフ**

::: warn
SAC は本来 Windows 全体のマルウェア防御の一部なので, OFF にすると保護レベルが下がります. それでも本講義では Stack / Haskell 開発を進めるためには現状この方法しか実用解がありません.

サーベイ結果(2026 年 5 月時点):

- **SAC には個別アプリの除外リスト機能が無い**. Microsoft 公式 FAQ で「**There is currently no way to bypass Smart App Control protection for individual apps**」と明記.
- `Cabal-simple_*.exe` のような Stack 内部の補助実行ファイルは Microsoft 署名を持たないため一律ブロックされる.
- イベントログ (`Applications and Services Logs > Microsoft > Windows > CodeIntegrity > Operational`, Event ID 3076/3077) で具体的にどの exe がブロックされたかは確認できるが, ブロックを解除する手段は無い.
- 当初 SAC は「**一度 OFF にするとクリーンインストールしないと再有効化できない**」仕様だったが, 最近の Windows update で **Windows セキュリティアプリから再有効化可能** に改善されている.

そのため「開発が終わったら再度 ON に戻す」運用も実用可能です. 個別の Stack 関連 exe を whitelist する手段は **2026 年現在も存在しません**.
:::

参考: [Smart App Control Frequently Asked Questions (Microsoft Support)](https://support.microsoft.com/en-us/windows/smart-app-control-frequently-asked-questions-285ea03d-fa88-4d56-882e-6698afdb7003)


---

これらは本章後半で扱う **GHC のエラー** とは別軸の「環境起因の問題」です. エラーメッセージそのものに違和感が無く挙動だけが変な場合は, まず上記 3 点を疑ってみてください.

:::


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

`stack run practice` で`practice`と表示されれば成功です.

これからスクリプトで実行していくにあたって,`practice.hs`の中身をもう少し詳しく見てみましょう.

~~~ haskell
import Lib

main :: IO ()
main = putStrLn "practice"
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
practice1
practice2
practice3
~~~

また,ghciと異なって,出力結果が同じ画面に現れないので,
以降のコード例では, その行の結果をコメントで書くこととします. コメント部分は,記述しなくても結果は変わらないので,省略しても構いません.

::: warn
以降, CH3 以降の各練習問題に関しては異なる`hs`ファイルを作成し, package.yaml に追加,実行するようにして下さい.
課題として都度提出してもらいます.

- 各演習は **`Exercise CHN-K`** という見出しで識別されます (例: `Exercise CH4-3`).
  - URL アンカーは `#exercise-chN-K` (例: `fp4.html#exercise-ch4-3`).
  - 提出ファイル名は `chN-K.hs` (例: 3章 の 練習問題 1 → `ch3-1.hs`).
- package.yaml 上の `executables` 名称は任意につけて構いません.

提出先に関しては講義中に指定します.
:::

# エラー対応

スクリプトを書き始めると, ほぼ確実にエラーに遭遇します. Haskellのエラーメッセージは慣れていないと長く感じられますが, **構造が固定化されている** ため, 読み方さえ覚えれば原因がほぼ自動的に分かります.

本章では (1) エラーメッセージの基本構造, (2) 典型的な 5 種類のエラーパターン, (3) fp3 までの内容を題材にしたエラー解決演習 を扱います.

::: note
本章のエラー例は fp3 までの語彙 (トップレベル束縛, `main`, `let`, ghci で見た組込関数) のみで構成しています. 関数定義に伴う典型エラー (関数版 Non-exhaustive patterns / 引数不足など) は **fp5 以降の演習** にも含めていますので, そちらでも実例を確認してください.
:::

## エラーメッセージの構造

GHC のエラーは概ね以下の **4 ブロック構造** で出力されます.

~~~ sh
<ファイルパス>:<行>:<列>: error: [<エラーコード>]
    <人間向けメッセージ (`•` 記号で始まる箇条書き)>
    <文脈情報 (`In an equation for...` など)>
  |
<行番号> | <該当ソース行>
       | <該当箇所の ^ マーク>
~~~

たとえば:

~~~ sh
/path/to/Main.hs:3:14: error: [GHC-88464]
    Variable not in scope: greeting
  |
3 | main = print greeting
  |              ^^^^^^^^
~~~

各ブロックの読み方:

| ブロック | 役割 | 着目点 |
|---|---|---|
| **ファイル + 行:列** | エラー位置 | まずここで「どこか」を特定 |
| **error: [GHC-xxxxx]** | エラーコード | 検索キーワードとして使える |
| **`•` の行** | 人間向け要約 | 何が間違っているか |
| **`In ...` の行** | 文脈 | どの式・宣言の中で起きたか |
| **`^^^^` マーク** | 該当箇所 | ソース内の正確な位置 |

エラー読解の順序は: **「位置」→「タイプ(要約)」→「ソース上の該当箇所」** の順が最速です. エラーコード (`GHC-88464` 等) は Google 検索でも有効です.

## 典型的なエラーパターン

### 1. `Variable not in scope` (未定義変数)

**スコープ(参照可能範囲)に変数が見つからない** ときのエラー. **タイポ** または **大文字小文字の取り違え** が最頻出原因です.

ソース:

~~~ haskell
main :: IO ()
main = print greeting
~~~

実エラー出力:

~~~ sh
app/Main.hs:2:14: error: [GHC-88464]
    Variable not in scope: greeting
  |
2 | main = print greeting
  |              ^^^^^^^^
~~~

修正: 変数を事前に定義するか, 既存変数のタイポを直す.

~~~ haskell
greeting :: String
greeting = "hello"

main :: IO ()
main = print greeting
~~~

::: warn
変数の最初の文字を **大文字で書いた場合** は別のエラーになります.
Haskell は小文字始まり = 変数, 大文字始まり = 型 / コンストラクタ, と扱うため,
`Greeting` と書くと「`Data constructor not in scope: Greeting`」になります.

~~~ sh
app/Main.hs:3:14: error: [GHC-88464]
    Data constructor not in scope: Greeting
~~~
:::

### 2. `Couldn't match type` (型不一致)

ある型を期待しているところに別の型が来たとき出るエラー. **`(expected type) と (actual type) が不一致`** という構造になっています.

ソース:

~~~ haskell
main :: IO ()
main = do
    let n = 1 :: Int
    print (n ++ "!")
~~~

実エラー出力:

~~~ sh
app/Main.hs:4:12: error: [GHC-83865]
    • Couldn't match expected type ‘[Char]’ with actual type ‘Int’
    • In the first argument of ‘(++)’, namely ‘n’
      In the first argument of ‘print’, namely ‘(n ++ "!")’
      In a stmt of a 'do' block: print (n ++ "!")
  |
4 |     print (n ++ "!")
  |            ^
~~~

読み方:

- `expected type ‘[Char]’`: 「`[Char]` (= `String`) が来るはずだった」(`(++)` は両側ともリストを要求)
- `actual type ‘Int’`: 「実際には `Int` が来た」(`n` の型)

修正方針: 文字列同士の連結にするなら `show` で `n` を文字列に変換.

~~~ haskell
main = do
    let n = 1 :: Int
    print (show n ++ "!")    -- "1!"
~~~

### 3. `No instance for` (型クラス制約の不満足)

ある型に対して要求された機能(`Show`, `Num`, `Eq` など)が定義されていないときのエラー.

ソース (型不適合: `putStrLn` に数値を渡す):

~~~ haskell
main :: IO ()
main = putStrLn 42
~~~

実エラー出力:

~~~ sh
app/Main.hs:2:17: error: [GHC-39999]
    • No instance for ‘Num String’ arising from the literal ‘42’
    • In the first argument of ‘putStrLn’, namely ‘42’
      In the expression: putStrLn 42
      In an equation for ‘main’: main = putStrLn 42
  |
2 | main = putStrLn 42
  |                 ^^
~~~

読み方:

- `No instance for ‘Num String’`: 「`String` 型に `Num` (数値) のインスタンスがない」
- `arising from the literal ‘42’`: 「リテラル `42` から要請されている」
- → `putStrLn :: String -> IO ()` なので `42` が `String` だと思われ, でも `42` は `Num` 由来なので不適合

修正: 文字列にしたいなら `show` で変換.

~~~ haskell
main = putStrLn (show 42)
~~~

別のケース (関数そのものを print してしまう):

~~~ haskell
main :: IO ()
main = print head
~~~

実エラー出力:

~~~ sh
app/Main.hs:2:8: error: [GHC-39999]
    • No instance for ‘Show ([a0] -> a0)’ arising from a use of ‘print’
        (maybe you haven't applied a function to enough arguments?)
    • In the expression: print head
      In an equation for ‘main’: main = print head
  |
2 | main = print head
  |        ^^^^^
~~~

`(maybe you haven't applied a function to enough arguments?)` という GHC のヒントが非常に重要. **関数本体ではなく, 結果を渡したかった** のではないか? と聞いてくれている.

修正: `head` に引数 (リスト) を与える.

~~~ haskell
main = print (head [1, 2, 3])
~~~

### 4. `Parse error` (構文エラー)

文法として認識できない. **インデント不足** や **括弧の不整合** が代表的.

ソース (do 内のインデント忘れ):

~~~ haskell
main :: IO ()
main = do
putStrLn "hello"
~~~

実エラー出力:

~~~ sh
app/Main.hs:3:1: error:
    Parse error: module header, import declaration
    or top-level declaration expected.
  |
3 | putStrLn "hello"
  | ^^^^^^^^^^^^^^^^
~~~

`do` の本体は **1 つ深くインデント** する必要があります.

修正:

~~~ haskell
main :: IO ()
main = do
    putStrLn "hello"
~~~

### 5. `Non-exhaustive patterns` (網羅されていないパターン, 実行時)

パターンマッチで **すべての場合がカバーされていない** とき, コンパイルは通っても (警告は出る) **実行時** に該当しない値が渡されると停止します. fp3 で見せた `(x, y) = (1, 2)` のような **let パターン束縛** でも同じ事が起きます.

ソース:

~~~ haskell
main :: IO ()
main = do
    let (c:_) = "" :: String
    putStrLn [c]
~~~

実行時メッセージ (`stack run` 等で):

~~~ sh
probe: app/Main.hs:3:9-28: Non-exhaustive patterns in c : _
~~~

`(c:_)` は「先頭の 1 文字 + 残り」というパターンだが, 右辺が **空文字列** だと該当する文字が無いため失敗する.

修正 1: 空でないことが確実な値を与える.

~~~ haskell
main = do
    let (c:_) = "hello" :: String
    putStrLn [c]       -- "h"
~~~

修正 2: 事前に空判定する.

~~~ haskell
main = do
    let xs = "" :: String
    if null xs then putStrLn "empty"
               else putStrLn [head xs]
~~~

::: warn
類似に **`head []` などの空リスト操作** があります. これも実行時エラー:

~~~ sh
probe: Prelude.head: empty list
CallStack (from HasCallStack):
  ...
  head, called at app/Main.hs:... in main:Main
~~~

リスト操作前に `null` で検査するのが安全です. 後の章では `Maybe` 型でこの種の失敗を **型で表現する** 手法を扱います.
:::

## エラー読解のコツ

1. **最初の error: 行だけ見る**. Stack のビルドエラーは複数ブロック表示しますが, 修正すべきは **一番上の error:** がほとんどです.
2. **エラーコード (`GHC-xxxxx`) を覚える**. 同じコードは同じ種類の原因.
3. **`^^^^` の位置を必ず見る**. 行番号だけでなく列までヒントが出ています.
4. **`expected` / `actual` の型は逆ではない**. 「期待していたのは expected, 来てしまったのが actual」.
5. **ghci で `:t <式>`** で局所的に型を確認すると, 型不一致の原因が見えやすい.
6. **小さく書いて頻繁にビルド**. 100 行書いてからビルドするのではなく, 数行書いたら `stack build`. エラーが出る場所がすぐ分かります.

## 演習

ここからは fp1〜fp3 の復習を兼ねた **エラー解決演習** です. 各問題で:

1. 提示されたコードと **実際の GHC エラー出力** を読む
2. エラーの原因を答える
3. 動くコードに修正する

提出は `ch4-K.hs` (修正後の動作するコード) に加え, **何が原因だったか** を 1 〜 2 行のコメントで書いてください.

::: note

### Exercise CH4-1

**未定義変数のエラー (fp3 数値型の復習)**

以下のコードは fp3 「数値型」節で扱った三角形面積の計算ですが, コンパイルが通りません. 出力されるエラーを読んで原因を答え, 動くように修正してください.

~~~ haskell
-- ch4-1.hs (誤りあり)
trianglearea :: Double
trianglearea = 5 * 4 / 2

main :: IO ()
main = print triangleArea
~~~

実エラー (`stack build` の出力):

~~~ sh
app/Main.hs:5:15: error: [GHC-88464]
    Variable not in scope: triangleArea :: Double
    Suggested fix: Perhaps use ‘trianglearea’ (line 2)
  |
5 | main = print triangleArea
  |              ^^^^^^^^^^^^
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

**原因**: 変数名の **大文字小文字** が定義 (`trianglearea`) と呼び出し (`triangleArea`) で食い違っている. Haskell は大文字小文字を区別する.

GHC が `Suggested fix: Perhaps use ‘trianglearea’` というヒントまで出してくれているので, それを参考に揃える.

修正:

~~~ haskell
-- ch4-1.hs (修正版)
triangleArea :: Double
triangleArea = 5 * 4 / 2

main :: IO ()
main = print triangleArea   -- 10.0
~~~

呼び出し側を揃えるか定義側を揃えるかどちらでも OK. **キャメルケース (`triangleArea`) が Haskell の慣習**.

</details>

:::

::: note

### Exercise CH4-2

**型不一致のエラー (fp3 リストと数値の復習)**

以下のコードは fp3 「リスト」節で扱ったリスト連結 (`++`) を使っていますが, 型エラーが出ます. 原因を答えて修正してください.

~~~ haskell
-- ch4-2.hs (誤りあり)
main :: IO ()
main = print ([1, 2, 3 :: Int] ++ 5)
~~~

実エラー:

~~~ sh
app/Main.hs:2:35: error: [GHC-39999]
    • No instance for ‘Num [Int]’ arising from the literal ‘5’
    • In the second argument of ‘(++)’, namely ‘5’
      In the first argument of ‘print’, namely ‘([1, 2, 3 :: Int] ++ 5)’
      In the expression: print ([1, 2, 3 :: Int] ++ 5)
  |
2 | main = print ([1, 2, 3 :: Int] ++ 5)
  |                                   ^
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

**原因**: `(++)` は **リスト同士** を連結する演算子 (`[a] -> [a] -> [a]`) なのに, 右辺の `5` は **`Num` 型 (`Int` などの数値)** で, リストではない. そのため GHC は「`5` も `[Int]` 型 (= `Num [Int]`) であってほしいが, `Num` のインスタンスは `[Int]` には無い」と訴えている.

修正 1: 数値をリストに包んで連結する.

~~~ haskell
main :: IO ()
main = print ([1, 2, 3 :: Int] ++ [5])   -- [1,2,3,5]
~~~

修正 2: cons 構築子 `(:)` で先頭に足す.

~~~ haskell
main :: IO ()
main = print (5 : [1, 2, 3 :: Int])   -- [5,1,2,3]
~~~

</details>

:::

::: note

### Exercise CH4-3

**Parse error と論理演算 (fp3 論理型の復習)**

以下は fp3 「論理型」節で見た偶数判定を `do` の中で実行しようとしたコードです. 構文エラーで止まります. 原因を答えて修正してください.

~~~ haskell
-- ch4-3.hs (誤りあり)
main :: IO ()
main = do
print (101 `mod` 2 == 0)
print (202 `mod` 2 == 0)
~~~

実エラー:

~~~ sh
app/Main.hs:4:1: error:
    Parse error: module header, import declaration
    or top-level declaration expected.
  |
4 | print (101 `mod` 2 == 0)
  | ^^^^^^^^^^^^^^^^^^^^^^^^
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

**原因**: `do` ブロック内の文が **インデントされていない**. Haskell は **オフサイド規則 (offside rule)** によりインデントで構造を判定するため, `do` の本体は `do` より深い (= 右側に位置する) 必要がある.

GHC は「トップレベル宣言が来るはずだった」と判断してしまい, `print` で始まる行を **新しい関数定義として読もうとして失敗** している. これが「`Parse error: ... top-level declaration expected`」の正体.

修正:

~~~ haskell
-- ch4-3.hs (修正版)
main :: IO ()
main = do
    print (101 `mod` 2 == 0)   -- False (奇数)
    print (202 `mod` 2 == 0)   -- True  (偶数)
~~~

`do` の本体は **同じ列に揃えて** インデントする. 4 スペースが慣習だが揃っていれば 2 でも 6 でも OK.

</details>

:::

::: note

### Exercise CH4-4

**型クラス制約のエラー (fp3 数値型の混在)**

以下のコードは整数と小数を混ぜて使っていますが, 型エラーが出ます. 原因を答えて修正してください.

~~~ haskell
-- ch4-4.hs (誤りあり)
main :: IO ()
main = do
    let bmi = 60 / (1.75 * 1.75) :: Int
    print bmi
~~~

実エラー:

~~~ sh
app/Main.hs:4:25: error: [GHC-39999]
    • No instance for ‘Fractional Int’ arising from the literal ‘1.75’
    • In the first argument of ‘(*)’, namely ‘1.75’
      In the second argument of ‘(/)’, namely ‘(1.75 * 1.75)’
      In the expression: 60 / (1.75 * 1.75) :: Int
  |
4 |     let bmi = 60 / (1.75 * 1.75) :: Int
  |                     ^^^^
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

**原因**: 結果を `:: Int` と型注釈で指定しているが, 式の中に小数リテラル `1.75` が混ざっている. GHC は「結果が `Int` ならすべての要素も `Int` のはず → `1.75` も `Int` であってほしい → でも `1.75` は `Fractional` 制約が要る → `Int` には `Fractional` のインスタンスが無い」と推論して `No instance for ‘Fractional Int’` を出している.

修正: BMI は小数になるので **`Double` で扱う**.

~~~ haskell
-- ch4-4.hs (修正版)
main :: IO ()
main = do
    let bmi = 60 / (1.75 * 1.75) :: Double
    print bmi   -- 19.591836734693878
~~~

もし整数結果が欲しい場合は `floor` などで切り捨てる: `print (floor bmi :: Int)`.

</details>

:::

::: note

### Exercise CH4-5

**実行時エラー: 網羅されていないパターン (fp3 文字列とリストの復習)**

以下のコードはコンパイルは通りますが, 実行すると途中で止まります. 出力されるエラーを読んで原因を答え, 修正してください.

~~~ haskell
-- ch4-5.hs (誤りあり)
main :: IO ()
main = do
    let (c:_) = "hello" :: String
    putStrLn [c]               -- 'h'
    let (d:_) = "" :: String
    putStrLn [d]               -- 実行時に止まる
~~~

実行時出力 (`stack run` 等で):

~~~ sh
h
ch4-5: app/Main.hs:5:9-28: Non-exhaustive patterns in d : _
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

**原因**: `let (d:_) = ""` は「先頭 1 文字 + 残り」というパターン束縛だが, 右辺が **空文字列** で先頭に該当する文字が無いため失敗する. パターン束縛も「網羅されていないパターン」の一種.

実は **コンパイル時にも警告が出ている** はず (`Pattern match(es) are non-exhaustive` の warning). `stack build` のメッセージをよく見ると気づける.

修正 1: 事前に **`null` で空判定** する.

~~~ haskell
-- ch4-5.hs (修正版 A)
main :: IO ()
main = do
    let xs = "hello" :: String
    if null xs then putStrLn "empty"
               else putStrLn [head xs]

    let ys = "" :: String
    if null ys then putStrLn "empty"
               else putStrLn [head ys]
~~~

修正 2: パターン束縛ではなく `head` (こちらも空文字列だと実行時エラーだが, 検査と組み合わせる前提).

~~~ haskell
main = do
    let xs = "hello" :: String
    putStrLn (if null xs then "" else [head xs])
~~~

**安全な扱いとして** 後の章で `Maybe` 型を使い「失敗しうる」ことを **型で表現する** 手法を学びます (例: `listToMaybe`).

</details>

:::
