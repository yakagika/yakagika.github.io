---
title: プログラミング用の設定
description: VSCode・CLI・基本コマンドなど, 講義共通の環境設定
tags:
    - lecture
featured: false
date: 2024-10-18
tableOfContents: true
nextChapter: git.html
---

# プログラミング用の設定

この設定は複数の講義で共通の内容です. 各講義の言語固有のセットアップ (Python, Haskell など) については, それぞれの講義ページを参照してください.

::: note
- [プログラミング基礎 1 Pythonと環境構築](python1.html)
- [関数型プログラミング Haskell セットアップ](fp2.html)
:::

この資料の後に, 複数の講義で共通に使う次の 2 つを順に読みます.

- [共通資料 バージョン管理とGitHub](git.html): git と GitHub の導入, 変更の確認と記録, 公開してよいものの判断
- [共通資料 コーディングエージェントの利用](agent.html): codex と herdr の導入と操作, `AGENTS.md` と skill, 分からないまま承認しないための手順

## ソフトウェアの管理

**パッケージマネージャ**は, ソフトウェアのインストールと更新をコマンドで管理する仕組みです. Web サイトからインストーラーを個別に探す方法と比べて, 同じ名前のソフトウェアを同じ手順で導入できます.

本講義で使うソフトウェアは, 個別に別の手順を指定したものを除き, Windows では **winget**, macOS では **Homebrew** で管理します.

### Windows: winget

winget は Microsoft が提供する Windows のパッケージマネージャです. Windows 11 には標準で含まれています. PowerShell で次を実行し, バージョンが表示されることを確認します.

~~~ powershell
winget --version
~~~

`winget` が認識されない場合は, Microsoft Store で「アプリ インストーラー」(App Installer) をインストールまたは更新し, PowerShell を開き直します.

### macOS: Homebrew

Homebrew は macOS のパッケージマネージャです. macOS には最初から入っていないため, 未導入の場合は [Homebrew の公式サイト](https://brew.sh/index_ja)にある手順でインストールします. インストール後に次を実行し, バージョンが表示されることを確認します.

~~~ bash
brew --version
~~~

Homebrew のインストール完了時に, `brew` を利用できるようにするための追加コマンドが表示されることがあります. 表示された場合は, そのコマンドも実行してからターミナルを開き直します.

## テキストエディタのインストール

テキストエディタとは, プログラムを書くためのソフトウェアです.
プログラムを書くことをコーディング (Coding) といいます.

テキストエディタにはたくさんの種類があり, それぞれ独自の機能を持っています.
Windows に最初から入っている「メモ帳」もテキストエディタですが, プログラムを書くために様々な機能が追加された高機能なテキストエディタもたくさんあります.

例えば, シンタックスハイライト機能は, 以下のプログラムのように, プログラムの記述を役割や意味に応じて色付けして見やすくしてくれます.

~~~ python
## シンタックスハイライト
from datetime import datetime

def greet_based_on_time():
    now = datetime.now()
    current_hour = now.hour

    if 5 <= current_hour < 12:
        greeting = "Good morning, world!"
    elif 12 <= current_hour < 18:
        greeting = "Good afternoon, world!"
    else:
        greeting = "Good night, world!"

    return greeting

# 関数を呼び出して結果を表示
print(greet_based_on_time())
~~~

また, スペースをタブに変換するなどの機能もあります.

この資料では世界的に人気のある Microsoft の開発したテキストエディタである **VSCode (Visual Studio Code)** を利用します. 最近では生成 AI を利用した自動補完機能が付いた **Cursor (有料)** などもあります. AI 利用法は[共通資料 コーディングエージェントの利用](agent.html)で扱います. Cursor を既に利用している場合は, そちらを使っても構いません. それ以外のテキストエディタを使う場合は, 必要な設定を各自で行ってください.

Windows は PowerShell で次を実行します.

~~~ powershell
winget install -e --id Microsoft.VisualStudioCode
~~~

macOS はターミナルで次を実行します.

~~~ bash
brew install --cask visual-studio-code
~~~

インストールが終了したら VSCode を起動します. サインインを求められますが, ここでは「Continue without Signing In」を選択してサインインせずに進めます.

![VSCode Sign In](/images/common/vscode-sign-in.png)

表示モードは好きなものを選択してください.
![VSCode Mode](/images/common/vscode-mode.png)

拡張機能はこのあと入れるので Skip してください.
![VSCode Extensions](/images/common/vscode-extensions.png)


VSCode は様々な拡張機能があり, 利用しやすいようにカスタマイズすることが可能です.

::: warn
その他の便利な拡張機能等に関しては自己責任で調べて導入してください.
:::

左側にある四角が 4 つ並んだアイコンを選択します.

![VSCode Install Extensions](/images/common/vscode-install-extensions.png)

::: note
**拡張機能: データサイエンス実践**

検索窓に `Python` と入力して `Python` の `install` を押します.

![VSCode Install Python](/images/common/vscode-install-python.png)

検索窓に `latex` と入力して `LaTeX Workshop` の `install` を押します.

![VSCode Install LaTeX Workshop](/images/common/vscode-install-latexworkshop.png)
:::

::: note
**拡張機能: 関数型プログラミング**

検索窓に `Haskell` と入力して `Haskell Syntax Highlighting` の `install` を押します.

![VSCode Install Haskell Syntax](/images/common/vscode-install-haskell-syntax.png)
:::

これで基本的な設定は完了です.

ファイルを編集する際には, 左側のファイルアイコンをクリックして, プログラムの保存されているフォルダを選択します.

![VSCode Files](/images/common/vscode-files.png)

ディレクトリが表示されるので, 編集したいファイルをクリックすることで編集が可能となります.
![VSCode Edit](/images/common/vscode-edit.png)

その他細かな利用法に関しては, 今後実際に利用する際に説明します. また, 基本的な操作やショートカット等に関しては, 各自で調べてみてください.

## IME の設定

プログラムは基本的に **「半角英数字」** で記述されます. プログラム中に全角の空白や記号が混じるとエラーの原因となる場合があります. そのため, プログラムを書く前に, そういったミスが起きないように IME の設定をしましょう.

タスクトレーから IME の設定ができます. 基本的に記号をすべて半角に設定しましょう (スペースは必ず半角にしましょう). 特に, 句読点をコンマとピリオドに変更しましょう.

![IME](/images/common/ime.png)


## CLI の基本操作

プログラムの開発環境にはマウスなどでクリックして操作する GUI (Graphical User Interface) をもった IDE (Integrated Development Environment) などもありますが, 基本的には文字によってコンピュータに命令を送る CLI (Command Line Interface) を利用します. 映画やマンガなどで, ハッカーが黒い画面に文字を打ち込んでいる場面の, あの画面です.

コンピュータのオペレーティングシステムとユーザ間の CLI を提供するプログラムを Shell といい, Windows では, Command Prompt や PowerShell などがあります. Mac などの Unix 系では, Bash や zsh があります. これらは端末ソフトウェアを介して利用します. Windows では Windows Terminal, macOS ではターミナル (Terminal.app) です.

開発に使う環境は好みで選んで構いません. 以下では PowerShell での操作を示します.

Windows 11 の検索バーで `Terminal` と検索して, 出てきた `Terminal` をクリックしましょう.

![Screenshot Terminal](/images/common/terminal-launch.png)

自動的に `Windows PowerShell` が起動します. 立ち上がった黒色の画面に文字でコマンド (命令) を入力して, コンピュータを操作します.

![Screenshot Terminal](/images/common/terminal-window.png)


### エンコーディング

実際にコマンドを入力する前に, 初心者がつまずきやすいポイントとして, Windows のエンコーディングについて解説します.

PC は文字をそのまま扱えず, 内部ではすべて数値 (バイト列) として記録します. そこで, 1 つ 1 つの文字をどのバイト列で表すかという対応の規則を決めておきます. この規則を文字エンコーディングと呼びます. 同じ文字でもエンコーディングが違えばバイト列が変わるので, 書いた側と読む側でエンコーディングが食い違うと文字化けが起きます.

エンコーディングには複数の種類があります (日本語設定の Windows は Shift-JIS, Unix 系は Unicode が一般的です).
Python は UTF-8 という文字エンコーディングがデフォルトなので, Windows においても可能な限り UTF-8 を用いた方が良いです.

そこで, ターミナル上で利用するエンコーディングを変更します.
PowerShell を起動して, `chcp 65001` と打ち込み, PowerShell 上で利用する文字エンコーディングを UTF-8 に変更しましょう. `chcp` が利用する文字コードを変更するコマンド (change code page) で, その後に変更したい文字コードを入力します. `65001` は, Windows がエンコーディングに割り振っている番号 (コードページ) のうち, `UTF-8` を表す番号です. これは PowerShell を起動するたびに行ってください.

~~~ sh
chcp 65001
~~~

と入力すると,

~~~ sh
Active code page: 65001
PS C:\Users\user>
~~~
のように表示されるはずです.

### 日本語表示

PowerShell の設定によっては日本語が表示されず, 日本語部分が `□` で置き換えられて表示されます. これは, 使用しているフォントに日本語が含まれていないために発生します.


![Screenshot PowerShell](/images/common/powershell-tofu.png)


設定を変更して日本語を表示できるようにしましょう (適当な日本語を入力してみて, 問題なく表示されるようであれば, 変更は必要ありません).

左上の `下向きの矢印 > 既定値 > 外観 > フォントフェイス` の部分を日本語フォントに変更し `保存` をクリックすることで, 日本語が表示されるようになります.

![Screenshot Terminal](/images/common/terminal-setting1.png)

![Screenshot Terminal](/images/common/terminal-setting2.png)

![Screenshot Terminal](/images/common/terminal-setting3.png)

その他色やサイズなど, 好きな設定に変更できます. あとで, 好みにカスタマイズしましょう.

### ディレクトリ

基礎的なコマンドを学ぶ前に, ディレクトリに関して理解しておきましょう.
コンピュータの中のデータは, 以下のような木構造になっています. ファイルを分類して入れておく入れ物 (フォルダ) を**ディレクトリ**といい, ディレクトリの中にさらにディレクトリを入れられるので, 全体が木の形になります. この全体をディレクトリ構造と呼びます.

~~~
C: -- Users -- hoge
          |
            -- hoge2 -- Desktop
                   |
                     -- Downloads
                   |
                     -- Documents -- huga
                                |
                                  -- huga2
~~~


CLI において, ユーザはこの木構造のどこかに存在しており, この木構造を移動しながら様々な作業を行います. 現在いるディレクトリのことを `working directory`(以下 wd) や `current directory` といいます.


## 基礎的なコマンド

プログラミングで最低限必要になるコマンドを, ディレクトリの移動に関するものを中心に学習します.

wd は, CLI の左側に表示されていることが多いです.

~~~ sh
PS C:/Users/hoge2>
~~~

のように表示されていれば今 `C:` ドライブ下の `Users` 下の `hoge2` が wd となります.

::: warn

- Windows では, ディレクトリを区切る文字が `¥` あるいは `\` で表示されていると思います.

- Mac では, `/` です.

本資料では, `/` を利用しています. 自分の環境に合わせて適宜読み替えてください.
:::

CLI の左側に表示されていない場合にも `pwd` コマンド (print working directory) を入力すると, 現在のディレクトリが表示されます.

~~~ sh
PS C:/Users/hoge2> pwd

PATH
----
C:/Users/hoge2
~~~

wd の下に何があるかを調べるコマンドとして `ls` コマンド (list) があります.

::: warn
以下, `PS C:\Users\hoge2` の部分は省略します.
:::

~~~ sh
> ls

Desktop
Downloads
Documents
~~~

※実際の画面では, もう少しいろいろな情報が書かれているかと思います.

ディレクトリ構造を確認するコマンドとして `tree` があります. `tree` と入力して Enter キーを押すと, wd 以下のディレクトリ構成が確認できます.

~~~ sh
> tree
Folder PATH listing
Volume serial number is 00000157 B8F4:6480
C:.
├───Contacts
├───Desktop
├───Documents
│   ├───hoge
│   └───slds
│       └───program
├───Downloads
~~~

`tree [PATH]` と入力すると, wd ではなく指定した `[PATH]` 以下のディレクトリ構造が表示されます.
また, `/f` オプションを加えることでファイルも表示されます.

~~~ sh
> tree .\Documents\ /f
Folder PATH listing
Volume serial number is 000001D1 B8F4:6480
C:\USERS\AKAGI\DOCUMENTS
├───hoge
│       hello.py
│       slds-2-10.py
│
└───slds
    └───program
            hello.py
~~~

::: warn

macOS には `tree` コマンドが最初から入っていないため, Homebrew でインストールします.

~~~ bash
brew install tree
~~~

また, オプションも Windows とは異なっています.

|コマンド| 意味 |
| :---: | :---: |
| -d | ディレクトリのみ表示 |
| -L N | N 階層まで表示 |
| -P X | 正規表現 X に従って表示 |

~~~ sh
> tree
.
├── hoge
│      └── hoge.py
└── huga
        └── huga.py

3 directories, 2 files
> tree -d
.
├── hoge
└── huga

3 directories
> tree -L 1
.
├── hoge
└── huga

3 directories, 0 files
> tree -P "hoge*"
.
├── hoge
│     └── hoge.py
└── huga

~~~

:::


wd から別のディレクトリに移動するコマンドとして `cd` コマンド (change directory) があります.

`cd [移動先]` と打つことで, `ls` コマンドで出てきたディレクトリに移動できます.

~~~ sh
> ls
Desktop
Downloads
Documents

> cd Documents
> pwd
PS C:/Users/hoge2/Documents
~~~

::: warn

移動先のディレクトリ名はすべて自分で入力する必要はありません. 最初の数文字を入力して `Tab` キーを押すと, 自動で補完されます.

:::


`cd ..` と打つと一つ上 (親) のディレクトリ, `cd ~` と打つとホームディレクトリ (基本的には最初に開いた際にいた場所) に一挙に移動することができます.

~~~ sh
> pwd
PS C:/Users/hoge2/Documents
> cd ..
> pwd
PS C:/Users/hoge2
> cd Documents/huga
> pwd
PS C:/Users/hoge2/Documents/huga
> cd ~
> pwd
PS C:/Users/hoge2
~~~

`mkdir [作りたいディレクトリ名]` コマンド (make directory) で, 新しいディレクトリを作成できます.

`rmdir [消したいディレクトリ名]` コマンド (remove directory) で, ディレクトリを消すことができます.

~~~ sh
> pwd
PS C:/Users/hoge2
> ls
Desktop
Downloads
Documents
> cd Documents
> ls
huga
huga2
> mkdir huga3
> ls
huga
huga2
huga3
> rmdir huga3
> ls
huga
huga2
~~~

`rmdir` は空のディレクトリを消すためのコマンドです. 中身ごと消す方法は次の `rm` の節で扱いますが, 事故が起きやすいので慣れるまでは使いません.

`touch [作りたいファイル名]` コマンドで空のファイルを作成できます. テキストエディタを開く前に, CLI からファイルだけ先に用意しておきたいときに便利です.

`rm [消したいファイル名]` コマンド (remove) でファイルを削除できます. `rmdir` がディレクトリ用であるのに対し, `rm` はファイル用です.

::: warn
`rm` で削除したファイルはゴミ箱に入らず, 基本的に復元できません. 実行前に対象が正しいかを必ず確認しましょう. なお `rm -r [ディレクトリ名]` とするとディレクトリを中身ごと削除できますが, 誤って重要なファイルを消してしまう事故が起きやすいので, 慣れるまでは使わない方が安全です.
:::

~~~ sh
> ls
huga
huga2
> touch memo.txt
> ls
huga
huga2
memo.txt
> rm memo.txt
> ls
huga
huga2
~~~

### プログラムの中断

CLI 上でプログラムを実行している最中に止めたくなることがあります (例えば無限ループに入ってしまった場合など). そのようなときは `Ctrl + C` (Ctrl キーを押しながら C キー) を入力することで, 実行中のプログラムを強制的に中断できます. これから何度も使うので覚えておきましょう.

::: note
**練習: 作業用ディレクトリを作ろう**

これから, 作業をするためのディレクトリをコマンドで作成しましょう.

- (**OneDrive, iCloud などのクラウドストレージではない**) Documents に移動
- Programs というディレクトリを作成し移動

以降は受講している講義に応じてディレクトリを作成してください (名前は好きに設定して良いです).
:::

::: note
**データサイエンス実践の場合**

- Python というディレクトリを作成し移動
- slds というディレクトリを作成し移動
  (slds; special lecture data science)

これからこの講義で利用するプログラムなどは slds に保存しましょう.

→ [プログラミング基礎 1 Pythonと環境構築へ進む](python1.html)
:::

::: note
**関数型プログラミングの場合**

- Haskell というディレクトリを作成し移動
- functional_programing というディレクトリを作成し移動

これから関数型プログラミング講義で利用するプログラムなどは functional_programing に保存しましょう.

→ [関数型プログラミング Haskell セットアップへ進む](fp2.html)
:::
