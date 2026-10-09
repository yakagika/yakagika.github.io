---
title: 共通資料 バージョン管理とGitHub
description: 資料
tags:
    - programming
    - lecture
featured: false
date: 2026-09-04
open: true
tableOfContents: true
previousChapter: setup.html
nextChapter: agent.html
---

本資料は複数の講義で共通に使う資料です. コーディングエージェントに書かせた変更を確認し, 作業を記録し, 何を公開してよいかを判断するために必要な範囲の git と GitHub を扱います.

エージェント自体の導入と操作は[共通資料 コーディングエージェントの利用](agent.html)で扱います.

この資料は, git と GitHub を自分の手で一度使ってみるための章です. 次の資料では, 差分の確認から commit と push までを codex に頼んで実行させます. そのとき画面に表示されるコマンドの意味を言えるように, ここで一度は自分で打っておいてください. 使いこなせるようになることは, 初日の目標ではありません.

::: warn

**この資料の情報は, 短い期間で古くなります.** GitHub の画面, 認証の方法, 料金は変わります. 資料と実際の画面が食い違ったら, 公式のヘルプで最新の情報を確かめてください.

:::

# なぜエージェントと git を一緒に使うのか

git は, ソフトウェアの**バージョン管理**の仕組みです. 変更を記録し, 記録どうしを比べ, 過去の記録の状態へ戻すために作られました. コーディングエージェントを使うと, 次の 3 つの場面でこれらの働きが必要になります.

## 変更の差分を読む

エージェントは, 1 回の指示で複数のファイルを書き換え, 既存の行を消すことがあります. 書き換え後のファイルだけでは, どの行が足されどの行が消えたのかが分かりません. git は記録どうしの差分を取れるので, エージェントが何をしたかを行単位で確認できます.

## 戻る場所を作る

「さっきの状態に戻して」と頼んでも, 戻るとは限りません. エージェントは, 書き換える前のファイルの中身を保持していません. 会話を再開する機能 (`/resume`) で戻るのは会話であって, ファイルの中身ではありません.

エージェントに作業させる前に commit しておけば, その状態まで戻れます. 戻れる場所を作れるのは自分だけです.

## 結果を出した版の同定

分析のコードを直すと出力も変わります. 発表資料に載せた図が, どの時点のコードとどの時点のデータから出たものかを後から言えないと, 質疑に答えられません. commit には日時とメッセージが残り, `git log` で一覧できるので, どの記録の状態で出した図かを示せます.

::: note

エージェントを複数同時に動かすと, 全員が同じディレクトリを触って, 後から書いたエージェントが先の変更を上書きします. 上書きされた側は, 自分の変更が消えたことを検知しません. git は branch で記録を枝分かれさせ, worktree で作業する場所を分けて, この上書きを防ぎます. 分けた作業を合流させるときに, 同じ行を両方が変えていれば, git がコンフリクトとして止めます. この 2 つは[共通資料 コーディングエージェントの利用](agent.html)の発展節で扱います.

:::

::: note

本資料が扱うのは, エージェントを使ううえで必要になる範囲と, 教員と同じリポジトリを共同編集するための `pull` と `merge` までです. 複数人での開発で使う機能 (pull request, レビュー, マージ戦略) までは踏み込みません.

:::

# リポジトリを置くディレクトリ

作業用のディレクトリを 1 つ作ります. 置き場所を間違えると, 後から直すのが面倒です.

~~~ bash
mkdir ~/work
~~~

Windows の PowerShell でも `~` はホームディレクトリを指すので, 同じコマンドで作れます.

::: warn

**OneDrive, iCloud Drive, Google Drive, Dropbox の同期対象の中にリポジトリを置かないでください.**

git はリポジトリの状態を `.git` という隠しディレクトリで管理しています. git はこの中のファイルを短時間のロックを取りながら書き換えますが, 同期サービスは書き換えの途中のファイルを掴むので, git が失敗したり記録が壊れたりします. 複数の端末で同じディレクトリを同期していると, 両方の `.git` が混ざってリポジトリ自体が読めなくなることもあります.

**Windows では, デスクトップと Documents が OneDrive の管理下に入っていることがあります.** 初期設定でそうなっている場合があるので, 置く前に確認してください. macOS も `~/Documents` と `~/Desktop` が iCloud Drive の対象になっていることがあります. どちらも `~/work` のように, 同期の対象外へ新しく作るのが確実です.

GitHub へ push してあれば, 端末が壊れても `clone` し直せます. リポジトリをクラウド同期に入れる必要はありません.

:::

::: note

同期の対象の中に置くと, 次のことが起きます.

- git がロックを取れず `Unable to create '.git/index.lock'` で失敗する
- 同期サービスが `analysis 2.py` のような重複ファイルを作り, git がそれを新しいファイルとして扱う
- 同期がタイムスタンプを書き換え, 変更していないファイルが変更済みとして並ぶ
- OneDrive の「ファイル オンデマンド」や iCloud Drive の「ストレージを最適化」が `.git` の中身を端末から退避させ, オフラインのときに git が読めなくなる

:::

# git と GitHub の役割

**git** は手元のパソコンで動くプログラムです. ファイルの状態を記録し, 差分を取り, 過去の記録へ戻します. インターネットに接続していなくても動きます.

**GitHub** は git の記録をインターネット上に置いておくサービスです. Microsoft が運営しています. 手元の記録を GitHub に送っておけば, パソコンが壊れても失われませんし, 他人に見せることもできます.

git は GitHub が無くても使えます. GitHub にもブラウザ上でファイルを編集する機能はありますが, エージェントは手元のファイルを書き換えるので, この講義では手元の git を使います.

同じような役割のサービスに GitLab や Bitbucket があります.

この講義では, 各自の GitHub アカウントで**リポジトリ** (記録の置き場所, repository, 略して repo) を 1 つ作り, そこに作業を記録していきます.

# git のインストール

OS ごとに手順が違います. 自分の OS の手順だけ実行してください.

## Windows

Windows には **winget** というコマンドが用意されています. Microsoft が提供しているソフトウェアの導入と更新の仕組みで, macOS の Homebrew に相当します. Windows 11 には最初から入っています.

[共通資料 プログラミング用の設定](setup.html)で扱った PowerShell を開いて, 次の 2 つを実行します.

~~~ powershell
winget install -e --id Git.Git
winget install -e --id GitHub.cli
~~~

2 つ目は GitHub CLI で, 後の認証で使います.

どちらも途中で「このアプリがデバイスに変更を加えることを許可しますか?」という確認 (ユーザー アカウント制御) が出ます. 発行元が Git for Windows は Johannes Schindelin, GitHub CLI は GitHub, Inc. であることを確かめてから「はい」を押します.

インストール後は **PowerShell を一度閉じて開き直してください.** 起動中のウィンドウには新しいコマンドの場所が反映されません.

::: note

`winget` が認識されないと出た場合, 「アプリ インストーラー」(App Installer) が入っていないか, 導入直後で登録が終わっていません. Microsoft Store で「アプリ インストーラー」を検索して入れ, PowerShell を開き直してください. Windows 10 は 1809 より前のバージョンでは winget を使えないので, その場合は git-scm.com のインストーラを使います.

:::

## macOS

**Homebrew** で入れます. Homebrew は macOS のソフトウェア導入の仕組みで, 入っていなければ [brew.sh](https://brew.sh/index_ja) の手順に従って先に入れてください.

~~~ bash
brew install git
brew install gh
~~~

2 つ目は GitHub CLI で, 後の認証で使います.

## インストールの確認

OS を問わず, 次を実行してバージョンが表示されれば成功です.

~~~ bash
git --version
gh --version
~~~

~~~ text
git version 2.51.0
gh version 2.100.0
~~~

## 名前とメールアドレスの設定

git は記録を残すときに, 誰が記録したかを一緒に書き込みます. 最初に一度だけ設定します. 自分の名前とアドレスに置き換えてください.

~~~ bash
git config --global user.name "自分の名前"
git config --global user.email "自分のアドレス"
~~~

ここで設定したメールアドレスは記録に含まれ, public なリポジトリでは誰でも読めます. 公開したくないアドレスは使わないでください.

::: note

GitHub が発行する `<数字>+<ユーザ名>@users.noreply.github.com` という, メールアドレスを公開せずに記録に使うためのアドレスもあります. GitHub の設定画面の Emails で確認できます.

:::

::: note

**既定のエディタの設定**

`git commit` を `-m` 無しで実行すると, メッセージを書くためにエディタが開きます. 既定では `vim` が開き, 使い方を知らないと終了できません. この資料では `-m` を付けて commit しますが, 教員と共同編集するときは, `git pull` が合流の記録のメッセージを書くためにエディタを開きます ([merge: 2 つの変更を合わせる](#merge-2-つの変更を合わせる)). VSCode を使うように変えておいてください.

~~~ bash
git config --global core.editor "code --wait"
~~~

設定前に `vim` が開いてしまったときは, `Esc` を押してから `:q!` と入力して `Enter` を押すと, 保存せずに閉じます.

:::

# public と private の違い

GitHub でリポジトリを作るとき, **public** (公開) と **private** (非公開) を選びます.

- **public**: URL を知っていれば誰でも中身を読めます. 検索エンジンにも載ります.
- **private**: 自分と, 自分が明示的に招待した人だけが読めます.

迷ったら private にしてください. 後から public に変えられます. 逆に, public にしたものを private に戻しても, 公開されていた間に誰かがコピーしたものは取り戻せません.

## public に置いてはいけないもの

次のものは, 誰も見ないだろうと考えたとしても public なリポジトリに入れられません.

**API キーと認証トークン**. 外部サービスを使うための鍵です. たとえば, 教員が契約したアカウントの認証トークンを配布する講義があります. その鍵は共有の前払い残高に直結しているので, 公開されると第三者が残高を使い切れます. GitHub 上の公開リポジトリは機械的に走査されており, 鍵が置かれると短時間で発見されます.

**個人情報を含むデータ**. アンケートの回答, 名簿, 成績など, 個人が特定できるデータです. 匿名化したつもりでも, 複数の項目を組み合わせると個人が特定できることがあります.

**提供を受けたデータ**. 企業や自治体から研究目的で提供されたデータには, 公開してよい範囲の取り決めがあります.

## private は公開範囲を狭めるだけ

private にすれば何でも置ける, とはなりません. private でも, データは GitHub というアメリカの事業者のサーバへ送られます. 個人情報や提供を受けたデータは, private であっても外部へ送ってよいかを先に確認してください. 提供元との取り決め, 大学の規程, 倫理審査で承認した利用範囲が判断の根拠になります.

判断がつかないデータは, 手元に置いたまま `.gitignore` で除外し, リポジトリには取得手順だけ書きます.

::: warn

**鍵とデータは, `.gitignore` で記録の対象から外します.** `.gitignore` はリポジトリの一番上に置くファイルで, 記録しないファイルの名前を 1 行に 1 つ書きます. 書かれたファイルを, git は記録の候補に入れません. 鍵はソースコードに直接書かず, `.env` という別のファイルに書いて, `.env` を `.gitignore` で除外します. 書き方は, リポジトリを手元に `clone` した後の[.gitignore に書く除外の設定](#gitignore-に書く除外の設定)で扱います.

:::

::: warn

`data/` と `.env` をリポジトリから除外し, クラウド同期からも外すと, **提供を受けたデータと鍵はどこにもバックアップされません.** 端末が壊れると失われます. 原本は外付けディスクや大学のストレージなど, リポジトリの外に別途保管してください.

:::

## 鍵を記録に入れてしまった場合

git は履歴を残すので, 一度記録した鍵はファイルから消しても履歴に残ります.

やるべきことは, ファイルを直すことではなく, **その鍵を無効にして発行し直すこと**です. 教員から配布された鍵なら教員に連絡してください. 履歴からの完全な削除は手順が複雑で, 既に誰かが取得していた場合には意味がありません.

# GitHub のアカウントとリポジトリ

## アカウントの作成

[github.com](https://github.com/) を開きます. 右上に Sign in と Sign up が並んでいるので, **Sign up** を押します.

![GitHub のトップページ. アカウントの作成は右上の Sign up から始める](/images/common/git/top.png)

Email, Password, Username, Country/Region を入力して Create account を押すと, 入力したアドレスに確認コードが届きます. Google や Apple のアカウントで作ることもできますが, どちらを選んでも後の手順は変わりません.

![Sign up の入力画面. 必要なのはアドレス, パスワード, ユーザ名, 国だけ](/images/common/git/signup.png)

Username は URL の一部になり (`https://github.com/<ユーザ名>`), 他人から見えます. 後から変更できますが, 変更すると以前の URL は無効になり, 手放した名前は他人が取得できます. 最初に決めてください.

## 二段階認証の設定

パスワードだけでは, どこかで漏れたパスワードを使い回されるとアカウントを乗っ取られます. GitHub は 2023 年以降, コードを提供する利用者に段階的に二段階認証を必須化しており, 対象になると設定するまで操作が制限されます. 対象になるのを待たず, 最初に設定してください.

右上のアイコンを押すとメニューが開きます. Settings に入ります.

![右上のアイコンから開くメニュー. Settings はこの中にある](/images/common/git/menu.png)

左のメニューで Password and authentication を選びます. Sign in methods の一覧が出ます.

![Settings の Password and authentication. 二段階認証の設定はこのページの下にある](/images/common/git/settings-auth.png)

同じページを下にたどると Two-factor authentication のセクションがあります. 右上が Enabled になっていれば設定済みです. 未設定なら, 認証アプリ (Google Authenticator など) かパスキーを登録します.

![Two-factor authentication が Enabled になった状態. ここが Enabled なら設定は済んでいる](/images/common/git/2fa.png)

::: warn

設定の途中で **recovery code (回復コード)** が表示されます. 認証アプリを入れた端末を失くしたときの唯一の復旧手段で, これを失うと GitHub 側でも復旧できません. 印刷するか, パスワード管理ソフトに保存してください.

:::

## リポジトリの作成

画面右上の `+` を押し, New repository を選びます.

![+ メニューの New repository](/images/common/git/new-repo-menu.png)

入力画面は 1 General と 2 Configuration の 2 段に分かれています.

![リポジトリ作成の入力画面. visibility を Private, Add README を On, .gitignore を Python にした状態](/images/common/git/new-repo.png)

上から順に次のようにします.

- **Repository name**: 英数字とハイフンで, 中身が分かる名前 (例: `ds-practice-2026`). 入力すると使えるかどうかがその場で表示されます. 図では動作確認のため `aaaaa` と入れていますが, 自分のものは意味の分かる名前にしてください
- **Description**: 1 行の説明 (任意)
- **Choose visibility**: 右のドロップダウンで Private を選びます
- **Add README**: 右のトグルを On にします. 空でないリポジトリができ, すぐ `clone` できます
- **Add .gitignore**: ドロップダウンから Python を選びます
- **Add license**: No license のままで構いません

Create repository を押すと作成されます.

## 教員を Collaborator に招待する

教員が private のリポジトリを読めるようにするには, 教員を **Collaborator** (共同編集者) として招待します. 招待した相手だけが, private のリポジトリの中身を読めます. 招待には教員の GitHub のアカウント名が要るので, 講義で案内されたものを手元に用意してください.

1. 作成したリポジトリのページを開き, リポジトリ名の下に並ぶタブから **Settings** を押します. 画面の幅が狭いとタブが畳まれるので, その場合は `...` のメニューの中から選びます.
2. 左のメニューの Access という見出しの下にある **Collaborators** を押します. パスワードか二段階認証のコードを求められたら入力します. 設定を変える操作の前に, GitHub が本人かどうかを確かめるためです.
3. Manage access の欄にある **Add people** を押します.

    ![Settings の Collaborators の画面. まだ誰も招待していないので, Manage access の欄の中央に Add people のボタンがある](/images/common/git/collab-settings.png)

4. 開いた入力欄に教員のアカウント名を入力し, 下に出る候補から教員のアカウントを選びます. 名前の似た別人のアカウントが候補に並ぶことがあるので, 講義で案内されたアカウント名と 1 文字ずつ一致しているかを確かめてから選んでください.

    ![Add people を押すと開く入力欄. 上端にはリポジトリ名が表示され (図では塗りつぶしています), 相手を選ぶまでは右下のボタンが Add to repository のまま押せない](/images/common/git/collab-add.png)

5. 相手を選ぶと, 右下のボタンの表示が `Add <アカウント名> to <リポジトリ名>` に変わります. このボタンを押します.

招待を送ると, Manage access の一覧に教員のアカウントが **Pending Invite** という表示つきで並びます. 教員にはメールで招待が届き, 教員が受け取ると Pending Invite の表示が消えます. この時点から, 教員はリポジトリの中身を読めます.

招待は送ってから 7 日で失効します. 失効したら, 一覧から古い招待を消し, 同じ手順で招待し直します.

::: note

個人のアカウントのリポジトリでは, Collaborator は読むだけでなく, 変更を commit して push することもできます. 読むだけの権限を選ぶ設定はありません. 教員は, この権限を使って原稿やプログラムに直接添削を入れることがあります. 教員が push した変更を手元に取り込む方法は, [共通資料 LaTeX による原稿作成](latex.html)の[教員の添削を取り込む](latex.html#教員の添削を取り込む)で扱います.

:::

## GitHub CLI での認証

`clone` や `push` のたびに GitHub は本人確認を求めます. 先に入れた GitHub CLI で一度だけ済ませます.

~~~ bash
gh auth login
~~~

対話形式で聞かれるので, 次のように答えます.

- Where do you use GitHub? → **GitHub.com**
- What is your preferred protocol for Git operations on this host? → **HTTPS**
- Authenticate Git with your GitHub credentials? → **Yes**
- How would you like to authenticate GitHub CLI? → **Login with a web browser**

`One-time code (XXXX-XXXX) copied to clipboard` と 8 文字のコードが表示されます. コードはクリップボードにも入っています. `Enter` を押すとブラウザが開くので, GitHub にサインインしていなければサインインします. 次の画面にコードを貼り付けて Continue を押し, 続く画面で Authorize github を押します.

![コードを入力する画面. 端末に表示されたコードを 8 つの枠に入れる](/images/common/git/credential.png)

端末に `Logged in as <自分のユーザ名>` と出れば認証は完了です.

::: warn

`gh auth login` を使わない場合, macOS では `clone` や `push` のときにターミナルで `Username` と `Password` を聞かれます. **ここで GitHub のログインパスワードを入れても通りません.** パスワードによる認証は 2021 年に廃止されており, personal access token を自分で発行して貼る必要があります. その手間を省くために GitHub CLI を使います.

:::

## clone: 手元にコピーする

作成したリポジトリのページで緑の Code ボタンを押すと, Clone の欄が開きます. HTTPS のタブを選び, 表示された URL を右のボタンでコピーします.

![Code ボタンを押した状態. HTTPS のタブの URL をコピーする](/images/common/git/code-button.png)

先に作った作業用ディレクトリへ移動してから実行します.

~~~ bash
cd ~/work
git clone https://github.com/<自分のユーザ名>/<リポジトリ名>.git
cd <リポジトリ名>
~~~

## clone したディレクトリの中身を確認する

`clone` したディレクトリに何が入っているかを, [共通資料 プログラミング用の設定](setup.html)で扱った `tree` で確認します.

macOS では次のように実行します.

~~~ bash
tree -a -L 1
~~~

~~~ text
.
├── .git
├── .gitignore
└── README.md

2 directories, 2 files
~~~

`-a` は名前が `.` で始まるファイルも表示する指定です. 付けないと `.git` と `.gitignore` が表示されません. `-L 1` は一番上の階層だけを表示する指定です. `.git` の中には多数のファイルがあるので, 階層を制限します.

Windows の PowerShell では `tree /f` を実行すると, `.gitignore` と `README.md` が表示されます. `.git` は隠しフォルダなので `tree` には表示されません. 隠しフォルダも含めて確認するには `ls -Force` を実行します.

エクスプローラーも, 既定の設定では `.git` のような隠しフォルダを表示しません. エクスプローラーの上部にある **表示** を押し, メニューの一番下の **表示** から **隠しファイル** にチェックを入れておくことを勧めます. `.git` が見えれば, そのディレクトリが git のリポジトリかどうかをエクスプローラーで見分けられます. macOS の Finder では `Cmd + Shift + .` で隠しファイルの表示を切り替えられます.

![エクスプローラーの表示のメニュー. 表示の中の表示から, 隠しファイルにチェックを入れる](/images/common/git/explorer-hidden-files.png)

`tree` で表示されたファイルとディレクトリは, それぞれ次のものです.

- **`README.md`**: リポジトリを作るときに Add README を On にしたので作られたファイルです. リポジトリの説明を書きます
- **`.gitignore`**: Add .gitignore で Python を選んだので作られたファイルです. 記録しないファイルを書きます. 書き方は次の節で扱います
- **`.git`**: git の記録の実体です. これまでの変更の履歴はすべてこの中にあります. 手で編集したり消したりしません. 消すと手元の履歴が失われます

VSCode でこのディレクトリを開くと, エクスプローラーに `.gitignore` と `README.md` が表示されます. `.git` は VSCode の既定の設定で表示されません.

# .gitignore に書く除外の設定

鍵をソースコードに直接書くと, 記録するときに一緒に入ります. 鍵は `.env` という別のファイルに書き, そのファイルを追跡の対象から外します. `.env` は必要になったときに自分でリポジトリの一番上に作るファイルで, `clone` した直後にはありません.

対象から外すには, リポジトリの一番上にある `.gitignore` に, 除外したいものを 1 行に 1 つ書きます. [clone したディレクトリの中身を確認する](#clone-したディレクトリの中身を確認する)で見たとおり, このファイルはリポジトリを作ったときに作られています. この講義で除外するものは次のとおりです.

~~~ text
# --- 認証情報 ---
.env
.env.local

# --- データ ---
data/
*.csv
*.xlsx
*.xls

# --- Python と uv が作るもの ---
.venv/
__pycache__/
*.pyc
.ipynb_checkpoints/

# --- OS が作るもの ---
.DS_Store
Thumbs.db
~~~

## パターンの読み方

- `/` を含まない行は, どの階層にも効きます. `*.csv` はリポジトリのどこにある CSV も除外し, `data/` は `src/data/` のような下位の `data` ディレクトリも除外します
- 末尾の `/` はディレクトリだけに一致します
- `*` は任意の文字列に一致しますが, `/` には一致しません. このため `data/*.csv` は `data/a.csv` に一致し, `data/sub/a.csv` には一致しません

## 各行の意図

**認証情報**. `.env` は鍵を書くためのファイルです. `.env.local` のような派生も作られるので併せて外します.

::: note

GitHub でリポジトリを作るときに Add .gitignore で Python を選ぶと, `.env` `.venv` `__pycache__` `.ipynb_checkpoints` は最初から入っています. 足りないのは `data/` と表計算ファイルなので, そこを追記します.

:::

**データ**. `data/` を丸ごと外したうえで, `*.csv` と `*.xlsx` も外します. 分析の途中で作った中間ファイルが `data/` の外へ散らばりやすく, その中に個人情報や提供データが混じるためです. 公開してよいデータを意図的に記録するときは, `-f` を付けて明示します.

~~~ bash
git add -f data/pref_stats_2026.csv
~~~

**Python と uv が作るもの**. `.venv/` は uv が作る仮想環境で, 数百 MB になることもあります. 各自の端末で作り直せるので記録しません. `__pycache__/` と `*.pyc` は Python が実行時に作る中間ファイルです.

::: warn

**`uv.lock` は除外しません.** `pyproject.toml` を置いて uv でプロジェクトを管理する場合, `uv.lock` にはどのライブラリのどのバージョンを使ったかが書かれます. これがあると他の端末で同じ環境を再現できるので, 自動生成されるファイルですが記録します.

:::

**OS が作るもの**. `.DS_Store` は macOS が, `Thumbs.db` は Windows がフォルダごとに作るファイルです.

`AGENTS.md` は除外しません. エージェントへの指示を書くファイルで, リポジトリの一部として記録します. ただし記録される以上, ここに鍵やアカウント名を書けません. 中身は[共通資料 コーディングエージェントの利用](agent.html)で扱います.

::: warn

**Jupyter Notebook (`.ipynb`) は, 実行結果をファイル本体に埋め込みます.** `df.head()` で表示した個人情報の行も, `.ipynb` の中に文字列として残ります. `data/` と `*.csv` を除外しても, notebook を記録すればデータが出ていきます.

notebook を記録するときは, 保存する前に出力をすべて消してください. VSCode では notebook の上部にある Clear All Outputs で消せます.

:::

::: warn

**`.gitignore` に書いても, 既に記録したファイルは追跡から外れません.** `.gitignore` は追跡していないファイルを候補に入れない設定であり, 追跡済みのファイルには効きません.

誤って記録したファイルを外すには, 次のようにします.

~~~ bash
git rm --cached .env
git commit -m ".env を追跡の対象から外す"
~~~

`--cached` を付けると, 手元のファイルは残したまま追跡からだけ外れます. ただし**それ以前の履歴には残る**ので, 鍵であれば無効にして発行し直してください.

:::

# 日常的に使う 9 つのコマンド

以降はすべて, `clone` したディレクトリの中で実行します.

## status: 何が変わっているかを見る

~~~ bash
git status
~~~

~~~ text
On branch main
Changes not staged for commit:
        modified:   src/analysis.py

Untracked files:
        notes/2026-04-15-groupby.md
~~~

`modified` は追跡済みのファイルが書き換わったこと, `Untracked` はまだ git が知らないファイルであることを示します. 実際の出力にはこれに加えて操作の案内が数行出ますが, 読むべきはこの 2 つの一覧です.

## diff: 中身の変化を行単位で見る

~~~ bash
git diff
~~~

~~~ text
diff --git a/src/analysis.py b/src/analysis.py
--- a/src/analysis.py
+++ b/src/analysis.py
@@ -10,7 +10,7 @@
 df = pd.read_csv('data/survey.csv')

-mean = df['score'].mean()
+mean = df['score'].dropna().mean()
 print(mean)
~~~

先頭が `-` の行が消えた行, `+` の行が足された行です. この例では, 平均を取る前に欠損値を除くように書き換わっています.

::: warn

**`git diff` は追跡済みファイルの未ステージの変更しか表示しません.** エージェントが新しく作ったファイルは `Untracked` なので, `git diff` に何も出ません. 差分が空なのを見て「変更されていない」と判断すると, 新規ファイルを読まないまま次へ進みます.

エージェントに作業させたら, **まず `git status` で一覧を見て, 新規ファイルは中身を開いて読んでください.** 変更されたファイルは `git diff` で読みます. 次の資料で codex に差分を頼むときも同じで, 依頼に「新しく作ったファイルも含めて」と添えます.

:::

## add: 記録するものを選ぶ

`add`, `commit`, `push` の 3 つで, 書き換えたファイルを GitHub へ届けます. この 3 つは, 荷物を送る手順にたとえられます.

![add はファイルを箱に入れる, commit は箱を梱包して伝票を貼る, push は GitHub へ郵送する](/images/common/git/add-commit-push.png){.wide}

- `add` は, 送るものを**箱に入れる**操作です. 書き換えたファイルのうち, 記録するものだけを選んで入れます. 入れ直しも取り出しもできます
- `commit` は, 箱を**梱包して伝票を貼る**操作です. 伝票には, 中身の説明 (コミットメッセージ), 記録した人, 日時が書かれます. 梱包した箱は手元に残ります
- `push` は, 梱包した箱を GitHub へ**郵送する**操作です

たとえと違う点が 1 つあります. 郵便では荷物が手元からなくなりますが, `push` しても手元の記録は消えず, 同じ記録が GitHub にも置かれます.

~~~ bash
git add src/analysis.py notes/2026-04-15-groupby.md
~~~

`git add .` と書くと, **コマンドを実行したディレクトリより下**の変更をまとめて選べます. リポジトリの一番上で実行しないと, 上の階層の変更は入りません. また `.gitignore` で除外していないファイルは全部入るので, 実行する前に `git status` で一覧を確認してください.

## `diff --staged`: 記録する直前に中身を確認する

`git add` したファイルは `git diff` に出なくなります. これから記録される中身を読むには, 次を使います.

~~~ bash
git diff --staged
~~~

鍵やデータを誤って選んでいないかは, ここで最後に確認できます. 荷物のたとえでは, 箱を閉じる前に中身を確かめる操作です. **commit する前に必ず 1 度実行してください.**

## commit: 記録する

~~~ bash
git commit -m "欠損値を除いてから平均を取るように修正"
~~~

`-m` の後ろは**コミットメッセージ**で, 何をしたかを自分の言葉で書きます. 後から履歴を見たときに, この 1 行だけで何をしたか分かる必要があります. 「修正」「更新」だけでは分かりません.

## push: GitHub に送る

~~~ bash
git push
~~~

commit しただけでは手元にしか残りません. push して初めて GitHub 側に反映されます. 提出のたびに push してください.

::: warn

**push は, 記録を GitHub へ送る操作です.** 記録に鍵やデータが入っていると, 送った時点で情報が外へ出て, 取り消せません. 意味の分からないコマンドは実行せず, 調べるか教員に聞いてください. codex に頼んで実行させるときも同じです.

:::

## pull: GitHub 側の変更を取り込む

教員を Collaborator に招待すると, 教員も同じリポジトリに commit して push できます. 教員が push した変更は, 自分の手元にはまだありません. GitHub 側の変更を手元に取り込むのが `pull` です.

~~~ bash
git pull
~~~

最初に一度だけ, 次の設定をしておきます. 教員と自分の両方が commit しているときに, 2 つの変更を [merge](#merge-2-つの変更を合わせる) で合わせるという指定です. 設定しないと, その場面で `git pull` が `Need to specify how to reconcile divergent branches` と表示して止まります.

~~~ bash
git config --global pull.rebase false
~~~

自分が commit していない間に教員だけが push していた場合, `pull` は教員の commit を手元の履歴の先にそのまま足します.

~~~ text
Updating c0ae055..aa51c5d
Fast-forward
 src/analysis.py | 2 +-
 1 file changed, 1 insertion(+), 1 deletion(-)
~~~

`Fast-forward` は, 手元の履歴を GitHub 側の先頭まで進めただけ, という意味です.

教員と共同編集するリポジトリでは, **作業を始める前に `git pull` してください.** 手元を最新にしてから書き換えると, 次の merge で衝突しにくくなります.

教員の変更を自分の変更と合わせる前に読みたいときは, `pull` を `fetch` と `merge` の 2 つに分けて実行します. `fetch` は GitHub 側の記録を手元に取ってくるだけで, 自分のファイルは書き換えません.

~~~ bash
git fetch
git log --oneline main..origin/main
git diff main...origin/main
~~~

`origin/main` は, `fetch` で取ってきた GitHub 側の `main` です. 2 行目は GitHub 側にだけある commit の一覧を, 3 行目は自分と分かれた後に GitHub 側で加わった変更を表示します (3 行目の `...` は点 3 つです). 読み終えたら `git merge origin/main` で合わせます. `git pull` は, この `fetch` と `merge` を続けて実行するコマンドです.

## merge: 2 つの変更を合わせる

教員と自分の両方が commit していると, 履歴は 2 本に分かれています. この状態で `push` すると, GitHub 側に手元に無い commit があるため拒否されます.

~~~ text
 ! [rejected]        main -> main (fetch first)
error: failed to push some refs to 'https://github.com/<ユーザ名>/<リポジトリ名>.git'
hint: Updates were rejected because the remote contains work that you do not
hint: have locally.
~~~

`git pull` を実行すると, git は 2 本の履歴を **merge** (合流) し, 両方の変更を含む新しい commit を作ります. この commit のメッセージを書くために, エディタが開きます. `Merge branch 'main' of ...` という既定のメッセージが入っているので, そのまま保存して閉じれば合流が終わります. VSCode なら, 開いたタブを閉じます.

~~~ text
Auto-merging src/analysis.py
Merge made by the 'ort' strategy.
 src/analysis.py | 1 +
 1 file changed, 1 insertion(+)
~~~

`git log --oneline --graph` で, 分かれた履歴が合流したことを確かめられます. 左の線が履歴の枝分かれを表します.

~~~ text
*   4ca528d Merge branch 'main' of https://github.com/<ユーザ名>/<リポジトリ名>
|\
| * d5c92fa ファイルの先頭に目的を書く
* | a00f23b 欠損値を除いてから平均を取るように修正
|/
* 4f46cf5 平均点を求める
~~~

この例では, 教員がファイルの先頭に 1 行足し (`d5c92fa`), 自分が平均の行を直しました (`a00f23b`). 書き換えた行が離れているので, git が自動で合わせました. 合流したら `git push` で GitHub へ送ります.

### コンフリクトの解決

教員と自分が同じ行, または隣り合う行を書き換えていると, git はどちらを採るか決められず, 合流を止めます. これを**コンフリクト** (衝突) といいます.

~~~ text
Auto-merging src/analysis.py
CONFLICT (content): Merge conflict in src/analysis.py
Automatic merge failed; fix conflicts and then commit the result.
~~~

`git status` では, 衝突したファイルが `both modified` として出ます. ファイルを開くと, 衝突した箇所に git が印を付けています.

~~~ text
<<<<<<< HEAD
mean = df['score'].dropna().mean()
print(mean)
=======
mean = df['score'].mean()
print(f'平均点: {mean:.1f}')
>>>>>>> 1a942ba6d0c3e8b5f27a94c1e0d8b3f6a2c7e915
~~~

`<<<<<<< HEAD` から `=======` までが自分の変更, `=======` から `>>>>>>>` までが教員の変更です. `>>>>>>>` の後ろの英数字は, 教員の commit の識別子です. この例では, 自分は欠損値を除くように平均の行を直し, 教員はその次の行の表示を直していました. 次の手順で解決します.

1. 2 つの変更を読み, 残す内容を決めて, 印の行 (`<<<<<<<`, `=======`, `>>>>>>>`) を消して書き直します. この例では両方の変更を残します.

    ~~~ python
    mean = df['score'].dropna().mean()
    print(f'平均点: {mean:.1f}')
    ~~~

2. `git add src/analysis.py` で, 解決したファイルを選びます.
3. `git commit -m "教員の表示の修正を取り込む"` で, 合流の記録を作ります.
4. `git push` で送ります.

VSCode で衝突したファイルを開くと, 印の上に Accept Current Change (自分の変更を採る), Accept Incoming Change (教員の変更を採る), Accept Both Changes (両方を残す) のボタンが出ます. ボタンを使っても, 結果が意図どおりかは自分で読んで確かめます.

どちらを残すか判断できないときは, `git merge --abort` を実行すると, 合流を始める前の状態に戻ります. そのうえで教員に相談してください.

::: note

PDF や画像のように中身が文字でないファイルは, 行に分けて合わせられません. 教員と自分の両方が同じファイルを更新していれば, 書き換えた箇所に関係なくコンフリクトになります. 元になるファイルから作り直せるもの (LaTeX の原稿から作る PDF など) は, 初めから記録せず `.gitignore` で除外しておくと, このコンフリクトは起きません. 作り直せないファイルが衝突したときは, どちらの版を残すかを教員に相談してください. LaTeX の原稿での設定は, [共通資料 LaTeX による原稿作成](latex.html)の[記録するもの](latex.html#記録するもの)で扱います.

:::

## log: 履歴を見る

~~~ bash
git log --oneline
~~~

~~~ text
a3f79f8 欠損値を除いてから平均を取るように修正
f041ec0 README にリポジトリの目的を書く
568a3c1 Initial commit
~~~

左の 7 文字が記録の識別子です. 発表資料の図がどの記録の状態で出たものかは, これで示せます.

## restore: 変更を捨てる

エージェントの変更が意図と違っていたとき, 手元の変更を捨てて記録済みの状態に戻します.

~~~ bash
git restore --source=HEAD src/analysis.py
~~~

`--source=HEAD` は「最後に commit した状態から取り直す」という指定です. これを省くと `git add` で選んだ内容から取り直すので, `add` 済みの変更は残ります.

::: warn

**restore は取り消せません.** commit していない変更は警告なく消えます. この資料で扱うコマンドのうち, 元に戻せないのはこの操作だけです.

また restore が戻すのは追跡済みのファイルだけです. エージェントが新しく作ったファイルは残るので, 要らなければ自分で削除してください.

エージェントに作業させる前に commit しておけば, いつでもここへ戻れます.

:::

# branch と worktree

エージェントを複数同時に動かすための branch と worktree は, [共通資料 コーディングエージェントの利用](agent.html)の発展節で扱います. 作業中に見慣れないディレクトリが増えていたら, worktree によって分けられた作業場所です.

# ディレクトリの構成

進捗を記録するリポジトリは, 次の構成を出発点にしてください. 作業しながら自分に合う形に変えて構いません.

~~~ text
<リポジトリ名>/
├── AGENTS.md          # エージェントへの指示
├── README.md          # このリポジトリが何か
├── .gitignore         # 記録しないもの
├── notes/             # 学んだことを書き出す場所
│   └── 2026-04-15-pandas-groupby.md
├── src/               # プログラム
│   └── analysis.py
└── data/              # データ (.gitignore で除外. GitHub 側には現れません)
~~~

`AGENTS.md` は, エージェントに毎回読ませる決まりごとを書くファイルです. 中身は[共通資料 コーディングエージェントの利用](agent.html)で扱います.

`notes/` は, エージェントに教わったことを自分の言葉で書き出す場所です. これは任意ですが, プログラムの理由を問われたときに答える材料になります (データサイエンス実践では最終回の発表の質疑で問います).

::: note

git が記録するのはファイルであってディレクトリではありません. 空のディレクトリを作っても `git status` には出ず, GitHub 側にも現れません. 中にファイルを 1 つ置いてから記録します.

:::

::: note

### Exercise GIT-1

**リポジトリを作って最初の記録を残す**

変更する, 差分を読む, 記録する, 送る, の順に 1 周します.

1. `git --version` と `gh --version` が表示されることを確認し, 名前とメールアドレスを設定する.
2. `gh auth login` で認証する.
3. GitHub で private のリポジトリを 1 つ作る (Add README を On, Add .gitignore を Python).
4. `~/work` へ移動して `clone` する.
5. `.gitignore` に `data/` と表計算ファイルの行を追記する.
6. `README.md` をエディタで開き, このリポジトリが何か (どの講義の, 何を記録するものか) を 1 行書く.
7. `git status` で, `README.md` と `.gitignore` が `modified` として出ることを確認する.
8. `git add README.md .gitignore` してから, もう一度 `git status` を実行し, 表示が変わることを確認する.
9. `git diff --staged` で, 記録される中身を確認する.
10. 何をしたか分かるメッセージを付けて `git commit` し, `git push` する.
11. GitHub のリポジトリのページを再読み込みし, README の内容がページ下部に表示されていることと, Commits の履歴に自分のメッセージが載っていることを確認する.

8 で表示が変わる理由を説明できるようにしてください.

<details class="protected" data-pass="yakagika">
<summary>回答例</summary>

~~~ bash
git --version
gh --version
git config --global user.name "自分の名前"
git config --global user.email "自分のアドレス"
gh auth login

cd ~/work
git clone https://github.com/yourname/ds-practice-2026.git
cd ds-practice-2026

# .gitignore に data/ と *.csv などを追記 (エディタで編集)
# README.md に 1 行書く (エディタで編集)

git status
# On branch main
# Changes not staged for commit:
#         modified:   .gitignore
#         modified:   README.md

git add README.md .gitignore
git status
# Changes to be committed:
#         modified:   .gitignore
#         modified:   README.md

git diff --staged       # 記録される中身を最後に確認する
git commit -m "README にリポジトリの目的を書き, .gitignore にデータの除外を追記"
git push
~~~

8 で表示が変わる理由は次のとおりです. `git status` は変更を 2 つの段階に分けて表示します. `git add` する前は `Changes not staged for commit` (記録する対象に入っていない変更), `git add` した後は `Changes to be committed` (次の `commit` に入る変更) です. `add` はこの変更を次の記録に入れると選ぶ操作で, `commit` はそこで選んだものをまとめて記録する操作です.

`git add .` ではなくファイル名を指定したのは, `.gitignore` で除外できているかを確かめる前に, 全部を選ばないためです.

</details>

:::

::: note

### Exercise GIT-2

**新しく作ったファイルの扱いを確かめる**

エージェントが作るファイルは, 多くが新規ファイルです. 差分を確認するときの落とし穴を, 自分で確かめます.

1. GIT-1 のリポジトリで, `notes/first.md` を作り, 1 行書く.
2. `git diff` を実行し, 何も表示されないことを確認する.
3. `git status` を実行し, `notes/` が `Untracked` として出ることを確認する.
4. `git add notes/first.md` してから `git diff --staged` を実行し, 中身が表示されることを確認する.
5. `git commit` して `git push` する.

エージェントに作業させた後は, 差分が空でも変更が無いとは限らない理由を説明できるようにしてください.

<details class="protected" data-pass="yakagika">
<summary>回答例</summary>

~~~ bash
mkdir notes
echo "# 最初の記録" > notes/first.md

git diff
# (何も表示されない)

git status
# Untracked files:
#         notes/

git add notes/first.md
git diff --staged
# +# 最初の記録

git commit -m "最初のノートを置く"
git push
~~~

`git diff` が表示するのは, 追跡している (一度でも記録した) ファイルの未ステージの変更だけです. 新しく作ったファイルは追跡していないので, `git status` の `Untracked` にだけ出ます. エージェントが新しいファイルを作っても `git diff` は空のままなので, 作業の後はまず `git status` で一覧を確認します.

`git status` は, 中にファイルのあるディレクトリを `notes/` のようにまとめて表示します. 中のファイルを個別に見るときは `git status -uall` を使います.

</details>

:::

::: note

### Exercise GIT-3

**公開してよいか判断する**

次の 5 つについて, public のリポジトリに入れてよいか, 入れてはいけないか, 追加の確認が要るかを判断し, その理由を書いてください.

1. 自分で書いた `analysis.py`
2. 講義で配布された外部 API の認証トークンを書いた `.env`
3. 政府統計のポータルからダウンロードした CSV
4. 授業で配布された, 学生の学籍番号を含むアンケート結果
5. `analysis.py` が出力した集計結果のグラフ画像

<details class="protected" data-pass="yakagika">
<summary>回答例</summary>

1. コードの中に認証トークンやパスワードを直接書いていなければ, **入れてよい**です. 自分で書いたコードで, 鍵もデータも含まないからです. 直接書いていた場合は, 2 と同じ理由で入れてはいけません. 記録する前に, 鍵を `.env` に移します.
2. **入れてはいけません**. 認証トークンは共有の前払い残高に直結します. 公開リポジトリは機械的に走査されるため, 発見されるまでの時間は短いです. `.gitignore` に `.env` を書いて除外します.
3. **利用規約を確認してから判断します**. 公開データでも再配布の可否や出典表示の条件はデータセットごとに違います. e-Stat のデータは出典を明記すれば再配布できますが, 条件の異なるものもあります. なお容量が大きい場合は, リポジトリに入れず取得手順を書く方が扱いやすいです.
4. **入れてはいけません**. 学籍番号は個人を特定できます. 学籍番号を消しても, 学部と学年と回答の組み合わせで特定できることがあります. `data/` ごと除外します.
5. **元データによります**. 集計結果そのものは個人を特定しませんが, 元データが 4 のような非公開データで, かつ集計の区分が細かいと (たとえば該当者が 1 人しかいない区分があると), その値から個人が特定できます. 元データが 3 なら, 3 と同じ条件で判断します.

</details>

:::

::: note

### Exercise GIT-4

**GitHub 側の変更を取り込んで合流させる**

教員の添削の代わりに, GitHub の画面で自分のリポジトリを直し, 手元の変更と合流させます. [Exercise GIT-1](#exercise-git-1) のリポジトリを使います.

1. `git config --global pull.rebase false` を設定する.
2. GitHub のリポジトリのページで `README.md` を開き, 右上の鉛筆のアイコン (Edit this file) を押す. 末尾に 1 行書き足し, Commit changes を押して commit する.
3. 手元では `git pull` をしないまま, `.gitignore` の末尾に 1 行書き足して commit し, `git push` する. 拒否されることを確認する.
4. `git fetch` してから `git diff main...origin/main` を実行し, 2 で書き足した文が表示されることを確認する.
5. `git pull` で合流させ, `git log --oneline --graph` で履歴が合流したことを確認してから, `git push` する.
6. もう一度 GitHub の画面で `README.md` の 1 行目を書き換えて commit し, 手元でも `git pull` をしないまま同じ 1 行目を別の内容に書き換えて commit する. `git pull` でコンフリクトを起こし, 解決して push する.

3 で push が拒否された理由と, 6 でだけコンフリクトになった理由を説明できるようにしてください.

<details class="protected" data-pass="yakagika">
<summary>回答例</summary>

~~~ bash
git config --global pull.rebase false

# 3: GitHub 側の変更を取り込まずに commit して push する
git add .gitignore
git commit -m ".gitignore に除外の行を足す"
git push            # rejected (fetch first) と表示される

# 4: 合わせる前に GitHub 側の変更を読む
git fetch
git diff main...origin/main

# 5: 合流させて送る
git pull            # エディタが開くので, 既定のメッセージのまま閉じる
git log --oneline --graph
git push

# 6: 同じ行を書き換えてコンフリクトを起こす
git add README.md
git commit -m "README の 1 行目を書き換える"
git pull            # CONFLICT (content): Merge conflict in README.md
# README.md を開いて印を消し, 残す内容に書き直す
git add README.md
git commit -m "README の 1 行目の衝突を解決する"
git push
~~~

3 で拒否されるのは, GitHub 側に手元に無い commit (2 で作ったもの) があるからです. push は GitHub 側の履歴の先に自分の commit を足す操作なので, GitHub 側が先に進んでいると足せません.

5 では `README.md` と `.gitignore` という別々のファイルを書き換えたので, git が自動で合わせます. 6 では同じファイルの同じ行を両方が書き換えたので, git はどちらを残すか決められず, コンフリクトとして止まります.

</details>

:::

# この資料を使う講義

この資料は, 次の講義から参照されます. 講義ごとの扱いは, それぞれの資料の最初の章で案内します.

- [特別講義DS](slds1.html)
- [データサイエンス実践](dsp1.html)
- [関数型プログラミング](fp1.html)
