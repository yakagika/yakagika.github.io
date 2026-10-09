---
title: 共通資料 LaTeX による原稿作成
description: TeX Live と VSCode で学会発表の原稿を LaTeX で書き, GitHub で教員と共有する
tags:
    - programming
    - lecture
featured: false
date: 2026-10-08
open: true
tableOfContents: true
katex: true
previousChapter: agent.html
---

本資料は複数の講義で共通に使う資料です. 学会発表の原稿を LaTeX で書き, GitHub の private リポジトリで教員と共有するまでを扱います. 原稿の様式には, 計測自動制御学会 (SICE) のシステム・情報部門 学術講演会 (SSI) のテンプレートを使います.

前提として[共通資料 プログラミング用の設定](setup.html)で VSCode を, [共通資料 バージョン管理とGitHub](git.html)で git と GitHub CLI を導入しておいてください.

::: warn

**この資料の情報は, 短い期間で古くなります.** TeX Live の版, 学会のテンプレートの配布場所, VSCode の拡張機能の画面は変わります. 資料と実際の画面が食い違ったら, 配布元の最新の情報を確かめてください.

:::

# LaTeX で原稿を書く理由

LaTeX は, 文章と書式の指示を書いたテキストファイルから PDF を組版する仕組みです. Word と比べると, 原稿をテキストで書くことで次の 3 つができます.

- **差分を読める**: 原稿がテキストなので, git で何を書き換えたかを行単位で確認できます. 教員の添削も差分として残ります.
- **番号と参照を自動で振れる**: 図, 表, 数式, 参考文献の番号は LaTeX が振ります. 図を 1 枚足しても, 本文中の「Fig. 2」を手で直す必要はありません.
- **学会の様式に合わせられる**: 多くの学会が LaTeX のテンプレートを配布しています. 余白や文字の大きさはテンプレートが決めるので, 書き手は中身に集中できます.

LaTeX の原稿 (`.tex`) から PDF ができるまでには, 2 つのプログラムを順に動かします. 本資料では日本語に対応した **pLaTeX** で原稿を組版して `.dvi` という中間ファイルを作り, **dvipdfmx** で `.dvi` を PDF に変換します. SSI のテンプレートは pLaTeX 用に作られているので, それに合わせます.

~~~ text
paper.tex  --(platex)-->  paper.dvi  --(dvipdfmx)-->  paper.pdf
~~~

図や数式の番号を確定させるには pLaTeX を 2 回以上動かす必要があります. この手順は **latexmk** というプログラムが必要な回数だけ自動で繰り返すので, 書き手が回数を数える必要はありません.

# TeX Live のインストール

**TeX Live** は, pLaTeX, dvipdfmx, latexmk と日本語のフォントをまとめた配布です. 容量が数 GB あり, インストールには 1 時間以上かかることがあります. 時間に余裕のあるときに, 電源とネットワークにつないだ状態で始めてください.

## macOS

[共通資料 プログラミング用の設定](setup.html)で入れた Homebrew で, TeX Live の macOS 版 (MacTeX) を入れます. `-no-gui` は, 付属のエディタなどを除いた版です. エディタには VSCode を使うので, こちらで足ります.

~~~ bash
brew install --cask mactex-no-gui
~~~

途中で macOS のパスワードを求められたら入力します. 終わったらターミナルを開き直します.

## Windows

winget には TeX Live がありません. TeX Live の公式サイトからインストーラ [install-tl-windows.exe](https://mirror.ctan.org/systems/texlive/tlnet/install-tl-windows.exe) をダウンロードし, ダブルクリックで実行します. 警告が出たら「詳細情報」から「実行」を選びます.

インストーラの画面では, 設定を変えずに Install (インストール) を押します. 必要なファイルを 1 つずつダウンロードするので, 回線によっては終わるまで 1 時間以上かかります. 終わったら PowerShell を開き直します.

::: warn

インストール先のパスに日本語などの ASCII 以外の文字が含まれると, インストールに失敗することがあります. 既定のインストール先 (`C:\texlive\2026` のような場所) から変えないでください.

:::

::: note

Windows には MiKTeX という別の配布もあり, winget で入れられます. MiKTeX は, 足りないパッケージをビルドのたびに取りに行く仕組みで, latexmk の実行には別途 Perl が必要です. 本資料の設定は TeX Live を前提にしているので, TeX Live を使ってください.

:::

## インストールの確認

ターミナル (Windows は PowerShell) で次の 3 つを実行し, いずれもバージョンが表示されることを確認します.

~~~ bash
platex --version
dvipdfmx --version
latexmk --version
~~~

`platex` が見つからないと表示された場合は, ターミナルを開き直してからもう一度試します. それでも見つからない場合は, パソコンを再起動します.

TeX Live を入れる前から VSCode を開いていた場合は, VSCode もすべて終了して起動し直します (macOS はウィンドウを閉じるだけでは終了しないので, `Cmd + Q` で終了します). 起動中の VSCode は, TeX Live を入れる前のコマンドの探し場所 (環境変数 `PATH`) を持ち続けるので, ターミナルで確認できても VSCode からのビルドでは `latexmk` が見つからないことがあります.

# VSCode の LaTeX Workshop

VSCode では, 拡張機能 **LaTeX Workshop** で LaTeX の原稿を編集します. [共通資料 プログラミング用の設定](setup.html)の拡張機能の手順と同じく, 左の拡張機能のアイコンを押し, 検索窓に `latex` と入力して `LaTeX Workshop` の `install` を押します. すでに入れていれば, この手順は不要です.

LaTeX Workshop は, 既定では `.tex` を書き換えるたびに pdfLaTeX という英語向けのプログラムでビルドしようとします. 日本語の原稿は pdfLaTeX では組版できません. 次に作る原稿のリポジトリには, pLaTeX でビルドする設定のファイルが入っています ([ビルドの設定ファイル](#ビルドの設定ファイル)).

# 原稿のリポジトリを作る

原稿は, 講義の進捗を記録するリポジトリとは別のリポジトリで管理します. 共著者である教員と共有する範囲を, 原稿だけに限るためです.

## テンプレートから作る

原稿のリポジトリは, 教員が用意した**テンプレートリポジトリ** [yakagika/ssi-paper-template](https://github.com/yakagika/ssi-paper-template) から作ります. SSI の配布するテンプレートに, 本資料の設定 (ビルドの設定, 参考文献の書式, 記録しないファイルの指定) を加えたものです. 何を変えてあるかは[テンプレートの中身](#テンプレートの中身)で説明します.

1. ブラウザで GitHub にログインした状態で, テンプレートリポジトリのページを開きます.
2. 右上の緑のボタン **Use this template** を押し, **Create a new repository** を選びます.

    ![テンプレートリポジトリのページ. 右上の Use this template を押すと, Create a new repository と Open in a codespace の 2 つが出る](/images/common/latex/use-template.png)

3. 入力画面で次のようにします.
    - **Owner**: 自分のアカウント
    - **Repository name**: 原稿だと分かる名前 (例: `ssi2026-paper`)
    - **visibility**: **Private** を選びます
    - **Include all branches**: チェックを入れないままにします
4. **Create repository** を押します.

自分のアカウントに, テンプレートと同じファイルを持つ private のリポジトリができます. このリポジトリは中身を複写した別のリポジトリで, テンプレートの変更の履歴も, テンプレートとのつながりも持ちません. 自分のリポジトリなので, 自由に commit して push できます.

作成したら, [教員を Collaborator に招待する](git.html#教員を-collaborator-に招待する)の手順で教員を招待し, 作業用のディレクトリへ `clone` します. clone するのは, いま作った**自分のリポジトリ**です.

~~~ bash
cd ~/work
git clone https://github.com/<自分のユーザ名>/ssi2026-paper.git
cd ssi2026-paper
git remote -v
~~~

`git remote -v` は, push の送り先 (`origin`) を表示します. 自分のユーザ名のリポジトリが表示されていれば, そのまま使えます.

::: warn

テンプレートリポジトリ (`yakagika/ssi-paper-template`) を直接 clone しないでください. 送り先が教員のテンプレートになり, push は権限がないので拒否されます. `git remote -v` に `yakagika/ssi-paper-template` と表示されたら, そのディレクトリは削除し, 自分のリポジトリを clone し直します.

:::

# テンプレートの中身

## 入っているファイル

clone したリポジトリには, 次のファイルが入っています.

~~~ text
ssi2026-paper/
├── .gitignore
├── .latexmkrc
├── .vscode/
│   └── settings.json
├── README.md
├── SICE-SSI.sty
├── figure/
│   └── count.png
├── paper.tex
├── references.bib
└── sice-ssi.bst
~~~

| ファイル | 内容 |
|---|---|
| `paper.tex` | 原稿. はじめは見本の骨組みが書いてあり, これを書き換えて自分の原稿にする |
| `references.bib` | 参考文献の一覧 ([参考文献](#参考文献)) |
| `figure/` | 原稿に載せる図を置くディレクトリ. 見本の図が 1 枚入っている |
| `SICE-SSI.sty` | 余白, 文字の大きさ, 題目の書式などを決める SSI の様式のファイル. 配布元のまま |
| `sice-ssi.bst` | 参考文献を SSI の推奨する形式に整える設定 |
| `.latexmkrc`, `.vscode/settings.json` | 保存したら pLaTeX でビルドする設定 ([ビルドの設定ファイル](#ビルドの設定ファイル)) |
| `.gitignore` | 記録の対象から外すファイルの指定 ([記録するもの](#記録するもの)) |

SSI の様式そのもの (`SICE-SSI.sty`) は, 大会のページの発表要領で配布されています. 2026 年の大会では[SSI2026 の発表要領](https://www.sice.or.jp/org/SSI2026/presentation.html)にあり, 原稿の分量などの決まりもそこに書かれています. 投稿の前に, その年の発表要領を読んでください.

## SSI の配布版から変えたところ

SSI の配布する見本 (`sample.tex`) から, 次の 3 点を変えてあります.

**図を読み込む指定**. 配布版は, 図を読み込むパッケージを `\usepackage[dvips]{graphicx}` と指定しています. `dvips` は, `.dvi` を PDF でなく PostScript という形式に変換するプログラムです. この指定のまま Python で作った PNG の図を読み込むと, 図の大きさを読み取れず, `Cannot determine size of graphic` というエラーでビルドが止まります. 本資料では dvipdfmx で PDF を作るので, テンプレートでは `\usepackage[dvipdfmx]{graphicx}` に変えてあります. 他の学会のテンプレートを使うときも, この指定を確かめてください.

**参考文献**. 配布版は参考文献を原稿の末尾に手で書く方式です. テンプレートでは, 文献の情報を `references.bib` にまとめて書き, 番号と書式は自動で整える方式に変えてあります. 書式を決める `sice-ssi.bst` は, 配布版の見本が推奨する形式に合わせて作ったものです.

**ビルドと記録の設定**. `.latexmkrc`, `.vscode/settings.json`, `.gitignore` を加えてあります. 中身は次の節で説明します.

## ビルドの設定ファイル

ビルドの設定は 2 つのファイルにあります. どちらもリポジトリに記録されているので, 教員が clone したときも同じ設定でビルドできます.

1 つ目は, latexmk の設定ファイル `.latexmkrc` です. ファイル名は `.` から始まります.

~~~ perl
$latex = 'platex -synctex=1 -interaction=nonstopmode -file-line-error %O %S';
$bibtex = 'pbibtex %O %B';
$dvipdf = 'dvipdfmx %O -o %D %S';
$pdf_mode = 3;
~~~

1 行目は, 組版に `platex` を使う指定です. 後ろの 3 つのオプションは, PDF と原稿の行を対応づける情報を作ること, エラーで止まらず最後まで処理すること, エラーの位置をファイル名と行番号で表示することを指定します. 2 行目は, 参考文献の一覧を作るのに日本語に対応した `pbibtex` を使う指定です. 3 行目は, `.dvi` から PDF への変換に `dvipdfmx` を使う指定です. 4 行目の `$pdf_mode = 3` は, latexmk に「`.dvi` を作り, dvipdfmx で PDF にする」という手順を選ばせます.

2 つ目は, VSCode の設定ファイル `.vscode/settings.json` です.

~~~ json
{
    "latex-workshop.latex.recipe.default": "latexmk (latexmkrc)",
    "latex-workshop.latex.autoBuild.run": "onSave"
}
~~~

1 行目は, LaTeX Workshop に `.latexmkrc` の設定どおりに latexmk を動かす手順 (recipe) を選ばせます. 2 行目は, ファイルを保存したときにビルドする指定です. このファイルはこのリポジトリを開いたときだけ効くので, 他のリポジトリの VSCode の設定は変わりません.

## 保存でビルドされることの確認

VSCode でリポジトリのディレクトリを開き, `paper.tex` を開きます. 何か 1 文字書き足して消し, `Ctrl + S` (macOS は `Cmd + S`) で保存します.

保存するとビルドが始まり, 画面の下端のステータスバーに進み具合が表示されます. 終わると `paper.tex` と同じディレクトリに `paper.pdf` ができます. エディタの右上にある View LaTeX PDF のアイコンを押すと, PDF がエディタの右側に開きます. 開いた PDF は, 保存してビルドし直すたびに更新されます.

ビルドに失敗すると, 画面の下の「問題」(Problems) の欄にエラーが表示されます. エラーの読み方は[エラーが出たとき](#エラーが出たとき)で扱います.

# 原稿の書き方

テンプレートの `paper.tex` には, はじめから原稿の骨組みが書いてあります. この節では骨組みを例に書き方を説明します. 自分の原稿は, `paper.tex` の中身を書き換えて作ります.

## 原稿の骨組み

次がテンプレートの `paper.tex` です. SSI の原稿に必要な要素をひととおり含みます. 研究の内容は架空のもので, 参考文献のうち日本語の 2 件も実在しません.

~~~ latex
\documentclass{jarticle}
\usepackage{SICE-SSI}
\usepackage[dvipdfmx]{graphicx}
\usepackage{amsmath}
\usepackage{url}

\begin{document}

\title{大学図書館の貸出記録による利用者の類型化}
\author{○千葉太郎\ \ 商科花子\ （千葉商科大学）}
\abstract{
  大学図書館の貸出記録から利用者の行動を類型化した．
  （ここに 3〜5 行で目的，方法，結果を書く．）
}
\keyword{クラスタリング，図書館，貸出記録}

\maketitle\thispagestyle{empty}
\pagestyle{empty}

\section{はじめに}

大学図書館の利用者は，目的によって借りる本の分野と期間が異なる\cite{tanaka2020}．
本研究では，貸出記録をクラスタリングして利用者を類型化する\footnote{データは匿名化されたものを用いた．}．

\section{方法}

\subsection{データ}

2025 年度の貸出記録 1,200 件を，分野ごとに集計して用いた\cite{suzuki2018}．記録の内訳を Table~\ref{tab:data} に示す．

\begin{table}[t]
  \centering
  \caption{Number of loans by field.}
  \label{tab:data}
  \begin{tabular}{lr}
    \hline
    Field & Loans \\
    \hline
    Literature & 480 \\
    Science & 420 \\
    Qualification & 300 \\
    \hline
  \end{tabular}
\end{table}

\subsection{分析の手法}

利用者 $i$ の特徴量を $\boldsymbol{x}_i$，クラスタ $k$ の中心を $\boldsymbol{\mu}_k$ とし，
式 \eqref{eq:kmeans} の $J$ を最小にする k-means 法\cite{macqueen1967}を用いた．
\begin{equation}
  J = \sum_{k=1}^{K} \sum_{i \in C_k} \| \boldsymbol{x}_i - \boldsymbol{\mu}_k \|^2
  \label{eq:kmeans}
\end{equation}

\section{結果}

週ごとの貸出件数を Fig.~\ref{fig:count} に示す．

\begin{figure}[t]
  \centering
  \includegraphics[width=0.9\linewidth]{figure/count.png}
  \caption{Number of loans per week.}
  \label{fig:count}
\end{figure}

\section{おわりに}

（ここに結論を書く．）

\small
\bibliographystyle{sice-ssi}
\bibliography{references}
\normalsize

\end{document}
~~~

保存すると, 次の PDF ができます. 表と図は `[t]` の指定によりページの上端に置かれ, 本文は 2 段組になります.

![骨組みをビルドした結果. 題目からキーワードまでが 1 段組, 本文と参考文献が 2 段組になり, 図, 表, 数式, 脚注, 参考文献に番号が振られている](/images/common/latex/paper-skeleton.png)

LaTeX の原稿は, `\` で始まる**コマンド**と, `\begin{...}` から `\end{...}` までで範囲を指定する**環境**で書式を指示します. `\begin{document}` より前を**プリアンブル**といい, 使う様式とパッケージを宣言します. 骨組みの 1〜5 行目は, `jarticle` (日本語の論文の基本の書式) に SSI の様式 `SICE-SSI` を重ね, 図を読み込む `graphicx`, 数式を書く `amsmath`, 参考文献の URL を書く `url` を使う宣言です.

::: note

SSI の見本は, 句読点に全角の「，」と「．」を使っています. 骨組みもそれに合わせています. どちらに揃えるかは, 投稿先の見本と共著者の指示に従ってください.

:::

## 題目, 著者, 概要, キーワード

`\title`, `\author`, `\abstract`, `\keyword` に書いた内容は, `\maketitle` の位置に 1 段組で組まれます. SSI の様式では次の点に従います.

- **著者**: 発表する人 (登壇者) の名前の前に `○` を付けます. 名前の間は `\ \ ` で空白を空け, 最後に所属を全角の括弧で書きます
- **概要**: 目的, 方法, 結果を 3〜5 行にまとめます
- **キーワード**: 3〜5 語を `，` で区切ります

`\maketitle` の後の `\thispagestyle{empty}` と `\pagestyle{empty}` は, ページ番号を付けない指定です. SSI の見本のとおりに残してください.

## 節と小節

`\section{...}` で節, `\subsection{...}` で小節を作ります. 番号 (1, 2.1 など) は LaTeX が振ります. 番号を付けたくない見出しには `\section*{...}` と `*` を付けます.

本文の段落は, 空行で区切ります. 1 行の改行だけでは段落は変わりません. 原稿のテキストでは 1 文ごとに改行しておくと, git の差分が文単位で表示され, 教員の添削がどの文に入ったかを読みやすくなります.

## 数式

文中の数式は `$` で囲みます. `$\boldsymbol{x}_i$` は太字のベクトル $\boldsymbol{x}_i$ に, `_` は下付き, `^` は上付きになります.

番号を付けて別の行に置く数式は `equation` 環境で書き, `\label{...}` で名前を付けます. 本文からは `\eqref{...}` で参照すると, 括弧つきの番号 (1) が入ります.

~~~ latex
\begin{equation}
  J = \sum_{k=1}^{K} \sum_{i \in C_k} \| \boldsymbol{x}_i - \boldsymbol{\mu}_k \|^2
  \label{eq:kmeans}
\end{equation}
~~~

`\sum_{k=1}^{K}` は $\sum_{k=1}^{K}$ に, `\in` は $\in$ に, `\|` はノルムの記号 $\|$ になります. 記号の書き方が分からないときは, 「LaTeX 記号名」で検索するか, 書きたい数式をコーディングエージェントに伝えて LaTeX の書き方を尋ねてください. 尋ねた結果は, ビルドした PDF で意図した数式になっているかを必ず確認します.

## 図

図のファイルは, リポジトリの `figure` というディレクトリに置きます. Python で作った図は, `savefig` で解像度を指定して PNG で保存します.

~~~ python
fig.savefig("figure/count.png", dpi=300)
~~~

`dpi=300` は, 1 インチあたり 300 画素で保存する指定です. 2 段組の原稿では図が小さく印刷されるので, 既定の解像度では文字がにじみます.

原稿では `figure` 環境の中で `\includegraphics` を使って読み込みます. `width=0.9\linewidth` は, 図の幅を段の幅の 9 割にする指定です. 段をまたいで横いっぱいに置きたい図は, `figure` を `figure*` に変えます.

~~~ latex
\begin{figure}[t]
  \centering
  \includegraphics[width=0.9\linewidth]{figure/count.png}
  \caption{Number of loans per week.}
  \label{fig:count}
\end{figure}
~~~

SSI の様式では, 図の説明文 (`\caption`) と図の中の文字を英語で書き, 本文からは「Fig.~\ref{fig:count} に示す」のように `Fig.` を付けて参照します. `~` は改行しない空白で, 「Fig.」と番号が行をまたいで分かれるのを防ぎます. Python で図を作るときも, 軸の名前などを英語にしておきます.

## 表

表は `table` 環境の中に `tabular` 環境で書きます. `{lr}` は列ごとの文字の寄せ方で, `l` は左寄せ, `r` は右寄せ, `c` は中央です. 列は `&` で区切り, 行の終わりに `\\` を書きます. `\hline` は横線です.

~~~ latex
\begin{table}[t]
  \centering
  \caption{Number of loans by field.}
  \label{tab:data}
  \begin{tabular}{lr}
    \hline
    Field & Loans \\
    \hline
    Literature & 480 \\
    \hline
  \end{tabular}
\end{table}
~~~

表の説明文は表の上に置くので, `\caption` を `tabular` より前に書きます. 本文からは `Table~\ref{tab:data}` で参照します.

## 参考文献

参考文献は, 文献の情報を `references.bib` に 1 件ずつ書き, 本文からは `\cite{名前}` で引用します. 番号と書式は, ビルドのときに自動で整います. 次がテンプレートの `references.bib` です.

~~~ bibtex
@article{tanaka2020,
  author  = {田中 一郎},
  title   = {大学図書館の利用実態},
  journal = {図書館学研究},
  volume  = {12},
  number  = {3},
  pages   = {45--56},
  year    = {2020}
}

@book{suzuki2018,
  author    = {鈴木 次郎 and 佐藤 花子},
  title     = {データ分析入門},
  publisher = {商科出版},
  year      = {2018}
}

@inproceedings{macqueen1967,
  author    = {MacQueen, J.},
  title     = {Some methods for classification and analysis of multivariate observations},
  booktitle = {Proceedings of the Fifth Berkeley Symposium on Mathematical Statistics and Probability},
  volume    = {1},
  pages     = {281--297},
  year      = {1967}
}

@misc{ssi2026,
  author = {{計測自動制御学会 システム・情報部門}},
  title  = {SSI2026 発表要領},
  url    = {https://www.sice.or.jp/org/SSI2026/presentation.html},
  note   = {2026 年 10 月 9 日参照}
}
~~~

1 件は `@種類{名前, 項目 = {値}, ...}` の形です. `名前` は本文の `\cite{...}` に書く名前で, 著者と年を組み合わせると覚えやすくなります. よく使う種類は次の 4 つです.

| 種類 | 文献 | 主な項目 |
|---|---|---|
| `article` | 学術雑誌の論文 | `author`, `title`, `journal`, `volume`, `number`, `pages`, `year` |
| `inproceedings` | 学会の予稿集の論文 (SSI の原稿もこれ) | `author`, `title`, `booktitle`, `pages`, `year` |
| `book` | 書籍 | `author`, `title`, `publisher`, `year` |
| `misc` | Web のページなど | `author`, `title`, `url`, `note` |

項目の書き方は次のとおりです.

- **著者**: 複数の著者は ` and ` でつなぎます. 日本語の名前は姓と名の間に空白を入れます. 学会や組織のように姓と名に分けない名前は, `{{...}}` と二重の括弧で囲みます
- **ページ**: `45--56` のように `-` を 2 つ続けます. 書式の指定により `45/56` と表示されます
- **Web のページ**: `url` にアドレスを, `note` に参照した日を書きます

原稿の側では, 参考文献を置く位置 (本文の最後) に次の 4 行を書きます.

~~~ latex
\small
\bibliographystyle{sice-ssi}
\bibliography{references}
\normalsize
~~~

`\bibliographystyle{sice-ssi}` は書式の指定で, `sice-ssi.bst` を使います. `\bibliography{references}` は文献の一覧のファイル `references.bib` の指定で, 拡張子は付けません. 前後の `\small` と `\normalsize` は, 参考文献の文字を本文より一段小さくする指定です.

ビルドすると, 本文で引用した文献だけが, 引用した順に番号つきで並びます. 引用した箇所には上付きの番号 `1)` が入ります. テンプレートの `references.bib` には 4 件ありますが, 本文で引用していない `ssi2026` は一覧に載りません. 書式は SSI の見本が推奨する次の形で, 英語の文献は句読点が半角になります.

~~~ text
雑誌論文: 田中一郎：大学図書館の利用実態，図書館学研究，12-3，45/56（2020）
単行本:   鈴木次郎，佐藤花子：データ分析入門，商科出版（2018）
予稿集:   J. MacQueen: Some methods for classification ..., Proceedings of ..., 1, 281/297 (1967)
Web:      計測自動制御学会 システム・情報部門：SSI2026 発表要領，https://...（2026 年 10 月 9 日参照）
~~~

巻の番号 (`12`, `1`) は太字で表示されます.

論文の検索サイト (Google Scholar, CiNii Research など) では, 文献の情報を BibTeX の形式で書き出せます. 書き出したものを `references.bib` に貼り付け, `名前` を分かりやすいものに変えて使えます. ただし書き出した情報は, 項目が欠けていたり, 日本語の著者名が崩れていたりすることがあります. ビルドした PDF で, 著者, 題目, 巻, ページ, 年を元の文献と見比べてください.

`references.bib` を書き換えても PDF に反映されないときは, `paper.tex` を開いて保存し直します.

## 脚注

`\footnote{...}` を書いた位置に番号が入り, 内容は段の下に置かれます. 題目に付ける注 (他の学会で発表済みであることの断り書きなど) は, `\title` の中で `\thanks{...}` を使います.

## エラーが出たとき

ビルドに失敗すると, 「問題」(Problems) の欄にエラーの内容と行番号が出ます. 行番号の位置か, その少し前に原因があります. 初めのうちによく出るエラーと警告は次の 4 つです.

| エラーの表示 | よくある原因 |
|---|---|
| `Undefined control sequence` | コマンドの綴りの誤り, またはそのコマンドを含むパッケージを `\usepackage` していない |
| `Missing $ inserted` | `_` や `^` を `$` の外で使った. 文中で記号として書くときは `\_` と書く |
| `File 'figure/count.png' not found` | 図のファイルの名前か置き場所の誤り. `.tex` からの相対パスで書く |
| `Citation 'tanaka2020' on page 1 undefined` | `\cite` の名前が `references.bib` の文献の名前と一致しない, または `references.bib` を保存していない |

`%` から行末まではコメントとして無視されます. 本文で `%` を記号として書くときは `\%` と書きます.

PDF の参照が `??` と表示される場合は, 番号がまだ確定していません. もう一度保存すると latexmk が組版を繰り返し, 番号が入ります. それでも `??` のままなら, `\label` と `\ref` の名前が一致しているかを確認します.

# GitHub で教員と共有する

## 記録するもの

リポジトリには, 原稿を組版し直すのに必要なファイルだけを記録します. テンプレートから作ったリポジトリには, 次のファイルがはじめから記録されています.

| 記録するもの | 理由 |
|---|---|
| `paper.tex`, `references.bib` | 原稿と参考文献の一覧 |
| `figure/` の図 | 原稿が読み込む |
| `SICE-SSI.sty`, `sice-ssi.bst` | 原稿と参考文献の様式 |
| `.latexmkrc`, `.vscode/settings.json` | 誰が clone しても同じ設定でビルドするため |
| `.gitignore` | 誰が clone しても同じものを記録の対象から外すため |

組版した `paper.pdf` は記録しません. PDF は中身が文字でないので, git は行に分けて合わせられません. 教員と自分の両方が PDF を記録すると, 本文の別々の箇所を直していても, `git pull` のたびに PDF がコンフリクトになります. PDF は原稿からいつでも作り直せるので, 記録するのは原稿だけにし, PDF は各自の手元でビルドします.

テンプレートの `.gitignore` は, `.aux`, `.log`, `.dvi`, `.bbl`, `.synctex.gz` などの中間ファイルと, リポジトリの一番上にある PDF (`paper.pdf`) を記録の対象から外します. PDF の指定は `/*.pdf` で, 先頭の `/` があるので `figure/` に置いた PDF の図は外れません. ビルドした後に `git status` を実行し, 中間ファイルと `paper.pdf` が出てこないことを確認してください.

## 記録して送る

[共通資料 バージョン管理とGitHub](git.html)の日常的に使うコマンドで, 原稿の変更を記録して送ります.

~~~ bash
git status
git diff paper.tex
git add paper.tex references.bib figure/
git commit -m "結果の節に貸出件数の図を加える"
git push
~~~

変更の確認は `paper.tex` の差分で行い, 紙面はビルドした PDF を目で確かめます.

## 教員の添削を取り込む

教員は, 原稿を手元に取り込んでビルドした PDF を読み, 原稿を直接直して commit することがあります. 教員が push した変更は `git pull` で手元に取り込み, 自分の変更と合わせます. `pull` と `merge` の使い方, 最初に一度だけ行う設定, コンフリクトの解決の手順は, [共通資料 バージョン管理とGitHub](git.html)の[pull: GitHub 側の変更を取り込む](git.html#pull-github-側の変更を取り込む)と[merge: 2 つの変更を合わせる](git.html#merge-2-つの変更を合わせる)で扱います.

原稿のリポジトリでは, 作業を始める前に毎回 `git pull` します. 取り込んだ後に `git log` を実行すると, 教員の commit が履歴に並んでいます. どこを直されたかは, `git show <commit の識別子>` で差分として確認できます.

手元の `paper.pdf` は記録していないので, `git pull` では更新されません. 取り込んだ後に VSCode で `paper.tex` を開いて保存し, PDF を作り直してから, 教員の添削が紙面に入っていることを確かめます.

教員と自分が `paper.tex` の同じ行を直していると, `git pull` はコンフリクトで止まります. そのときは[コンフリクトの解決](git.html#コンフリクトの解決)の手順で印を消して書き直し, 保存して PDF を作り直し, 紙面を確かめてから commit します. どちらを残すか判断できないときは, `git merge --abort` で合流を始める前の状態に戻し, 教員に相談してください.

# 演習

::: note

### Exercise LATEX-1

**テンプレートから原稿のリポジトリを作り, 保存でビルドする**

1. `platex --version`, `dvipdfmx --version`, `latexmk --version` がすべて表示されることを確認する.
2. テンプレートリポジトリから, 自分のアカウントに原稿用の private のリポジトリを作る.
3. 教員を Collaborator に招待し, 自分のリポジトリを手元に clone する.
4. `git remote -v` で, 送り先が自分のリポジトリになっていることを確認する.
5. VSCode で `paper.tex` を開いて保存し, `paper.pdf` ができることを確かめる.

次の 2 つを説明できるようにしてください.

- テンプレートリポジトリを直接 clone してはいけない理由
- テンプレートの `paper.tex` で, `graphicx` の指定を `dvips` から `dvipdfmx` に変えてある理由

<details class="protected" data-pass="yakagika">
<summary>回答例</summary>

5 で, 題目から参考文献までが 1 ページに収まり, 右の段の上端に図, 末尾に参考文献が 2 件入った PDF ができれば成功です.

テンプレートリポジトリを直接 clone すると, 送り先 (`origin`) が教員のテンプレートになります. 学生には書き込む権限がないので push が拒否され, 教員と原稿を共有できません. Use this template で作ったリポジトリは自分のものなので, それを clone すれば送り先も自分のリポジトリになります.

`dvips` の指定のままだと, PNG の図の大きさを読み取れず, `Cannot determine size of graphic` というエラーでビルドが止まります. 本資料は dvipdfmx で PDF を作るので, `graphicx` にもそれを伝えます.

</details>

:::

::: note

### Exercise LATEX-2

**自分の原稿の骨組みを作る**

自分の研究の題目で `paper.tex` を作ります. 次をすべて含めてください.

- 題目, 著者 (自分に `○`), 概要, キーワード
- 「はじめに」「方法」「結果」「おわりに」の 4 つの節
- 番号つきの数式を 1 つと, 本文からのその参照
- Python で作った図を 1 枚 (説明文は英語) と, 本文からのその参照
- 表を 1 つと, 本文からのその参照
- 参考文献を 2 件と, 本文からの引用

中身はまだ仮の文章で構いません. ビルドした PDF で, 図, 表, 数式, 参考文献の番号が `??` でなく数字になっていることを確認してください.

<details class="protected" data-pass="yakagika">
<summary>回答例</summary>

[原稿の骨組み](#原稿の骨組み)の `paper.tex` が 1 つの回答例です. 参考文献は `references.bib` に 2 件書き, 本文でそれぞれを `\cite` します. `references.bib` に書いても, 本文で引用しない文献は一覧に載りません.

番号が `??` のまま残る場合は, `\label{...}` と `\ref{...}` (`\eqref{...}`, `\cite{...}`) の名前の綴りが一致しているかを確認します. `\label` は `\caption` より後に書きます. 前に書くと, 図や表でなく節の番号を指します.

</details>

:::

::: note

### Exercise LATEX-3

**原稿を push して教員と共有する**

1. Exercise LATEX-2 で書いた原稿をビルドした後, `git status` で, 中間ファイル (`.aux`, `.log`, `.dvi` など) と `paper.pdf` が出てこないことを確認する.
2. `paper.tex`, `references.bib`, `figure/` の変更を commit して push する.
3. GitHub のリポジトリのページで `paper.tex` を開き, 書き換えた原稿になっていること, `paper.pdf` と中間ファイルが無いことを確認する.
4. リポジトリの Settings の Collaborators で, 教員が招待を受け取ったか (Pending のままでないか) を確認する.

<details class="protected" data-pass="yakagika">
<summary>回答例</summary>

~~~ bash
git status
git add paper.tex references.bib figure/
git diff --staged --stat
git commit -m "SSI の原稿の骨組みを作る"
git push
~~~

`git diff --staged --stat` は, 記録するファイルの一覧と変更した行数だけを表示します. 中間ファイルと `paper.pdf` が一覧に入っていないことを, commit の前にもう一度確かめられます.

4 で Pending のままなら, 教員がまだ招待を受け取っていません. 招待は 7 日で失効するので, 失効していたら招待し直します.

</details>

:::

# この資料を使う講義

この資料は, 次の講義から参照されます. 講義ごとの扱いは, それぞれの資料の最初の章で案内します.

- [特別講義DS](slds1.html)
- [データサイエンス実践](dsp1.html)
