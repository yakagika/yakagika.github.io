---
title: pyenv+anacondaによるPyMC環境構築(旧版・備忘録)
description: 授業資料をuv/Colabに移行するにあたり,旧版のpyenv+anacondaによるPyMC環境構築手順をアンインストール用の備忘録として記録したものです.
tags:
    - python
    - pymc
    - pyenv
    - setup
featured: false
date: 2025-04-20
tableOfContents: true
---

# pyenv+anacondaによるPyMC環境構築(旧版)

::: warn
本記事は,授業資料(統計学特殊講義DS等)で使用していた `pyenv` + `anaconda` によるPyMC環境構築の手順を記録したものです.

授業では `pyenv`+`anaconda` から `uv` / Google Colab への移行を進めており,**この手順は今後の授業では使用しません**. 過去にこの手順で環境構築を行った学生が,conda環境をアンインストールする際の備忘録として残しています.

新しい環境構築手順については,最新の授業資料を参照してください.
:::

## conda環境のアンインストール

過去にこの手順でPyMC環境を構築した場合,以下の手順でconda環境を削除できます.

まず,conda環境を無効化します.

~~~ sh
conda deactivate
~~~

続いて,作成した仮想環境を削除します.

~~~ sh
conda remove --name pymc_env --all
~~~

その後,pyenv自体のアンインストールについては [pyenvによるPython環境構築(旧版・備忘録)](/posts/2025-04-20-pyenv-setup-archive) の手順に従ってください.

---

以下は旧版の環境構築手順です.

# 環境構築

それでは,実際にプログラム上でこのモデルを構築,推定してみましょう.

`Python`においてベイズ統計学に基づいた統計モデリングを実施するためのライブラリとして`PyMC5`があります(古いVersionとして`PyMC3`があり全く異なる記法などを用いているので注意して下さい). こちらのライブラリは,Pythonのパッケージマネージャーである`anaconda`を利用します.
通常これまでに利用してきた`pip`による環境と`anaconda`環境の併用は困難です. ただし,この講義では,`pyenv`を環境構築に利用していますので, `PyMC`を利用するためのディレクトリのlocal環境にのみ`anaconda`用の環境を構築することが可能です.

まずは,ターミナルで`PyMC`を実行する用のディレクトリを作成し,そこに移動して下さい.

以下, 移動したディレクトリ内に`anaconda`環境を構築していきます.

## `anaconda`のインストール

::: warn
`MacOS`の方は `pyenv install -l` で`anaconda`系統の環境が表示されるので `pyenv install anacondaXXX` でインストール可能です.

執筆時点の最新版 `anaconda3-2024.10-1`は`PyMC5`が対応していないため,`anaconda3-2024.02-1`が推奨されます.
:::

`Windows`の場合は `pyenv` でのインストールが提供されていないので,手動でインストールする必要があります.

`anaconda`の[公式ページ](https://www.anaconda.com/download#Downloads)に行き,`Get Started`から指示に従ってアカウント等を作成して下さい.

![](/images/posts/pymc-setup/anaconda-HP.png)

続いて, Windows版の`anaconda`のインストーラーをダウンロードします.

![](/images/posts/pymc-setup/anaconda-DW.png)

ダウンロードが完了したら,インストーラーをダブルクリックして起動します.
設定は変更せず`Next`をクリックしていきます.

![](/images/posts/pymc-setup/anaconda-Explorer.png)

![](/images/posts/pymc-setup/anaconda-installer.png)

![](/images/posts/pymc-setup/anaconda-just-me.png)

インストール先の設定画面が出たら, `pyenv`の`versions`フォルダに保存します.
パスを以下のように指定して次に進みましょう

`C:\Users\xxx\.pyenv\pyenv-win\versions\anaconda`
(XXXの部分を自分のユーザー名にしましょう.)

![](/images/posts/pymc-setup/anaconda-pass.png)

その後は基本的にデフォルトの設定のまま,`Next`,`Finish`を押しましょう.

![](/images/posts/pymc-setup/anaconda-installer-end.png)

![](/images/posts/pymc-setup/anaconda-installer-end2.png)

![](/images/posts/pymc-setup/anaconda-installer-end3.png)

インストールが完了したら,ターミナルを開き作業用フォルダに移動して,`pyenv versions` コマンドで`anaconda`がインストールされているか確認しましょう.

![](/images/posts/pymc-setup/anaconda-pyenv.png)

ローカル環境に`anaconda`を指定します.

~~~ sh
pyenv local anaconda
pyenv rehash
~~~

`anaconda`では`pip`ではなく`conda`を利用してライブラリをインストールします. まずは,`anaconda`自体を`update`しましょう.

~~~ sh
conda update -n base -c defaults conda
~~~

~~~ sh
Proceed ([y]/n)? y
~~~
が表示されたら, `y` を押して`Enter`します.

続いて`PyMC5`の公式サイトに従い,`anaconda`上で`PyMC5`をインストールします.
`pyenv`が既に仮想環境ですが,`conda create`コマンドによって `pymc`などがインストールされた`python`の仮想環境を更に構築します.

~~~ sh
conda create -c conda-forge -n pymc_env "pymc>=5"
~~~

入力後完了したら,仮想環境を有効化するためにterminalを一度初期化します.

~~~ sh
conda init powershell
~~~

::: warn
`MacOS`の場合は, `conda init zsh` コマンドです.
:::


を入力し, **`Terminal`を閉じて再度作業ディレクトリに移動したあと**に仮想環境を有効化する以下のコマンドを入力して下さい.


~~~ sh
conda activate pymc_env
~~~

::: warn
Macの場合は, このあと仮想環境をlocalに指定します.

~~~ sh
pyenv versions
  system
  3.12.3
* anaconda3-2024.02-1
  anaconda3-2024.02-1/envs/pymc_env
❯ pyenv local anaconda3-2024.02-1/envs/pymc_env
❯ pyenv local
anaconda3-2024.02-1/envs/pymc_env
~~~

:::


環境の確認をします.

~~~ sh
Get-Command python
~~~

::: warn
`MacOS`の場合は, `witch python` コマンドです.
:::

以下のように`pymc_env`内のpythonが表示されれば利用可能な状況になっています.

~~~ sh
CommandType     Name                                               Version    Source
-----------     ----                                               -------    ------
Application     python.exe                                         3.11.14... C:\Users\akagi\.pyenv\pyenv-win\versions\anaconda\envs\pymc_env\python.exe
~~~

必要なライブラリをインストールします.

~~~ sh
conda install seaborn scikit-learn statsmodels
~~~

途中で,`Proceed ([y]/n)?` と表示されたら`y`と入力してENTERを押します.

~~~sh
Preparing transaction: done
Verifying transaction: done
Executing transaction: done
~~~
などが表示されれば,環境構築完了です.

以降,Terminalで作業ディレクトリに移動した後`conda activate pymc_env` でこの環境が利用できるようになります.
