---
title: "PyMC の環境構築"
kind: code
open: true
---
## 環境構築 {#環境構築}

実際にプログラム上で[階層モデル](block:bayes-modeling#モデル全体像)を構築, 推定してみましょう.

`Python` においてベイズ統計学に基づいた統計モデリングを実施するためのライブラリとして `PyMC5` があります (古い Version として `PyMC3` があり全く異なる記法などを用いているので注意してください).

# []{#概要-os-別の使い分け}概要 (OS 別の使い分け) {#pymc-platform-tools}

PyMC の計算バックエンド `PyTensor` は **実行時に C コードをコンパイル** する設計で, Python の wheel だけでは完結せず, `g++`・`MKL` (BLAS/LAPACK)・`libpython` 等の非 Python 依存を必要とします. このため Windows では `pip` (および pip ベースの `uv`) 経由のインストールでは pytensor が Python fallback モードに陥り, **MCMC サンプリングが極端に低速** になります (1 draw あたり約 3 秒, 4 チェーン×2000 draws で数時間程度).

PyMC を動かす環境は, OS に応じて次のように用意します.

| OS | ツール |
|---|---|
| **Mac / Linux** | `uv` |
| **Windows** | `pixi` |

`pixi` は `uv` と同じく Rust 製・lockfile ベース・プロジェクト単位管理の高速ツールですが, **conda-forge エコシステムをバックエンドとする** 点が `uv` と異なります. conda-forge には `pytensor` 用の C コンパイラ (`m2w64-toolchain`/g++) や `MKL` などが pre-compiled binary として揃っているため, Windows でも追加のビルド環境構築なしに PyMC を高速に動作させられます. 操作感は `uv` とほぼ同じ (`pixi add`, `pixi run`, `pixi.lock`) で, PyMC 公式も現在は pixi を最も推奨しています.

# []{#mac-linux-uv-での環境構築}Mac / Linux: uv での環境構築 {#pymc-uv}

Mac または Linux では, `uv` で PyMC をインストールします.

~~~ sh
uv init -p 3.11.9 pymc-project
cd pymc-project
uv add "pymc>=5" arviz numpy pandas matplotlib scipy seaborn graphviz statsmodels scikit-learn nutpie
~~~

::: warn
- **Python のバージョン**: PyMC 5 系の wheel が安定して提供されているため `-p 3.11.9` などで 3.11 系を指定してください. Python 3.14 では一部依存パッケージのビルドが通らないことがあります.
- **arviz の trace プロットについて**: `arviz 1.0` 以降では `plot_trace` の subplot 数に上限 (40) があるため, `alpha` のような高次元変数を含める場合は `coords` で代表数名分にサブセットする必要があります ([モデル診断と可視化のコード](block:bayes-results#モデル診断と可視化)では対応済).
:::

**画像保存用ディレクトリの作成** (PyMC コードが `save_dir = 'images/'` に書き出すため, **先に作成しておく必要があります**):

~~~ sh
mkdir -p images
~~~

最終的なプロジェクト構成は以下のようになります.

~~~ sh
pymc-project/
├── images/                          # ← 作成しておく
├── hierarchical_regression.csv      # データファイル (講義配布)
├── main.py                          # PyMC コード
├── pyproject.toml
└── uv.lock
~~~

インストール後, 以下で動作確認します.

~~~ sh
uv run python -c "import pymc as pm; print(pm.__version__)"
~~~

バージョン番号 (例: `5.x.x`) が表示されれば準備完了です. スクリプトの実行は以下で行います.

~~~ sh
uv run python main.py
~~~

# []{#windows-pixi-での環境構築}Windows: pixi での環境構築 {#pymc-pixi}

## pixi のインストール {#pixi-のインストール}

PowerShell を開いて以下を実行します.

~~~ powershell
iwr -useb https://pixi.sh/install.ps1 | iex
~~~

::: warn
インストール後 **PowerShell を一度閉じて開き直し**, 以下で動作確認してください.

~~~ powershell
pixi --version
~~~
:::

## プロジェクト作成 {#プロジェクト作成}

~~~ powershell
cd $HOME\Desktop   # 任意の作業場所
pixi init pymc-project --platform win-64
cd pymc-project
~~~

::: note
**`--platform win-64` を付ける理由**

Windows on ARM (Snapdragon X 系) では conda-forge が `win-arm64` を実質サポートしていないため, 明示的に `win-64` を指定します. ARM 機でも Prism エミュレーションで x86_64 バイナリが透過実行されるので問題ありません. x86_64 Windows の方も同じコマンドで構いません (no-op).

既に `pixi init` を引数なしで実行してしまった場合は, 生成された `pixi.toml` を開き次のように修正してください.

~~~ toml
[workspace]
channels = ["conda-forge"]
platforms = ["win-64"]
~~~
:::

## ライブラリのインストール {#ライブラリのインストール}

~~~ powershell
pixi add pymc numpy pandas matplotlib seaborn arviz graphviz statsmodels scikit-learn scipy nutpie
~~~

::: note
このコマンドで PyMC 5 と PyTensor, **C/C++ コンパイラ** (`m2w64-toolchain`/g++), **MKL/BLAS/LAPACK**, **libpython** 等が一括で導入されます. これが pip/uv にはできない pixi の利点です.

ARM 機では以下の警告が出ますが, 正常動作の証なので無視して構いません.

~~~
WARN win-arm64 is not supported by the current environment,
     falling back to win-64 (emulation)
~~~
:::

::: warn
**arviz の trace プロットについて**: `arviz 1.0` 以降では `plot_trace` の subplot 数に上限 (40) があるため, `alpha` のような高次元変数を含める場合は `coords` で代表数名分にサブセットする必要があります ([モデル診断と可視化のコード](block:bayes-results#モデル診断と可視化)では対応済).
:::

## 画像保存用ディレクトリの作成 {#画像保存用ディレクトリの作成}

PyMC コードが `save_dir = 'images/'` に書き出すため, **先に作成しておく必要があります**.

~~~ powershell
New-Item -ItemType Directory -Path images
~~~

最終的なプロジェクト構成は以下のようになります.

~~~ powershell
pymc-project\
├── images\                          # ← 作成しておく
├── hierarchical_regression.csv      # データファイル (講義配布)
├── main.py                          # PyMC コード
├── pixi.toml
└── pixi.lock
~~~

## 動作確認とスクリプトの実行 {#動作確認とスクリプトの実行}

~~~ powershell
pixi run python -c "import pymc as pm; print(pm.__version__)"
~~~

バージョン番号 (例: `5.x.x`) が表示され, **`g++ not available` 等の警告が出なければ** C バックエンドが有効です. スクリプトの実行は以下で行います.

~~~ powershell
pixi run python .\main.py
~~~

::: warn
**`pixi shell` は使わない**

`pixi shell` は Windows on ARM 環境で `program not found` エラーとなる既知の不具合があります. この例では `pixi run <コマンド>` の形で実行してください. 日常使いには `pixi run` で十分です.
:::

# []{#参考-過去の環境構築方法について}(参考) 過去の環境構築方法について {#legacy-pymc-setup}

::: warn
以前の PyMC 環境を `pyenv` と `anaconda` で構築した方は, [旧版の環境構築手順 (備忘録)](/posts/2025-04-20-pymc-pyenv-setup-archive.html)を参照してアンインストールしてください.
:::


