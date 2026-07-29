---
title: 特別講義DS 補足B X(Twitter) APIによるデータの取得
description: 資料
tags:
    - datascience
    - statistics
    - python
featured: false
date: 2026-06-12
tableOfContents: true
previousChapter: slds_a1.html
nextChapter: slds_c1.html
---

本資料は章番号外の補足資料です. X(旧:Twitter)のAPIを利用して投稿データを取得する手順を扱います. 取得したデータを利用した分析 (ワードクラウド, トピックモデルなど) は[Ch15 自然言語処理](slds15.html)で扱っています.

APIという仕組み自体の説明 (REST, HTTPメソッド, JSONなど) は[補足A](slds_a1.html#apiとは)にまとめてあるので, 馴染みのない人は先にそちらを読んでください. `X.API`も`REST`アーキテクチャで提供されており, 本資料ではGETメソッドだけを使います.

関連する章・補足:

- [補足A EDINET APIによる財務データの取得](slds_a1.html): APIの基礎 (REST/HTTP/JSON) とAPIキーの取り扱い方
- [補足C YouTube Data APIによる動画・チャンネルデータの取得](slds_c1.html): 無料の割り当てで使える別のREST API. 料金と用途の比較は[後の節](#youtube-data-apiとの使い分け)で扱います
- [Ch15 自然言語処理](slds15.html): 取得した投稿テキストのワードクラウド・トピックモデル分析

# 利用上の注意 (料金と制限)

X APIは2026年2月6日の改定で料金体系が全面的に変わりました. **新規の開発者が使えるのは前払いクレジットによる従量課金 (pay-per-use) だけ**です. 無料の取得枠は廃止され, 月200ドルのBasicプランと月5,000ドルのProプランは既存契約者専用となり新規受付を終了しました. 2025年以前に書かれた解説記事にある「無料枠で月100件」「15分に1回」といった記述は, 現在のAPIには存在しません.

従量課金では, 先にクレジットを購入し, リクエストのたびに残高から差し引かれます. 月額の最低料金はありません. 主な操作の単価は次のとおりです.

| 操作 | 単価 |
|---|---|
| 投稿の取得 (post read) | 1件あたり0.005ドル |
| 投稿の作成 (post write) | 1件あたり0.015ドル (リンクを含む投稿は0.20ドル) |
| ユーザ情報の取得 (user lookup) | 1件あたり0.010ドル |

取得は月200万件で上限に達し, それを超えるにはEnterprise契約 (月4万ドル規模) が必要ですが, 講義や卒業研究の規模でこの上限に届くことはありません. 実際にかかる費用は取得件数から直接計算できます.

| 用途 | 取得件数 | 概算費用 |
|---|---|---|
| 授業中のデモ | 50件 | 0.25ドル |
| 演習1回 (1人あたり) | 500件 | 2.50ドル |
| 卒業研究のデータセット (1人あたり) | 5,000件 | 25ドル |

**前払いした金額がそのまま支出の上限になります.** プログラムの誤りでループが止まらなくなっても, 残高を使い切った時点でリクエストが失敗するだけで, 請求が青天井に膨らむことはありません. 少額 (10〜25ドル程度) をチャージして使うのが安全です.

::: note
本講義では**教員が契約したアカウントの認証トークンを配布**します. 受講生が各自でアカウントを登録する必要はありません. 配布されたトークンは共有の前払い残高に直結しているので, 後述する取得件数の上限を必ず守ってください.
:::

取得できる期間にも制限があります. 本資料で使う`search/recent`エンドポイントは**直近7日間の投稿**しか返しません. それ以前の投稿を取得するには全期間検索 (full-archive search) に対応した契約が必要です.

研究でデータを集める予定がない人は, 取得済みの[こちらのデータ](https://github.com/yakagika/yakagika.github.io/blob/main/slds_data/ch15/tweets.csv)をダウンロードして利用してください. [Ch15](slds15.html)の分析はこのデータで進められます.

# 発展: Grok (xAI) のX検索

xAIのAPIには, XをLLM経由で検索する`x_search`という機能があります. 1,000回の呼び出しで5ドルと単価は安く, 「あるトピックについてXでどう言われているか」を対話的に調べる用途には向いています.

ただし`x_search`が返すのは検索結果の要約と引用であり, いいね数・投稿日時・返信関係といった**構造化されたデータは取得できません**. 本資料のように1行 = 1投稿のCSVを作る用途の代わりにはならないので, データセットの作成にはX API v2を使ってください.

# 認証トークンの発行

ここからは自分でアカウントを契約する場合の手順です. 教員からトークンを受け取る受講生は[次の節](#トークンの受け渡し)まで読み飛ばして構いません.

XのAPIを利用するには, 認証トークン(`Bearer Token`)を発行します. 認証トークンとは`X.API`にアクセスするための認証情報です.

**1. 開発者アカウントを登録する.** [Xのデベロッパー用ページ](https://developer.x.com/en/portal/dashboard)からサインアップします. 利用目的を尋ねられるので, **250文字以上の英文で**回答します. 研究・教育目的であれば, その内容をそのまま書けば通ります.

![利用目的の記入欄](/images/slds/ch15/x-reason.png)

**2. クレジットをチャージする.** 従量課金は前払い制なので, ダッシュボードの課金設定からクレジットを購入します. 前節のとおりチャージ額が支出の上限になるため, まずは10〜25ドル程度から始めます.

**3. トークンを発行する.** 左のメニューの`Dashboard`に表示されている`Project APP`の`Keys and Tokens`(鍵)ボタンを押します.

![ダッシュボードのKeys and Tokens](/images/slds/ch15/xapi-dashbooard.png)

`Bearer Token`の`Regenerate`をクリックすると`Bearer Token`が表示されます. クリップボードにコピーして安全な場所に保存しましょう. このトークンは一度しか表示されません. 忘れた場合は別のトークンを再生成する必要があります.

![Bearer Tokenの発行画面](/images/slds/ch15/xapi-token.png)

::: warn
認証トークンは[補足A](slds_a1.html#apiキーの取得)のAPIキーと同様に**パスワードと同じ扱い**をしてください. 従量課金では漏洩の被害が課金に直結し, 第三者に前払い残高を使い切られます. 漏洩した場合はただちに`Regenerate`で再生成しましょう.
:::

# トークンの受け渡し

配布されたトークンは環境変数`X_BEARER_TOKEN`に入れて使います. ソースコードに直接書かず環境変数に置くことで, プログラムを提出・共有してもトークンは漏れません.

macOS・Linuxのターミナルでは次のように設定します.

~~~ sh
export X_BEARER_TOKEN='配布されたトークン'
~~~

WindowsのPowerShellでは次のように設定します.

~~~ sh
$env:X_BEARER_TOKEN = '配布されたトークン'
~~~

Python側では`os.environ`から読み出します.

~~~ py
import os

BEARER_TOKEN = os.environ.get('X_BEARER_TOKEN', '')
~~~

::: warn
`BEARER_TOKEN = 'AAAA...'`のようにトークンを直接書いたファイルは, 提出・共有・GitHubへの公開のいずれでも漏洩します. 課題として提出するプログラムでは必ず環境変数から読む形にしてください.
:::

# 利用後のトークンの削除

`export`で設定したトークンは, そのターミナルを閉じれば消えますが, 開いたまま使い続けている間は残ります. データの取得が終わったら, その場で環境変数から削除します.

macOS・Linuxのターミナルでは次のように削除します. 2行目は確認用で, 何も表示されなければ削除できています.

~~~ sh
unset X_BEARER_TOKEN
echo $X_BEARER_TOKEN
~~~

WindowsのPowerShellでは次のように削除します.

~~~ sh
Remove-Item Env:X_BEARER_TOKEN
echo $env:X_BEARER_TOKEN
~~~

`~/.zshrc`や`$PROFILE`などシェルの設定ファイルに`export X_BEARER_TOKEN=...`を書いた場合, 上のコマンドで消えるのは現在のシェルだけで, 新しいターミナルを開くたびに再設定されます. 設定ファイルからその行も削除してください. また, ターミナルに直接打った`export`の行はコマンド履歴に平文で残るので, 共有PCでは履歴からも削除してください.

::: warn
配布したトークンは講義終了後に教員側で無効化 (再生成) しますが, **教員から削除の指示があったら, 各自すみやかに上記の手順で環境変数と設定ファイルからトークンを削除してください.** 有効なトークンが残った環境は, そのまま共有残高への支出経路になります.
:::

# 取得プログラム

APIを操作するためのライブラリ`requests`と, CSVを扱う`pandas`をインストールしておきましょう.

~~~ sh
uv add requests pandas
~~~

## 検索の設定

取得の設定を定数として書き出します. `search/recent`は直近7日間の投稿を検索するエンドポイントで, 1回のリクエストで最大100件まで返します.

~~~ py
SEARCH_URL         = 'https://api.x.com/2/tweets/search/recent'
QUERIES            = ['国民民主党', '自民党']   # 検索ワード (1 語につき 1 リクエスト)
MAX_RESULTS        = 50                        # 1 語あたりの取得件数
HARD_CAP           = 100                       # 1 語あたりの上限 (安全装置)
PRICE_PER_POST_USD = 0.005                     # 投稿 1 件あたりの単価 (2026-02 改定)
OUTPUT_CSV         = 'posts.csv'
~~~

`HARD_CAP`は誤って大きな`MAX_RESULTS`を書いたときの安全装置です. 取得件数はこの値で頭打ちにし, APIが要求する下限10件も同時に満たすようにします.

~~~ py
def clamp(n: int) -> int:
    """1 リクエストあたりの取得件数を API の範囲 (10〜100) と HARD_CAP に収める."""
    return max(10, min(int(n), HARD_CAP))
~~~

## 実行前に費用を見積もる

従量課金では取得件数がそのまま費用になるので, リクエストを送る前に何件取得していくらかかるかを表示します. 単価に件数を掛けるだけです.

~~~ py
def estimate(queries: list, max_results: int) -> tuple:
    """取得予定件数と概算費用 (USD) を返す."""
    total = len(queries) * clamp(max_results)
    return total, total * PRICE_PER_POST_USD


def print_plan(queries: list, max_results: int) -> None:
    """課金の前に, 何件取得していくらかかるかを表示する."""
    total, cost = estimate(queries, max_results)
    print('=== 取得計画 ===')
    print(f'検索ワード   : {", ".join(queries)} ({len(queries)} 語)')
    print(f'1 語あたり   : {clamp(max_results)} 件 (上限 {HARD_CAP} 件)')
    print(f'合計取得件数 : {total} 件')
    print(f'概算費用     : {total} 件 x ${PRICE_PER_POST_USD} = ${cost:.2f}')
~~~

配布している`fetch_posts.py`は`--dry-run`を付けるとこの計画だけを表示して終了します. 認証トークンもネットワーク接続も使わないので, **課金される前に検索ワードと件数を確かめられます**. 付けずに実行した場合は計画を表示したうえで実行の可否を尋ね, `y`と答えたときだけAPIにリクエストを送ります.

~~~ sh
python fetch_posts.py --dry-run
~~~

## リクエストの送信

認証トークンをリクエストヘッダーに追加する関数`bearer_oauth()`を作ります. この関数で設定されたヘッダーによって`X.API`へのリクエストが認証されます.

~~~ py
def bearer_oauth(r):
    """Bearer Token 認証に必要なヘッダーを付ける."""
    r.headers['Authorization'] = f'Bearer {BEARER_TOKEN}'
    r.headers['User-Agent'] = 'v2RecentSearchPython'
    return r
~~~

指定されたURLにGETリクエストを送り, レスポンスを取得します. `response.status_code != 200`のときは例外を発生させます. 正常なレスポンスならJSONを辞書として返します.

~~~ py
def connect_to_endpoint(url: str, params: dict) -> dict:
    """GET リクエストを送り, レスポンスの JSON を返す."""
    response = requests.get(url, auth=bearer_oauth, params=params)
    response.encoding = response.apparent_encoding
    if response.status_code != 200:
        # 401 = トークンが誤っている, 403 = 残高不足, 429 = リクエスト過多
        raise RuntimeError(f'{response.status_code}: {response.text}')
    return response.json()
~~~

::: note
エラーの本文にはどの理由で失敗したかが書かれています. 残高が尽きた場合は`403`, 認証トークンが誤っている場合は`401`, 短時間にリクエストを送りすぎた場合は`429`が返るので, 失敗したらまず本文を読んでください.
:::

検索ワードを1つ受け取り, その投稿を取得します. `query`パラメータに`lang:ja -is:retweet`を付けて, 日本語かつリツイート以外に絞っています. リツイートは本文が重複するため, 分析対象からは外すのが普通です. `tweet.fields`には投稿日時といいね数などの指標を指定します. これらは投稿の取得料金に含まれており, 追加の課金は発生しません.

~~~ py
def fetch_posts(query: str, max_results: int) -> list:
    """検索ワード 1 語ぶんの投稿を取得し, JSON の data 部分を返す."""
    params = {
        'query'       : f'{query} lang:ja -is:retweet',   # 日本語かつリツイート以外
        'max_results' : clamp(max_results),
        'tweet.fields': 'created_at,public_metrics,author_id,lang',
    }
    return connect_to_endpoint(SEARCH_URL, params).get('data', [])
~~~

## CSVへの変換

取得したJSONを**1行 = 1投稿**のCSVに変換します. 列名はXの投稿の意味をそのまま表すものにします.

| 列名 | 中身 |
|---|---|
| `query` | 検索ワード |
| `text` | 投稿本文 |
| `like_count` | いいね数 |
| `retweet_count` | リツイート数 |
| `reply_count` | 返信数 |
| `impression_count` | 表示回数 (自分以外の投稿では返らないことがあり, その場合は0) |
| `created_at` | 投稿日時 |
| `post_id` | 投稿ID |
| `author_id` | 投稿者ID |

投稿1件を1行の辞書に変換します.

~~~ py
def to_row(query: str, post: dict) -> dict:
    """投稿 1 件を CSV の 1 行に変換する."""
    m = post.get('public_metrics', {})
    return {
        'query'           : query,                     # 検索ワード
        'text'            : post.get('text', ''),      # 投稿本文
        'like_count'      : int(m.get('like_count', 0)),
        'retweet_count'   : int(m.get('retweet_count', 0)),
        'reply_count'     : int(m.get('reply_count', 0)),
        'impression_count': int(m.get('impression_count', 0)),   # 取れない場合は 0
        'created_at'      : post.get('created_at', ''),
        'post_id'         : post.get('id', ''),
        'author_id'       : post.get('author_id', ''),
    }
~~~

## 全体の流れ

検索ワードを順に取得し, CSVに書き出します.

~~~ py
rows = []
for query in QUERIES:
    print(f'{query} の投稿を取得中...')
    for post in fetch_posts(query, MAX_RESULTS):
        rows.append(to_row(query, post))

df = pd.DataFrame(rows)
df.to_csv(OUTPUT_CSV, index=False, encoding='utf-8-sig')
print(f'{OUTPUT_CSV} に {len(df)} 行 ({df["query"].nunique()} ワード) を保存しました.')
~~~

この`posts.csv`は[Ch15 自然言語処理](slds15.html)のコードがそのまま読める構造です (Ch15の配布データ`tweets.csv`も同じ`query`・`text`列を持ちます).

::: note
このプログラムの完成版は[配布ページ](https://github.com/yakagika/yakagika.github.io/blob/main/slds_code/b1/)の`fetch_posts.py`にあります. `--dry-run`による費用の事前確認と, 実行前の確認プロンプトが入っています. 取得済みの[tweets.csv](https://github.com/yakagika/yakagika.github.io/blob/main/slds_data/ch15/tweets.csv)も配布しているので, トークンが無くても[Ch15](slds15.html)の分析は再現できます.
:::

## 補足C形式への変換

`posts.csv`の列名はXの投稿として素直に付けたので, そのままでは[補足C](slds_c1.html)の感情分析プログラムでは動きません. 補足Cの`ichikawa_youtube.csv`は**動画1本**を集計の単位にしており, X版ではそれに**検索ワード1語**が対応します. 同じコードを動かす場合の対応関係と変換規則は次のとおりです.

| 補足Cの列 | 補足C (YouTube) での意味 | 変換規則 (`posts.csv`から) |
|---|---|---|
| `video_id` | 動画ID (集計の単位) | `query`をそのまま入れる |
| `title` | 動画タイトル | `query`をそのまま入れる |
| `view_count` | 動画の再生数 | `impression_count`のワード単位の合計 |
| `like_count` | 動画の高評価数 | `like_count`のワード単位の合計 |
| `comment_count` | 動画のコメント数 | ワード単位の行数 |
| `comment` | コメント本文 | `text`を改名 |
| `comment_like_count` | コメントのいいね数 | `like_count`を改名 |
| `comment_published_at` | コメントの投稿日時 | `created_at`を改名 |

ワード単位の集計列は`groupby`と`transform`で埋めます. `transform`は集計結果を元の行数に戻して返すので, 同じワードの行に同じ値が入ります. これによって`groupby('video_id')`でワードごとに集約したときの動きが補足Cと一致します.

~~~ py
import pandas as pd

df = pd.read_csv('posts.csv')
g = df.groupby('query')

converted = pd.DataFrame({
    'video_id'            : df['query'],
    'title'               : df['query'],
    'view_count'          : g['impression_count'].transform('sum'),
    'like_count'          : g['like_count'].transform('sum'),
    'comment_count'       : g['query'].transform('size'),
    'comment'             : df['text'],
    'comment_like_count'  : df['like_count'],
    'comment_published_at': df['created_at'],
})
converted.to_csv('posts_youtube_format.csv', index=False, encoding='utf-8-sig')
~~~

この変換は[配布ページ](https://github.com/yakagika/yakagika.github.io/blob/main/slds_code/b1/)の`to_youtube_format.py`としても置いてあります.

## 取得したデータの分析

変換した`posts_youtube_format.csv`は補足Cの`ichikawa_youtube.csv`と同じ列を持つので, [補足Cの配布コード](https://github.com/yakagika/yakagika.github.io/blob/main/slds_code/c1/)の`sentiment_analysis.py`は入力ファイル名を書き換えるだけで動きます.

~~~ py
INPUT_CSV = 'posts_youtube_format.csv'    # ichikawa_youtube.csv から変更する
~~~

集計の単位が検索ワードになるので, 出力の意味も変わります. 円グラフは1つの検索ワードに対する投稿の感情比率を, 散布図はワードごとのポジティブ比率といいね数の合計の関係を表します. 検索ワードを3語以上にすると散布図の点が増え, ワード間の比較ができます.

ワードクラウドとトピックモデルによる分析は[Ch15 自然言語処理](slds15.html)で扱います.

# YouTube Data APIとの使い分け

[補足C](slds_c1.html)で扱うYouTube Data APIと比べると, 費用と取得できるデータの範囲が大きく異なります.

| 項目 | X API v2 | YouTube Data API v3 |
|---|---|---|
| 費用 | 従量課金のみ (取得1件0.005ドル) | 無料の割り当て1日10,000ユニット |
| 無料枠 | 無し (2026年2月に廃止) | 有り (毎日リセット) |
| 認証方式 | Bearer Token (英文250字の目的申請が必要) | APIキー (Googleアカウントがあれば即日発行) |
| 取得できる期間 | 直近7日 (全期間検索は別契約) | 投稿日で絞り込み可能 |
| 料金体系の安定性 | 2023年以降たびたび改定されている | 変更は少ない |

受講生が各自でキーを取って手を動かす演習にはYouTube Data APIが向いています. Xを使うのは, 短文の集合という形式そのものが分析対象になる場合や, 特定の話題への反応を追う場合です. その場合も費用が取得件数に比例するので, 検索ワードと件数を決めてから実行してください.
