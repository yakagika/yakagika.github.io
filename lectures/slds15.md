---
title: 特別講義DS Ch15 自然言語処理
description: 資料
tags:
    - datascience
    - statistics
    - python
featured: true
date: 2024-11-12
tableOfContents: true
previousChapter: slds14.html
nextChapter: slds16.html
---

# 自然言語処理

非構造化データの分析手法として,前章では画像データを扱いました. 本章では, テキストデータを分析する手法である**自然言語処理 (NPL; Natural Language Processing)**について解説します.

# トークン化と形態素解析
自然言語処理にも様々な手法がありますが,基本的には元の文章データをそのまま使うことはなく,文章を適切な単位に分割するなどの前処理を施します. これを**トークン化**といい,代表的な手法としては**単語分割**,**品詞分割**,**文字分割**,**サブワード分割**などがあります.

単語分割に関して元の文章が英語などの単語ごとに区切られている文章であれば

`"This is a pen"` → `"This","is","a","pen"`

という風に簡単に分割できますが,日本語のように単語間に空白などがない言語では特別な処理が必要になります.

また,英語であっても, 代名詞 `'This'`, 動詞 `is`, 冠詞 `"a"`, 名詞 `"pen"`などのように品詞分割するためには特別な処理が必要になります.

このような処理を施すために文章を最小の意味を持つ言語単位(**形態素**)に分割し,それぞれの品詞を識別処理を
**形態素解析(Morphological Analysis)**といいます.

::: note
形態素解析の基本的な手順は以下のようになります.

- データの作成
------------------------------------------------------------------

PDFやホームページなどから直接処理することも可能ですが, `.txt`や`.csv`,`.xlm`などの構造化データに変換しておくと処理が楽になります.

入力例文: `"太陽が昇る東の空が美しい"`

- テキストの前処理
------------------------------------------------------------------

不要なスペースや記号を除去しテキストを処理しやすい形に整理します.

- 形態素への分割
------------------------------------------------------------------

文章を形態素と呼ばれる最小単位に分割します.

`"太陽","が","昇る","東","の","空","が","美しい"`


- 品詞のタグ付け
------------------------------------------------------------------

分割された形態素に品詞情報を付与します.

`"太陽"`: 名詞

`"が"`: 助詞

`"昇る"`: 動詞

`"東"`: 名詞

`"の"`: 助詞

`"空"`: 名詞

`"が"`: 助詞

`"美しい"`: 形容詞

:::

日本語の形態素解析用のPythonパッケージとして有名なものには,`Mecab`や`Janome`があります.

::: note

- `Mecab`
    - 日本語のオープンソース形態素解析システム
    - pythonのライブラリとしてはmecab-python3


- `janome`
    - Pythonで書かれた日本語形態素解析器
    - MecabよりインストールなどがPythonに最適化されており,インストールなどが楽
    - ただし,遅いので大規模な処理では余り使われない

本資料では,以下形態素解析用ライブラリとして`Mecab`を利用するので

~~~ sh
pip install mecab-python3

pip install unidic-lite
~~~

を実行してインストールしておきましょう(`unidic-lite`は形態素解析用の日本語の辞書です.)

:::

形態素解析の練習用のデータとして[千葉商科大学のHPに掲載されている理念](https://www.cuc.ac.jp/about_cuc/outline/spirits/index.html)を利用してみます. テキスト部分をコピーして,`UTF-8`の`cuc.txt`ファイルを作成し,`data`フォルダに保存しましょう.

![](/images/Ch15-WordCloud1.png)

まずは,テキストデータを読み込んでみます. `pandas`などを利用して読み込むこともできますが,ここではPythonの組み込みメソッドである,`open()`でファイルを開き,`read()`で読み込み,`close()`でファイルを閉じます.

~~~ py
# ファイルを開く
f = open('data/cuc.txt', encoding='utf-8')
# ファイルを読み込む
raw = f.read()
# ファイルを閉じる
f.close()
print(raw)

"""
建学の精神と理念:有用の学術と商業道徳の涵養

巣鴨高等商業学校を創設した文学博士遠藤隆吉は、自らの志とする学府創立に当たり、「建学の趣旨」を次のように述べています。

創設者 文学博士 遠藤 隆吉
...
"""
~~~

`pandas`などではこれらの手順を一度にまとめて行ってくれていましたが,この手法で行う場合は`close()`を利用してファイルを閉じることを忘れないようにしましょう. 忘れた場合,システムのリソースが無駄に占有され,他のプログラムやソフトがファイルにアクセスできなくなるなどの問題が生じる可能性があります. 今回はファイルを読み込むだけですが,間に適用される処理が多くなるほど,`open()`と`close()`の間の対応関係が分かりづらくなります.

そこでこのように手動で`close()`を呼ぶ代わりに,Pythonでは`with文`を使うことで,インデントブロックを抜けた時に自動的にファイルを閉じるようにできます.`with文`を使うことでコードがよりシンプルかつエラーに強くなりますので,一般的にはwith文の使用が推奨されます.

~~~ py
with open('data/cuc.txt', encoding='utf-8') as f:
    raw = f.read()

print(raw)
~~~

続いて形態素解析の前の前処理を行います.
(以下の形態素解析のコードなどは[神戸学院大学 林坂ゼミの資料](https://rinsaka.com/python/nltk/05-wordcloud.html)を参考にしました.)

前処理として, テキストファイルの改行,タブ,などを削除するための関数を用意します.
文字列を正規表現操作するための標準ライブラリ`re`を`import`する必要があるので注意して下さい.


~~~ py
import re #正規表現操作のための標準ライブラリ

def strip_CRLF_from_Text(text):
    """テキストファイルの改行,タブを削除し,形態素解析を実行
    改行前後が日本語文字の場合は改行を削除する．
    それ以外はスペースに置換する．
    """
    # 改行前後の文字が日本語文字の場合は改行を削除する
    plaintext = re.sub('([ぁ-んー]+|[ァ-ンー]+|[\\u4e00-\\u9FFF]+|[ぁ-んァ-ンー\\u4e00-\\u9FFF]+)(\n)([ぁ-んー]+|[ァ-ンー]+|[\\u4e00-\\u9FFF]+|[ぁ-んァ-ンー\\u4e00-\\u9FFF]+)',
                       r'\1\3',
                       text)
    # 残った改行とタブ記号はスペースに置換する
    plaintext = plaintext.replace('\n', ' ').replace('\t', ' ')
    return plaintext

text = strip_CRLF_from_Text(raw)
print(text)

"""
建学の精神と理念:有用の学術と商業道徳の涵養  巣鴨高等商業学校を創設した文学博士遠藤隆吉は...
"""
~~~

改行やタブが消えて, 文毎にスペースで区切られた文章が作成されました. 日本語の文章の途中で改行がある場合には,改行前後を結合して1文としてまとめられていることを確認しましょう.

続いて,前処理が施された文章を`mecab`を利用して形態素解析を実施してみます.

抽出する品詞は`word_types`に`[String]`で指定します.

~~~ py
import MeCab as mc

def mecab_wakati(text,word_types = ["名詞","動詞","形容詞","副詞"]):
    #分かち書き
    t = mc.Tagger()
    #word_types = [String]で指定 ("名詞","動詞","形容詞","副詞")
    node = t.parseToNode(text)
    sent = ""
    noun = [x for x in word_types if x == "名詞"]
    others = [x for x in word_types if x in [ "動詞", "形容詞","副詞"]]
    while(node):
        if node.surface != "":  # ヘッダとフッタを除外
            word_type = node.feature.split(",")[0]
            if word_type in noun:
                 sent += node.surface + " " # node.surface は「表層形」
            if word_type in others:
                sent += node.feature.split(",")[6] + " " # node.feature.split(",")[6] は形態素解析結果の「原型」
        node = node.next
        if node is None:
            break
    return sent

sent = mecab_wakati(text)
print(sent)
"""
建学 精神 理念 有用 学術 商業 道徳 涵養 巣鴨 商業 学校 創設 し...
"""

#動詞だけ抽出
sent = mecab_wakati(text,['動詞'])
print('-'*10 + '\n', sent)
"""
 スル スル アタル ノベル イル マサル
"""
~~~

以上で文章の形態素解析は完了です.

# WordCloud

広く使われる文章の可視化手法として,文章中に利用されている単語の頻度などを基準に文字の色や大きさを変える`WordCloud`があります. 先ほど形態素解析した文章を利用して,`WordCloud`を作成してみましょう.

まず日本語を表示するためにフォントの設定を行います.

`Windwos`と`Mac`で異なりますので,自身の環境に併せて`コメントアウト`を外して下さい.

~~~ py
# フォントの保存先を指定する（環境によって書き換えてください）
font_path = "C:\\WINDOWS\\FONTS\\MEIRYO.TTC"    ## Windows 版はこちら
# font_path = "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc"  ## Mac 版はこちら
~~~

`wordcloud`を`import`して,画像を生成します.

~~~ sh
pip install wordcloud
~~~
をしてから以下を実行しましょう.

~~~ py
from wordcloud import WordCloud
wc = WordCloud(width=1000
              ,height=400
              ,background_color='white'
              , regexp=r"[\w']+" #一文字を表示
              ,font_path=font_path).generate(sent)
wc.to_file("result/fig/cuc.png")
~~~

指定した保存先(`"result/fig/cuc.png"`)に以下の画像が保存されているはずです.

![](/images/ch15-WordCloud2.png)

WordCloudでは登場する用語の頻度で大きさや色などが強調されています. 千葉商科大学の理念では,実業,道徳,などが強調されていることが分かります.

WordCloudを作成する場合`'こと'`,`'もの'`などどのような文章にも頻出する用語は削除した方が望ましいです.
その場合,`stopwords`に記述した単語が除外されるので,除外しましょう.

~~~ py
stopwords = ['こと','もの','ため']
wc = WordCloud(width=1000
              ,height=400
              ,background_color='white'
              , regexp=r"[\w']+" #一文字を表示
              ,font_path=font_path
              ,stopwords=stopwords).generate(sent)
wc.to_file("result/fig/cuc2.png")
~~~
![](/images/ch15-WordCloud3.png)


これまでのコードを整理すると以下のようになります.

~~~ py
from wordcloud import WordCloud
import re #正規表現操作のための標準ライブラリ
import MeCab as mc

def strip_CRLF_from_Text(text):
    """テキストファイルの改行,タブを削除し,形態素解析を実行
    改行前後が日本語文字の場合は改行を削除する．
    それ以外はスペースに置換する．
    """
    # 改行前後の文字が日本語文字の場合は改行を削除する
    plaintext = re.sub('([ぁ-んー]+|[ァ-ンー]+|[\\u4e00-\\u9FFF]+|[ぁ-んァ-ンー\\u4e00-\\u9FFF]+)(\n)([ぁ-んー]+|[ァ-ンー]+|[\\u4e00-\\u9FFF]+|[ぁ-んァ-ンー\\u4e00-\\u9FFF]+)',
                       r'\1\3',
                       text)
    # 残った改行とタブ記号はスペースに置換する
    plaintext = plaintext.replace('\n', ' ').replace('\t', ' ')
    return plaintext

def mecab_wakati(text,word_types = ["名詞","動詞","形容詞","副詞"]):
    #分かち書き
    t = mc.Tagger()
    #word_types = [String]で指定 ("名詞","動詞","形容詞","副詞")
    node = t.parseToNode(text)
    sent = ""
    noun = [x for x in word_types if x == "名詞"]
    others = [x for x in word_types if x in [ "動詞", "形容詞","副詞"]]
    while(node):
        if node.surface != "":  # ヘッダとフッタを除外
            word_type = node.feature.split(",")[0]
            if word_type in noun:
                 sent += node.surface + " " # node.surface は「表層形」
            if word_type in others:
                sent += node.feature.split(",")[6] + " " # node.feature.split(",")[6] は形態素解析結果の「原型」
        node = node.next
        if node is None:
            break
    return sent


with open('data/cuc.txt', encoding='utf-8') as f:
    raw = f.read()

text = strip_CRLF_from_Text(raw)

#名詞だけ抽出
sent = mecab_wakati(text,['名詞'])

# WordCloud
# フォントの保存先を指定する（環境によって書き換えてください）
font_path = "C:\\WINDOWS\\FONTS\\MEIRYO.TTC"    ## Windows 版はこちら
#font_path = "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc"  ## Mac 版はこちら

stopwords = ['こと','もの','ため']
wc = WordCloud(width=1000
              ,height=400
              ,background_color='white'
              , regexp=r"[\w']+" #一文字を表示
              ,font_path=font_path
              ,stopwords=stopwords).generate(sent)
wc.to_file("result/fig/cuc2.png")
~~~

# トピックモデル
続いて, テキスト文書の集合から潜在的なトピック(話題)を抽出するために広く利用される古典的手法である,トピックモデルを利用してみましょう.

トピックモデルでは単語の分布を使って,文章が何について話しているかを抽出します.ただし,出力は単語の集合で表されるため,そのトピックが何に関する話題かは利用者が判断する必要があります.

    - 例: トピックA: ｢経済｣｢市場｣｢投資｣ ← 経済に関するトピック
    - 例: トピックB: ｢ねこ｣｢いぬ｣｢ペット｣ ← ペットに関するトピック と解釈できる

トピックモデルにもいくつかの手法がありますが,最も一般的な実装手法の一つにLDA（Latent Dirichlet Allocation: 潜在的ディリクレ配分法）があります. LDA以外にもPLSA（Probabilistic Latent Semantic Analysis）などがあります.

LDAでは各文章をトピックの混合分布として表現します.

    -   例: 文章1: トピックA 0.5, トピックB 0.4, トピックD 0.1
    -   例: 文章2: トピックA 0.6, トピックE 0.3

LDAでは,各文書のトピック分布と各トピックの単語分布にディリクレ分布を使用します.ディリクレ分布は,確率の分布に対する分布（事前分布）として使われ,特にトピックの混合率が異なる多様な文書集合に対応できます.この過程では「ギブスサンプリング」や「変分ベイズ法」といった推論手法を使い,文書全体のトピックと単語の分布が収束するまで反復的に計算されます.ギブスサンプリングや事後分布,事前分布などに関しては, 一般化線形モデルの章で扱っています.


## X(Twitter) APIを用いたデータの取得
自然言語解析では,ワードクラウドの事例のように,まとまった文章を分析する場合もありますが,X(旧:Twitter)のつぶやきのように,短い文章の集合を扱う場合もあります. ここでは, TwitterのAPIを利用して,つぶやきを取得し,そのつぶやきに関して分析してみましょう.

::: note
`API（Application Programming Interface）`とは,ソフトウェア同士がデータや機能をやりとりするための「窓口」のようなものです.開発者はAPIを通じて,他のサービスやアプリケーションの機能を利用できるため,複雑な処理を簡単に実行したり,他のサービスと連携することができます.

APIの利用に関わる主な通信技術には, `HTTP/HTTPSプロトコル`, `REST（Representational State Transfer）`, `SOAP（Simple Object Access Protocol）`,および`WebSocket`などがあり,それぞれ異なる目的や特徴を持っています.
:::

`X.API`は`REST`アーキテクチャで提供されて降りその概要は以下のとおりです.

::: note
- `HTTP/HTTPSプロトコル`

APIの通信は通常,ウェブブラウザと同様にHTTPやHTTPSプロトコルを使って行われます.HTTPSは通信を暗号化し,セキュリティを確保するため,多くのAPIで推奨されます.

- `REST（Representational State Transfer）`

RESTは,HTTPを利用してリソース（データや機能）にアクセスするための設計原則で,最も広く使われるAPI通信の形式です.REST APIでは,`GET`,`POST`,`PUT`,`DELETE`などのHTTPメソッドを使ってデータを取得,作成,更新,削除します.シンプルかつ軽量なため,モバイルアプリやWebサービスでの利用に適しています.

- `GET`

概要: リソース（データ）の取得に使用される.

例: ユーザー情報を取得する場合,GET /users/123のように送信すると,ユーザーIDが123のデータが返されます.

特徴: サーバー上のデータを変更しない「読み取り専用」操作.

- `POST`

概要: 新しいリソースを作成するために使用される.

例: 新しい記事を投稿する場合,POST /articlesで記事データをサーバーに送信すると,新しい記事が作成されます.

特徴: サーバーにデータを送信して新しいエントリを追加する操作.

- `PUT`

概要: 既存のリソースを更新するために使用される.

例: 記事の内容を変更する場合,PUT /articles/456で新しいデータを送信し,記事ID456の内容を更新します.

特徴: 指定されたリソース全体を置き換える操作.

- `DELETE`

概要: リソースの削除に使用される.

例: 記事を削除する場合,DELETE /articles/456を実行すると,記事ID456が削除されます.

特徴: サーバーからリソースを取り除く操作.

:::


REST APIでデータをやりとりする際のデータ形式として,**`JSON（JavaScript Object Notation）`**が一般的に使用されます.JSONはシンプルで軽量なテキスト形式で,読みやすく,プログラミング言語間の互換性も高いため,多くのAPIで標準的なフォーマットとして採用されています.

~~~ json
{
    "id": 123,
    "name": "John Doe",
    "email": "johndoe@example.com"
}
~~~


これから,`X.API`にGETメソッドを利用し呟きを取得します.APIから返答されるデータはjsonですが,今回はJSONを直接触らず,これまでに扱ってきた`CSV`に変換します. APIを操作するためのライブラリ`requests`をinstallしておきましょう.

~~~ sh
pip install requests
~~~

以下の処理はX APIの無料の範囲で行っていますので,誰でも再現できますが, アカウントの登録など手間が多く,また無料版のAPIでは15分に一回しかリクエストが送れないため,データの取得には最低でも30分かかります.
社長と社名が変わってからAPI機能が物凄く使いにくくなっており,値段も高額になっています.まともに研究で利用しようと思うと月5000ドルのAPI使用料を払う必要があります.2000ドルはこの講義のために払うのが難しいので,月200ドルのBasicプランであれば利用できるアカウントは準備しますが,Basicプランでは**過去7日間の呟きしか取得できない**ので注意しましょう.

![](/images/ch15-XAPIV2Products.png)

研究で利用する人以外は完成した[こちらのデータ](https://github.com/yakagika/yakagika.github.io/blob/main/slds_data/tweet.csv)をダウンロードして利用しましょう.


XのAPIを利用するには,認証トークン(`Bearer Token`)を発行する必要があります.認証トークンとは`X.API`にアクセスするための認証情報です.

[Xのデベロッパー用ページ](https://x.com/i/flow/login?input_flow_data=%7B%22requested_variant%22%3A%22eyJyZWRpcmVjdF9hZnRlcl9sb2dpbiI6Imh0dHBzOi8vZGV2ZWxvcGVyLnguY29tL2VuL3BvcnRhbC9wcm9qZWN0cy1hbmQtYXBwcyJ9%22%7D)からまずはサインアップします.

今回は無料版を利用するので, `Sign up for Free Account`をクリックしましょう.

![](/images/ch15-X-sign-up.png)

利用目的を尋ねられるので,**250文字以上の英文で**回答しましょう. その他のチェックを入れて,次に進みます.

![](/images/ch15-X-reason.png)

左のメニューの`Dashboard`に表示されている`Project APP`の`Keys and Tokens`(鍵)ボタンを押します.

![](/images/ch15-XAPI-Dashbooard.png)

`Bearer Token`の`Regenerate`をクリックすると`Bearer Token`が表示されます. クリップボードにコピーしてどこかに保存しましょう.このトークンは一度しか表示されません.忘れた場合は別のトークンを再生成する必要があるので注意しましょう.

![](/images/ch15-XAPI-Token.png)


取得したトークンを利用してプログラムを書いていきます.

まずは認証トークンを設定します.先ほど取得した認証トークンを`bearer_token`という変数に格納し,API呼び出し時に利用します(`YOUR_BEARER_TOKEN`の部分を書き換えましょう.)

~~~ py
import requests #HTTPリクエストを送るためのライブラリです.このライブラリを使ってAPIにアクセスします.
import pandas as pd
# 認証トークン
bearer_token = 'YOUR_BEARER_TOKEN'
~~~

`X.API`の「検索」機能にアクセスするためのURLを指定します. ここでは,最近のツイートを取得するエンドポイント`search/recent`を指定しています. `search/recent`では直近7日間のつぶやきを取得できます. それ以上過去のつぶやきは`Pro`アカウントでしか取得することができません.

~~~ py
# Twitter APIのエンドポイントURL
search_url = "https://api.x.com/2/tweets/search/recent"
~~~

取得対象のキーワード（検索ワード）を指定します.このコードでは「自民党」に関するツイートを検索します.このコードでは面倒ですが,自民党の検索が終わったらこの部分を｢国民民主党｣に書き換えて再度実行します.一つのプログラムで両方取得することも可能ですが,`X.API`の無料版では,**15分に1回しかリクエストが送れない**ため,複数のワードで検索するにはコード内で15分間待機する機能を入れる必要があるので,手動で切り替えるようにしています.

`tweet_count`で各検索で取得するツイート数の上限です.無料版のAPI機能では, **1ヶ月あたり100ツイートのみアクセスできる**ので,50にすると1ヶ月に2回アクセスできます. 1度失敗すると数を大幅に減らすか,1ヶ月待つか,課金する必要があるので注意して下さい.

~~~py
# 検索ワード
word = "国民民主党"
tweet_count = 50
~~~

認証トークンをリクエストヘッダーに追加するための関数`bearer_oauth()`を作成します.この関数で設定されたヘッダーを使って,`X.API`へのリクエストが認証されます.

~~~ py
def bearer_oauth(r):
    r.headers["Authorization"] = f"Bearer {bearer_token}"
    r.headers["User-Agent"] = "v2RecentSearchPython"
    return r
~~~

指定されたURLにGETリクエストを送信し,APIのレスポンスを取得します.`response.status_code != 200`でAPIからエラーが返された場合,例外を発生させます.正常なレスポンスを受け取った場合,JSON形式でデータを返します.

~~~ py
def connect_to_endpoint(url, params):
    response = requests.get(url, auth=bearer_oauth, params=params)
    response.encoding = response.apparent_encoding
    if response.status_code != 200:
        raise Exception(response.status_code, response.text)
    return response.json()
~~~

特定の検索ワードに関するツイートを取得します. 検索パラメータを`lang:ja`に設定し,日本語のツイートに限定しています.

~~~ py
def fetch_tweets_for_party(party):
    query_params = {
        "query": f"{party} lang:ja",
        "max_results": tweet_count
    }
    json_response = connect_to_endpoint(search_url, query_params)
    tweets_data = json_response.get("data", [])
    return [tweet["text"] for tweet in tweets_data]
~~~

作成した関数を利用して,つぶやきを取得します.

~~~ py
def main():
    all_tweets = []
    print(f"{word}のツイートを取得中...")
    tweets = fetch_tweets_for_party(word)
    for tweet in tweets:
        all_tweets.append({"Word": word, "Tweet": tweet})

    if all_tweets:
        df = pd.DataFrame(all_tweets)
        df.to_csv("tweets.csv", index=False, encoding="utf-8-sig")
        print("tweets.csvにデータを書き出しました。")
    else:
        print("取得したデータがありません。")
~~~

コード全体は以下のようになります.

~~~ py
import requests
import pandas as pd

# 認証トークン
bearer_token = 'YOUR_BEARER_TOKEN'

# Twitter APIのエンドポイントURL
search_url = "https://api.x.com/2/tweets/search/recent"

# 検索ワード
word = "国民民主党"

# 各政党ごとに取得する件数
tweet_count = 50

def bearer_oauth(r):
    """
    Method required by bearer token authentication.
    """
    r.headers["Authorization"] = f"Bearer {bearer_token}"
    r.headers["User-Agent"] = "v2RecentSearchPython"
    return r

def connect_to_endpoint(url, params):
    response = requests.get(url, auth=bearer_oauth, params=params)
    response.encoding = response.apparent_encoding
    if response.status_code != 200:
        raise Exception(response.status_code, response.text)
    return response.json()

def fetch_tweets_for_party(party):
    # パラメータの設定
    query_params = {
        "query": f"{party} lang:ja",
        "max_results": tweet_count  # APIの制限で一度に取得できるのは最大100件まで
    }

    # エンドポイントに接続してデータを取得
    json_response = connect_to_endpoint(search_url, query_params)
    tweets_data = json_response.get("data", [])
    return [tweet["text"] for tweet in tweets_data]

def main():
    all_tweets = []

    # 各政党ごとのツイートを取得してデータを集約
    print(f"{word}のツイートを取得中...")
    tweets = fetch_tweets_for_party(party)
    for tweet in tweets:
        all_tweets.append({"Word": party, "Tweet": tweet})

    # データをDataFrameに変換してCSVに書き出し
    if all_tweets:
        df = pd.DataFrame(all_tweets)
        df.to_csv("tweets.csv", index=False, encoding="utf-8-sig")
        print("tweets.csvにデータを書き出しました。")
    else:
        print("取得したデータがありません。")

if __name__ == "__main__":
    main()
~~~

このコードを`自民党`,`国民民主党`で二回走らせて,取得したそれぞれ50件の呟きをまとめたデータが[こちら](https://github.com/yakagika/yakagika.github.io/blob/main/slds_data/tweet.csv)になります.

以下,このデータを利用して分析を行ってみましょう.

## トピックモデル実践

LDAによるトピックモデルを利用するためにライブラリ`gensim`と,LDAの可視化用のライブラリ`pyLDAvis`をインストールしましょう.

~~~ sh
pip install gensim pyLDAvis
~~~

`import`と形態素解析のための関数を定義しておきます.
URLは上手く形態素解析できないので,URLを削除する関数も新たに定義しています.

~~~ py
import pandas as pd
import MeCab as mc
import re
from gensim.corpora.dictionary import Dictionary
from gensim.models import LdaModel
import pyLDAvis
import pyLDAvis.gensim_models as gensimvis
import pyLDAvis.gensim

def strip_CRLF_from_Text(text):
    """テキストファイルの改行,タブを削除し,形態素解析を実行
    改行前後が日本語文字の場合は改行を削除する．
    それ以外はスペースに置換する．
    """
    # 改行前後の文字が日本語文字の場合は改行を削除する
    plaintext = re.sub('([ぁ-んー]+|[ァ-ンー]+|[\\u4e00-\\u9FFF]+|[ぁ-んァ-ンー\\u4e00-\\u9FFF]+)(\n)([ぁ-んー]+|[ァ-ンー]+|[\\u4e00-\\u9FFF]+|[ぁ-んァ-ンー\\u4e00-\\u9FFF]+)',
                       r'\1\3',
                       text)
    # 残った改行とタブ記号はスペースに置換する
    plaintext = plaintext.replace('\n', ' ').replace('\t', ' ')
    return plaintext

def mecab_wakati(text,word_types = ["名詞","動詞","形容詞","副詞"]):
    #分かち書き
    t = mc.Tagger()
    #word_types = [String]で指定 ("名詞","動詞","形容詞","副詞")
    node = t.parseToNode(text)
    sent = ""
    noun = [x for x in word_types if x == "名詞"]
    others = [x for x in word_types if x in [ "動詞", "形容詞","副詞"]]
    while(node):
        if node.surface != "":  # ヘッダとフッタを除外
            word_type = node.feature.split(",")[0]
            if word_type in noun:
                 sent += node.surface + " " # node.surface は「表層形」
            if word_type in others:
                sent += node.feature.split(",")[6] + " " # node.feature.split(",")[6] は形態素解析結果の「原型」
        node = node.next
        if node is None:
            break
    return sent

def remove_urls(text):
    # URLを検出する正規表現パターン
    url_pattern = r'http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+'
    # URLを空文字に置換して除外
    return re.sub(url_pattern, '', text)
~~~

データを読み込みます. 今回は,自民党のつぶやきだけ抽出して利用してみましょう.
(このデータを取得した次の日に国民民主党の党首の不倫騒動があったので,そのつぶやきが取れていれば面白かったのですが,残念です.)

トークナイズ(形態素解析),と削除文字の指定,削除までをまとめて行います. ここで,指定している削除文字は一度結果を見たあとで追加したものです.実際の分析では,結果とコードを何往復かして,調整する作業が必要になります.

形態素解析の前に`remove_urls()`を適用していることに注意して下さい.

~~~ py
#データの読み込み
df = pd.read_csv('data/tweets.csv')

#国民民主党のリアクションシートだけのデータを作る
akagi = df[df['Word'] == '自民党']['Tweet']

#トークナイズ
txt = [mecab_wakati(strip_CRLF_from_Text(remove_urls(x)),["名詞","動詞"]).split(' ') for x in akagi]

#削除文字の指定
stopwords = ['オモウ','イウ','イル','アル','こと']
txt = [[x for x in t if x not in stopwords] for t in txt]
~~~

前回扱った｢千葉商科大学の理念｣は単一のテキストデータでしたが,今回の分析の対象は50件のつぶやきです. このような複数のテキストを扱う際には,前処理として**出現頻度による単語の削除**がよく用いられます. 殆ど全てのテキストに出てくるような単語(数字や副詞などが多い)は特徴を抽出する際には役に立たないので削除したほうが良い場合があります. また,反対に出現が非常に稀な単語,造語や個人名なども削除したほうがいい場合があります.

実際の分析では,どの程度の頻度を基準とするかを結果を見ながら調整する必要がありますが,今回は練習なので`2文書未満にしか出現しない単語`と,`全体の50%以上に出現する単語`を削除しています.

実装は`dictionary`クラスの`filter_extremes()`メソッドを利用しています.

~~~py
#辞書の作成
dictionary = Dictionary(txt)
#出現がx文書に満たない単語と、y%以上の文書に出現する単語を極端と見做し削除する
x =2
y =0.5
dictionary.filter_extremes(no_below=x,no_above=y)
# LdaModelが読み込めるBoW形式に変換
corpus = [dictionary.doc2bow(x) for x in txt]

print(f"Number of unique tokens: {len(dictionary)}")
print(f"Number of documents: {len(corpus)}")
"""
Number of unique tokens: 200
Number of documents: 50
"""
~~~

LDAでは,事前に抽出するトピック数を決めることができます.こちらも実際には調整が必要ですが,今回は決め打ちで`3`としています.

`LDA`の結果は`pyLDAvis`によって`html`形式で出力されます.

~~~ py
#3トピックを抽出
num_topics =3
lda = LdaModel(corpus, id2word =dictionary, num_topics=num_topics, alpha=0.01)

#トピックごとに上位5単語を表示
df =pd.DataFrame()
for t in range(num_topics):
    word=[]
    for i, prob in lda.get_topic_terms(t, topn=5):
        word.append(dictionary.id2token[int(i)])
    _ = pd.DataFrame([word],index=[f'topic{t+1}'])
    df = df._append(_)

print(df.T)
"""
  topic1 topic2 topic3
0    ホロブ     こと     反対
1     報道     立憲     問題
2     日本     国民     日本
3     兵庫      国     国民
4      県     政党     クル
"""

#可視化
#PyLDAvisの実装
visualisation = pyLDAvis.gensim.prepare(lda, corpus, dictionary)
pyLDAvis.save_html(visualisation, 'result/LDA_Visualization.html')
~~~

今回は3つのトピックで,`兵庫県知事の話題(?)`,`立憲民主党や,国民民主党など政党の話題(?)`,`なにかに反対している問題(?)`などが抽出されました. あまりはっきりしていませんが,もう少しつぶやきの数を増やすと分かりやすくなるかもしれません.

出力された`LDA_Visualization.html`をクリックするとブラウザ上で確認することができます.

![](/images/ch15-LDA-result1.png)

左側には主成分分析による第1主成分,第2主成分上にマッピングされたトピックの集合が可視化されており,右側には全体のトピックにおける単語の分布が表示されています.

それぞれのトピックをクリックすることでトピックごとの単語の分布が表示されます.

![](/images/ch15-LDA-result2.png)
![](/images/ch15-LDA-result3.png)

右上のバーで調整できるラムダは,トピックモデルの結果を調整するためのパラメータです.ラムダの値が大きいほど,他のトピックにも出現する一般的な単語を除外し,トピック内の単語の特徴を強調します. 値を変化させてどのようにトピックの分布が変わるかを確認してみましょう.

コード全体は以下のようになっています.

~~~ py
import pandas as pd
import MeCab as mc
import re
from gensim.corpora.dictionary import Dictionary
from gensim.models import LdaModel
import pyLDAvis
import pyLDAvis.gensim_models as gensimvis
import pyLDAvis.gensim


def strip_CRLF_from_Text(text):
    """テキストファイルの改行,タブを削除し,形態素解析を実行
    改行前後が日本語文字の場合は改行を削除する．
    それ以外はスペースに置換する．
    """
    # 改行前後の文字が日本語文字の場合は改行を削除する
    plaintext = re.sub('([ぁ-んー]+|[ァ-ンー]+|[\\u4e00-\\u9FFF]+|[ぁ-んァ-ンー\\u4e00-\\u9FFF]+)(\n)([ぁ-んー]+|[ァ-ンー]+|[\\u4e00-\\u9FFF]+|[ぁ-んァ-ンー\\u4e00-\\u9FFF]+)',
                       r'\1\3',
                       text)
    # 残った改行とタブ記号はスペースに置換する
    plaintext = plaintext.replace('\n', ' ').replace('\t', ' ')
    return plaintext

def mecab_wakati(text,word_types = ["名詞","動詞","形容詞","副詞"]):
    #分かち書き
    t = mc.Tagger()
    #word_types = [String]で指定 ("名詞","動詞","形容詞","副詞")
    node = t.parseToNode(text)
    sent = ""
    noun = [x for x in word_types if x == "名詞"]
    others = [x for x in word_types if x in [ "動詞", "形容詞","副詞"]]
    while(node):
        if node.surface != "":  # ヘッダとフッタを除外
            word_type = node.feature.split(",")[0]
            if word_type in noun:
                 sent += node.surface + " " # node.surface は「表層形」
            if word_type in others:
                sent += node.feature.split(",")[6] + " " # node.feature.split(",")[6] は形態素解析結果の「原型」
        node = node.next
        if node is None:
            break
    return sent

def remove_urls(text):
    # URLを検出する正規表現パターン
    url_pattern = r'http[s]?://(?:[a-zA-Z]|[0-9]|[$-_@.&+]|[!*\\(\\),]|(?:%[0-9a-fA-F][0-9a-fA-F]))+'
    # URLを空文字に置換して除外
    return re.sub(url_pattern, '', text)

#------------------------------------------------------------------
# ↑ ここまで,関数定義
# ↓ ここから,データ処理
#------------------------------------------------------------------

#データの読み込み
df = pd.read_csv('data/tweets.csv')

#国民民主党のリアクションシートだけのデータを作る
akagi = df[df['Word'] == '自民党']['Tweet']

#トークナイズ
txt = [mecab_wakati(strip_CRLF_from_Text(remove_urls(x)),["名詞","動詞"]).split(' ') for x in akagi]

#削除文字の指定
stopwords = ['オモウ','イウ','イル','アル','こと']
txt = [[x for x in t if x not in stopwords] for t in txt]

#辞書の作成
dictionary = Dictionary(txt)
#出現がx文書に満たない単語と、y%以上の文書に出現する単語を極端と見做し削除する
x =2
y =0.5
dictionary.filter_extremes(no_below=x,no_above=y)
# LdaModelが読み込めるBoW形式に変換
corpus = [dictionary.doc2bow(x) for x in txt]

print(f"Number of unique tokens: {len(dictionary)}")
print(f"Number of documents: {len(corpus)}")

#3トピックを抽出
num_topics =3
lda = LdaModel(corpus, id2word =dictionary, num_topics=num_topics, alpha=0.01)

#トピックごとに上位5単語を表示
df =pd.DataFrame()
for t in range(num_topics):
    word=[]
    for i, prob in lda.get_topic_terms(t, topn=5):
        word.append(dictionary.id2token[int(i)])
    _ = pd.DataFrame([word],index=[f'topic{t+1}'])
    df = df._append(_)

print(df.T)

#可視化
#PyLDAvisの実装
visualisation = pyLDAvis.gensim.prepare(lda, corpus, dictionary)
pyLDAvis.save_html(visualisation, 'result/LDA_Visualization.html')
~~~


# ニューラル言語モデル

トピックモデルでは,単語の分布を解釈していましたが,文章自体の意味を扱っているわけでは有りません. 文章や単語の意味を利用した分析手法について見てみましょう.

本節では, 2018年にGoogleが発表したニューラル言語モデルのである**`BERT(Bidirectional Encoder Representations from Transformers)`**を利用して見ましょう(なお,BERTの後継に`ELECTRA`がありますが,資料の更新ができていません.)

BERTはなどのニューラル言語モデルは**事前学習**と**ファインチューニング**という二段階の学習を行うのが一般的です.

::: note
- 事前学習

(日本語など)言語全般に関して大規模なテキストコーパス(Wikipediaなど)で学習
こちらのモデルがGoogleによって公開されている(ライブラリとして利用可能)

- ファインチューニング

事前学習済みのBERTモデルをタスク(穴埋め,ラベリング,校正などの用途)によって追加学習させる.
タスクに関連した新たなデータセットが必要
ラベリングをするのであれば,ラベル付けされた教師データが必要

:::

BERTはコーパスを用いてどのような学習を行っているのでしょうか. 基本的にBERTが行っているのは文章の穴埋め精度を高めるための学習です.

- 私はりんごを【MASK】

という一部が隠れた文章があったとき, 【MASK】の部分に入る文字列の確率計算をしています.
通常,文章の【MASK】部分に入る文章は,それぞれ確率が異なります.

例えば,上の文章では【MASK】部分に｢行う｣｢走る｣などの動詞が続く確率よりも｢食べる｣｢買う｣｢調理する｣などの動詞が続く確率が高いと考えられます.

- 私はりんごを行った ← 確率低い
- 私はりんごを食べた ← 確率高い

人間は過去の学習から,このような確率をなんとなく判断できますが,BERTはコーパスから教師あり学習をして,あらゆる語彙の連なりやすさの確率を計算しています.

$$𝑃(食べた│私はりんごを)=\frac{(コーパス中の頻度(私はりんごを食べた))}{(コーパス中の頻度(私はりんごを))}$$

$$𝑃(食べた│私はりんごを)= \frac{(コーパス中の頻度(私はりんごを行った))}{(コーパス中の頻度(私はりんごを))}$$

大抵はすでにこのような事前学習が行われたモデルを利用し,必要があればファインチューニングをそれぞれの利用者が行います. 日本語で有名な学習済みモデルには東北大がWikipediaの日本語記事ので学習したモデル(`'tohoku-nlp/bert-base-japanese-whole-word-masking`)などがあります.

BERTのファインチューニングのためには,目的に応じたデータセットが必要となります.このデータは,トピックモデルなどで扱ってきたような分析用のデータではなく,学習用のデータです.

::: note
- データセット

日本語データセットとして良く利用されるものは以下のとおりです.

- [Twitter日本語評判分析データセット](https://www.db.info.gifu-u.ac.jp/sentiment_analysis/)

Twitterの商品に関するポジティブ,ネガティブ,ニュートラルのラベリングデータ

- [SNOW D18日本語感情表現辞書](https://www.jnlp.org/GengoHouse/snow/d18)

日本語を48の勘定に分類

> 安らぎ、楽しさ親しみ、尊敬・尊さ、感謝、気持ちが良い、誇らしい、感動、喜び、悲しさ、寂しさ不満、切なさ、苦しさ、不安、憂鬱、辛さ、好き、嫌悪、恥ずかしい、焦り、驚き、怒り、幸福感、恨み、恐れ（恐縮等の意味で）、恐怖、悔しさ、祝う気持ち、困惑、きまずさ、興奮、悩み、願望、失望、あわれみ、見下し、謝罪、ためらい、不快、怠さ、あきれ、心配、緊張、妬み、憎い、残念、情けない、穏やか

- [livedorニュースコーパス](https://www.rondhuit.com/download.html#news%20corpus)

ニュース記事をサイト別/ジャンル別に分類

  - [トピックニュース](http://news.livedoor.com/category/vender/news/)
  - [Sports Watch](http://news.livedoor.com/category/vender/208/)
  - [ITライフハック](http://news.livedoor.com/category/vender/223/)
  - [家電チャンネル](http://news.livedoor.com/category/vender/kadench/)
  - [MOVIE ENTER](http://news.livedoor.com/category/vender/movie_enter/)
  - [独女通信](http://news.livedoor.com/category/vender/90/)
  - [エスマックス](http://news.livedoor.com/category/vender/smax/)
  - [livedoor HOMME](http://news.livedoor.com/category/vender/homme/)
  - [Peachy](http://news.livedoor.com/category/vender/ldgirls/)

- [有価証券報告書ネガポジデータセット](https://github.com/chakki-works/chABSA-dataset)

TIS株式会社が公開している上場企業の有価証券報告書を用いて作成されたマルチラベルのネガポジデータセット

ネガティブ, ポジティブ, ニュートラルの3ラベル
:::

BERTをどのように利用するかは,様々な応用がありえますが, 良く利用される事例は以下のようなものです.


::: note

- BERTの利用例
---

1.  **文章の穴埋め（Masked Language Model, MLM）**

    トークンを利用して文中の一部を隠し,その隠れた部分を予測します.たとえば「今日は【MASK】に行く」という文が与えられた場合,BERTは文脈に基づいて【MASK】部分が「学校」「会社」などになると予測します.この機能により,文章の補完やオートコンプリート機能に利用できます.

2.  **文章分類**

    BERTは感情分析や話題の分類などの文章分類タスクで広く使われています.例えば,商品レビューやSNSの投稿をポジティブ・ネガティブといった感情ラベルに分類することで,マーケティング分析やレコメンドシステムの精度を向上させます.また,ニュース記事をカテゴリに分けるなど,文書分類にも応用されています.

3.  **マルチラベル文章分類**

    一部の文章は,複数の感情やカテゴリに属することがあり,BERTは「ポジティブかつネガティブ」のように複数のラベルを付与するマルチラベル分類も可能です.これにより,特定のジャンルに限らない複数の特徴や感情を同時に判別し,より高度な文章分析を可能にします.たとえば,レビューが「高評価だが高価」といった異なる側面を含む場合も,それぞれの特徴を捉えることができます.

4.  **固有表現抽出（Named Entity Recognition, NER）**

    BERTを用いて文章から特定の固有名詞を抽出できます.例えば,文中の「人名」「組織名」「地名」などの固有表現を検出し,ビジネスや医療,自然言語処理の分野で多用されます.ニュース記事から企業名や国名を抽出して情報整理を行ったり,顧客対応で企業名や製品名を抽出して対応を迅速化するなどの応用が可能です.

5.  **文章校正**

    BERTを使った校正機能は,文法チェックやスペルチェックに利用され,Grammarlyのようなサービスに応用できます.文脈を考慮した校正が可能なため,単なる誤字脱字の修正だけでなく,不自然な表現を検知し,より適切な言い回しに修正することも可能です.

6.  **データの可視化と類似文章検索**

    BERTのエンコーディング機能を使うと,文章をベクトル化し,意味の似た文章を数値的に比較できるようになります.これにより,多次元空間における文章の類似性が計算でき,例えばPCAやt-SNEで次元を減らし,クラスタリングを行ってデータを可視化できます.類似した内容の文書を自動でグループ分けしたり,ユーザーが検索したい文に近い内容の文書を瞬時に探すといった検索機能にも利用されます.

:::


## マルチラベル分類

マルチラベル分類とは選択肢の中から復数のカテゴリーを選ぶ分類手法です.
今回は有価証券報告書データを利用して{ネガティブ,ニュートラル,ポジティブ}に分類します.
ネガティブと判定されると{1,0,0},ニュートラルと判定されると{0,1,0}のようなベクトルが返ってきます.

::: note

- GPU計算とCPU計算
---
PCにおける計算は通常CPUによって行われます.
これまでに実行してきたPythonプログラムは全てCPUを用いた計算を行っていました(画像処理ではGPU計算も可能なプログラムになっていました.)
一方でGPU(Graphics Processing Unit,画像処理装置,いわゆるグラボ)を利用してプログラムを計算することも可能です.
ニューラルネットワークモデルは,その特性から単純な計算を大量に行うためGPUを用いた並列計算が行われることが多いです.

![](/images/ch15-cpu-gpu.png)

:::

これからBERTを利用してマルチラベル分類を実施してみます. ただし,ニューラルモデルを利用するにあたって,学生それぞれのノートPCでGPU計算の環境を構築することが困難なので, Googleの提供するオンライン上のPythonの実行環境である`Colaboratory`を利用します.

まずは,[Google](https://www.google.com)のサービスを利用するためのGoogleアカウントを作成しましょう(既にある人はスキップ)

![](/images/ch15-google.png)

プログラムやデータなどはGoogleのクラウドストレージであるGoogle Driveに保存されます.
Google Drive上に作業用ディレクトリを作りましょう.

![](/images/ch15-google2.png)


新規作成からフォルダを作成し,適当な名前をつけましょう. フォルダ内にはデータを保存するフォルダ`data`を作成しておきましょう.
![](/images/ch15-google3.png)

作成したフォルダにプログラムやデータをドラッグアンドドロップすることでアップロードできます.

実際にコードやデータを利用する前にGoogle Drive上で`Colaboratory`のファイルを扱えるようにしましょう.

右下`+`ボタンをクリックして,`Colaboratory`のアドオンを検索し,インストールしましょう.

![](/images/ch15-google4.png)

::: warn
インストールが完了したら一度ページを再読み込みしましょう.
:::

今回はマルチラベル用のプログラムを新規作成します. フォルダの何もない部分を右クリックして,その他から,Colaboratoryのファイル(拡張子`.ipynb`)を作成しましょう.

![](/images/ch15-google5.png)


作成したファイルをダブルクリックするとColaboratoryが起動します.
まずは,右上の設定からGPU計算が可能なように設定を変更しましょう.

![](/images/ch15-colab.png)


Colaboratoryは対話型環境になっており, プログラムを書いてブロックごとに実行します. 各ブロックの左側にある再生ボタンを押すか,`Runtime`から実行方法を選択して実行します. `Run all`をクリックすると全てのブロックが上から順に実行されます.

![](/images/ch15-colab1.png)

![](/images/ch15-colab2.png)


`!`に続けて入力することでシェルコマンドも実行可能です.

最初に,今回のプログラムで必要となるライブラリをインストールしてみましょう.

~~~ py
#ライブラリのインストール
!pip install transformers==4.18.0 fugashi==1.1.0 ipadic==1.0.0 pytorch_lightning
~~~

~~~ py
import random
import glob
import json
from tqdm import tqdm

import torch
from torch.utils.data import DataLoader
from transformers import BertJapaneseTokenizer, BertModel
import pytorch_lightning as pl

# 日本語の学習モデル
MODEL_NAME = 'tohoku-nlp/bert-base-japanese-whole-word-masking'

# ------------------------------------------------------------------
# マルチラベル文章分類用のクラス
# ------------------------------------------------------------------
class BertForSequenceClassificationMultiLabel(torch.nn.Module):

    def __init__(self, model_name, num_labels):
        super().__init__()
        # BertModelのロード
        self.bert = BertModel.from_pretrained(model_name)
        # 線形変換を初期化しておく
        self.linear = torch.nn.Linear(
            self.bert.config.hidden_size, num_labels
        )

    def forward(
        self,
        input_ids=None,
        attention_mask=None,
        token_type_ids=None,
        labels=None
    ):
        # データを入力しBERTの最終層の出力を得る。
        bert_output = self.bert(
            input_ids=input_ids,
            attention_mask=attention_mask,
            token_type_ids=token_type_ids)
        last_hidden_state = bert_output.last_hidden_state

        # [PAD]以外のトークンで隠れ状態の平均をとる
        averaged_hidden_state = \
            (last_hidden_state*attention_mask.unsqueeze(-1)).sum(1) \
            / attention_mask.sum(1, keepdim=True)

        # 線形変換
        scores = self.linear(averaged_hidden_state)

        # 出力の形式を整える。
        output = {'logits': scores}

        # labelsが入力に含まれていたら、損失を計算し出力する。
        if labels is not None:
            loss = torch.nn.BCEWithLogitsLoss()(scores, labels.float())
            output['loss'] = loss

        # 属性でアクセスできるようにする。
        output = type('bert_output', (object,), output)

        return output

# モデルとトークナイザのロード
# num_label:カテゴリー数
tokenizer = BertJapaneseTokenizer.from_pretrained(MODEL_NAME)
bert_scml = BertForSequenceClassificationMultiLabel(
    MODEL_NAME, num_labels=2
)
bert_scml = bert_scml.cuda()
~~~








