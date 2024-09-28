---
title: 特別講義DS Ch13 教師あり/なし学習
description: 資料
tags:
    - datascience
    - statistics
    - python
featured: true
date: 2024-03-29
tableOfContents: true
previousChapter: slds12.html
nextChapter: slds14.html
---

# 教師あり/なし学習 (執筆中)


それぞれ様々な手法があり,使い分ける必要があります

教師あり学習
* 回帰
  * 重回帰ロジスティック回帰
* 決定木分析
* k-近傍法
* サポートベクターマシン
* ニューラルネットワーク
  *  パーセプトロン,畳込み,再起,etc

教師なし学習
* クラスタリング
  * 階層(ウォード法
  * 非階層(k-means法
* 主成分分析
* 確率密度推定

<br>

P3の図

### 主成分分析(Principle component analysis)
* 多数のデータが有る際に, それら全てを使うことは大変です.
  * 多次元になると可視化が困難
  * データの説明が困難
  * モデル化が困難
  * 計算が困難
* 主成分分析
  * 多数の変数の持つ情報を損なわずに圧縮する技術(次数削減)であり,予測モデル構築の前処理としてもよく使われる.
  * 主成分の分散が最大になる軸を探し,その軸に直行する軸の中で分散が最大になる軸を探していく.

<br>

P4の図

非線形SVMで天気を当ててるときに選ぶべき説明変数は?
* 先週は電力データで休日かどうかを当てることを考えたが,今度は天気を当ててみよう.
* Lightning,Lamp,Power,Airがある中でどのように説明変数を選ぶきだろうか.
* 休日と同じ変数だと,正解率はあまり良くない.
* 変数を2つ選ぶとして,今回は 4C2 = 4*3/2 = 6通りだが, 変数が増えればその組み合わせは莫⼤になる.

<br>

主成分を使って天気を当てよう.
電力データの次元削減をして, その主成分で天気を当てる非線形SVMを実行してみよう.
第1成分と第2成分で,情報の85%以上を説明できることが分かる.この2成分を利用して,SVMを実行してみる.

```python
# -*- coding: utf-8 -*-
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from sklearn.model_selection import train_test_split
# svmのパッケージ
from sklearn import svm
#主成分分析
from sklearn.decomposition import PCA
#4つある電力データを主成分分析で次元削減してみる
# データを読み込み
df = pd.read_csv("./data/energy_data.csv")
# 主成分分析のターゲットとなる電力データを抽出
target = df[['Lighting', 'Lamp', 'Power', 'Air']]
#データを標準化します
# apply メソッドは,DataFlame全体に与えた関数を適用
# axis = 1 行に対して
# axis = 0 列に対して
target = target.apply(lambda x: (x-x.mean())/x.std(), axis=0)
"""
ラムダ式を使わなければ
def standardization(x):
return (x-x.mean())/x.std()
target = target.apply(standardization(), axis=1)
"""
#主成分分析の実行
pca = PCA()
pca.fit(target)
# 寄与率
# その主成分で情報量のどの程度を説明できているか
print('第1主成分の寄与率', pca.explained_variance_ratio_[0])
print('第2主成分の寄与率', pca.explained_variance_ratio_[1])
#主成分ベクトル
feature = pca.transform(target)
# データを主成分空間に写像
# n列目が第n主成分空間への写像
feature = pca.transform(target)
plt.scatter(feature[:, 0], feature[:, 1], alpha=0.8, c=list(df['weather']))
plt.grid()
plt.xlabel('PC1')
plt.ylabel('PC2')
plt.show()
```

```
第1主成分の寄与率 0.5903597077929026
第2主成分の寄与率 0.27288683569579475
```

![](19_weather.png)

<br>

P6の図

説明変数に第1主成分と第2主成分を利用して,非線形SVMを実行(図示のプログラムはほぼ同じなので省略) .
TestDataは,それほど良くないが, TrainDataに関しては,かなり良く予測できている.

```python
#------------------------------------------------------------------
# ここからSVMに適用
#------------------------------------------------------------------
# 主成分をDFに追加
df['PC1'] = feature[:,0]
df['PC2'] = feature[:,1]
#説明変数を主成分とする
X = df[['PC1','PC2']]
#被説明変数
y = df['weather']
#トレーニングデータの作成
(train_X, test_X ,train_y, test_y) = train_test_split( X
, y
, stratify= y
, random_state = 0)
# SVMオブジェクトを定義
clf = svm.SVC(kernel='rbf')
#学習
clf.fit(train_X, train_y)
#結果の表示
print('正解率(train):', clf.score(train_X,train_y))
print('正解率(test):', clf.score(test_X,test_y))
```

```
第1主成分の寄与率 0.5903597077929026
第2主成分の寄与率 0.27288683569579475
正解率(train): 0.9130434782608695
正解率(test): 0.625
```

<br>

P7の図

### k-means法
最も広く使われているクラスタリング手法(データを類似度の高いグループに分ける手法). データをk個のグループに分割する場合は以下の手順で行われる.事前にクラスター数を指定する必要がある.
1. 入力データをプロットする
2. ランダムにk個の点をプロットする
3. 各ランダム点を,クラスター1,クラスター2,…,クラスターkの重心点とみなす.
4. 入力データの各点について,k個の重心点の中で最も近いものを選び,そのクラスターに分類する.
5. クラスター毎に重心を計算する.
6. 5.で定めたk個の重心を新しいクラスターの重心とする.
7. 4-6を設定した上限回数か,重心の移動距離が十分に小さくなるまで繰り返す.

<br>

P8の図

 これまでのデータはクラスタリングにあまり適していないので新しく配られたデータenergy.csv をクラスタリングしてみる.
* energy.csvは, 5~6⽉の1号館,研究館,体育館のデータを結合したもの.
* 建物を適切にクラスタリングできるかやってみよう.

<br>

k-means法で電力データの建物をクラスタリング
本来の教師なし学習では,事前にクラスターが分からないが,ここでは練習用として,実際のクラスタと,k-means法による結果を比較してみる.

```python
# -*- coding:utf-8 -*-
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.cluster import KMeans
# 1号館, 研究館, 体育館のデータをk-means法でクラスタリングしてみる
#データの読み込み
#単位が同じなので,標準化は必要ない
df = pd.read_csv('./data/energy.csv')
print(df)
# まずは図示してみる
# 通常は事前にクラスターが分からないことに注意
plt.scatter(df['Lighting'], df['Lamp'], alpha=0.8, c=list(df['Building']))
plt.grid()
plt.title('raw data')
plt.xlabel('Lighting')
plt.ylabel('Lamp')
plt.show()
#k-means法で分類
# init = 'random' とすると kmeans法になる
# init を指定しないとk-means++という手法になる
# k-means++ は 初期の重心を広範に取る手法でk-meansより安定した結果が得られる
# 分割するクラスタ数は3に設定
kmeans = KMeans(init='random', n_clusters=3)
#重心を計算
kmeans.fit(df[['Lighting','Lamp']])
prediction = kmeans.predict(df[['Lighting','Lamp']])
print(prediction)
# 結果を図示
# 通常は事前にクラスターが分からないことに注意
plt.scatter(df['Lighting'], df['Lamp'], alpha=0.8, c=prediction)
plt.grid()
plt.title('prediction')
plt.xlabel('Lighting')
plt.ylabel('Lamp')
plt.show()
```

P9の結果載せる

<br>

P10の図

### 階層クラスタリング
似たデータ点を集めて一つの点とみなすことを全体が一つの点になるまで繰り返すクラスタリング手法を階層クラスタリングといいます(k-means法は非階層クラスタリング).
｢似ている｣ことを表す概念に様々な距離を用います.
* ユークリッド距離
* マハラノビス距離
* コサイン類似度,etc…

階層クラスタリングは,全てのデータ点ごとの距離を計算するのであまり大きなデータには使えません(時間がかかる)
一方で,デンドログラムという結合順序を表す図を得ることで,直感的な理解が可能になります.

<br>

ユークリッド距離
* ユークリッド距離
一般的によく用いられる距離.ピタゴラスの定理で求められる.

数式

* 標準化ユークリッド距離
ユークリッド距離を標準化したもの. 各要素の重みをなくしたユークリッド距離.

数式

<br>

コサイン類似度
* n次元ベクトルの向きの類似性をcosθで表す.

数式

θ=0° → cosθ=1 ∶同じ向き
θ=90° → cosθ=0 ∶直行
θ=180° → cosθ=−1:逆向き

<br>

P13の図

クラスター同士の距離
階層クラスタリングなどでは,複数の点からなるクラスター間の距離
をどのように定義するかによっても,結果が異なります.
* 最短距離法
2つのクラスターのデータ間距離の最小のものをクラスター間の
距離とする.
計算量が少ないが,クラスターが帯状になりやすい.
* 群平均法
各クラスター同士の全てのデータの組み合わせの距離を測りそ
の平均をクラスター間距離とする.
* ウォード法
①各クラスターを結合した場合の重心(平均)と各クラスターの データとのユークリッド距離
②もともとのクラスターの重心と各クラスターのデータとのユークリッド距離
① - ② の値を距離とする.
計算量は多いが,精度が良い.

<br>

階層クラスタリングで電力データの建物をクラスタリング
k-means法と同じ分類を階層クラスタリングで行ってみる.

```python
# -*- coding:utf-8 -*-
import pandas as pd
import matplotlib.pyplot as plt
# 階層クラスタリング
from scipy.cluster.hierarchy import dendrogram, linkage, fcluster
# 1号館, 研究館, 体育館のデータを階層的クラスタリングしてみる
#データの読み込み
#単位が同じなので,標準化は必要ない
df = pd.read_csv('./data/energy.csv')
print(df)
# データが多すぎるとデンドログラムを見ても良くわからないので,
# 各建物10個程度に減らす
build1 = df[df['Building'] == 0].head(10)
research = df[df['Building'] == 1].head(10)
gym = df[df['Building'] == 2].head(10)
df = pd.concat([build1,research,gym])
df.index = range(0,30)
# まずは図示してみる
# 通常は事前にクラスターが分からないことに注意
plt.scatter(df['Lighting'], df['Lamp'], alpha=0.8, c=list(df['Building']))
plt.grid()
plt.title('raw data')
plt.xlabel('Lighting')
plt.ylabel('Lamp')
plt.show()
```

P14の結果載せる

<br>

階層クラスタリングで電力
データの建物をクラスタリ
ング
デンドログラムで各点がどのように統合されているかを確認

```python
"""
クラスタリングの実行
method で データの結合の方法を指示します
･ average 重みのない平均距離
･ centroid 重みのない重心までの距離
･ complete 最大距離
･ median 重みのある重心までの距離
･ single 最小距離
･ ward 内部平方距離
･ weighted 重みのある平均距離
metric で距離の測り方を選択します
・euclidean ユークリッド距離
・cosine コサイン類似度
・correlation 相関係数
・canberra キャンベラ距離
・chebyshev チェビシェフ距離
・cityblock 都市ブロック距離
・hamming ハミング距離
・jaccard Jaccard係数
"""
res = linkage(df[['Lighting','Lamp’]]
 , method = 'average’
 , metric = 'euclidean' )
# デンドログラムの図示
dendrogram(res)
plt.title("Dedrogram")
plt.ylabel("Threshold")
plt.show()
```

P15の結果載せる

<br>

階層クラスタリングで電力
データの建物をクラスタリ
ング
最後に結果を確認.データの数が違うので一概には言えないが,
k-meansでは上手くいっていなかった点も上手く分類できている.
```python
# クラスターの数(t)を指定して,どのクラスターにそれぞれが属するかを得る
clusters = fcluster(res, t=3, criterion='maxclust')
print(clusters)
plt.scatter(df['Lighting'], df['Lamp'], alpha=0.8, c=clusters)
plt.grid()
plt.title('prediction')
plt.xlabel('Lighting')
plt.ylabel('Lamp')
plt.show()
```

P16の結果載せる

<br>

## 自然言語処理(NPL,Natural Language Processing)

自然言語処理の流れ
文章
___
トークン化(トークナイザー)
* 文を適当な単位に分割すること
* 分割によって得られた文の構成要素をトークンと呼ぶ
* 方法としては
  * 単語分割
    * 単語に分割する
    * 日本語の場合MeCabやSudochi, Jumanなどの形態素解析ツール(品詞や活用で分類)が有名
  * 文字分割
  * サブワード分割
    * 文字分割を更に分割する (東京タワー → 東京 + タワー)
    * BERTはこれを採用
___
言語モデルによる処理
* 文章の出現しやすさを同時確率でモデル化
* p(私はパンを食べた) > p(私は家を⾷べた) ← “⾷べた” と “パン” が同じ⽂章に出現しやすい
* p(私はパンを食べた)> p(私にパンを食べた) ← “パンを⾷べた” と “は” が同じ⽂章に出現しやすい
* P(w|c)の条件の部分(c)を⽂脈と呼ぶ
* この学習にニューラルネットワークを利用
___
ベクトル化(分散表現の作成)
* トークンに対応付けたベクトル(分散表現)を作成.
___
クラスタリング
* 分散表現をクラスタ数に次元圧縮し, モデルとクラスタのラベルの損失関数を最⼩化する.

<br>

形態素解析
* 形態素解析
  * 文章を最小の意味を持つ言語単位（形態素）に分割し,それぞれの品詞を識別する処理
  * テキスト解析の前段階の処理として行われることが多い
* 手順
  * データの作成
    * PDFなどから直接処理することも可能だが,.txtデータにすると楽
    * 入力例文: 「太陽が昇る東の空が美しい」
  * テキストの前処理
    * 不要なスペースや記号を除去しテキストを処理しやすい形に整理
  * 形態素への分割
    * 文章を形態素と呼ばれる最小単位に分割
    * 「太陽」,「が」,「昇る」,「東」,「の」,「空」,「が」,「美しい」
  * 品詞のタグ付け
    * 分割された形態素に品詞情報を付与
    * 「太陽」: 名詞
    * 「が」: 助詞
    * 「昇る」: 動詞
    * 「東」: 名詞
    * 「の」: 助詞
    * 「空」: 名詞
    * 「が」: 助詞
    * 「美しい」: 形容詞

  <br>

形態素解析
* Pythonで利用可能な(日本語)形態素解析パッケージ
  * Mecab
    * 日本語のオープンソース形態素解析システム
    * pythonのライブラリとしてはmecab-python3
  * janome
    * Pythonで書かれた日本語形態素解析器
    * MecabよりインストールなどがPythonに最適化されており,インストールなどが楽
    * ただし,遅いので大規模な処理では余り使われない
* どっちでも良いが,Mecabを今回は使う
  * pip install mecab-python3==0.996.5
  * == 0.996.5 は新しいVerだとMacで利用するときに色々面倒くさいので古いバージョンを指定しています.
* ついでにWordCloudもInstallしておく.
  * pip install wordcloud

<br>

形態素解析 → ワードクラウドを試してみる.   (cf. https://rinsaka.com/python/nltk/05-wordcloud.html)

* 千葉商科大学の理念(https://www.cuc.ac.jp/about_cuc/outline/spirits/index.html )の
WordCloudを作成する.
* テキスト部分をコピペ→UTF-8のcuc.txtとしてdataフォルダに保存

P21の図

<br>

形態素解析 → ワードクラウドを試してみる.  (cf. https://rinsaka.com/python/nltk/05-wordcloud.html)

```python
from wordcloud import WordCloud
import os
import re
import MeCab as mc
def strip_CRLF_from_Text(text):
"""テキストファイルの改行，タブを削除し，形態素解析を実行する．
改行前後が日本語文字の場合は改行を削除する．
それ以外はスペースに置換する．
"""
# 改行前後の文字が日本語文字の場合は改行を削除する
plaintext = re.sub('([ぁ-んー]+|[ァ-ンー]+|[\\u4e00-\\u9FFF]+|[ぁ-んァ-ンー\\u4e00-\\u9FFF]+)(\n)([ぁ-んー]+|[ァンー]+|[\\u4e00-\\u9FFF]+|[ぁ-んァ-ンー\\u4e00-\\u9FFF]+)',
r'\1\3',
text)
# 残った改行とタブ記号はスペースに置換する
plaintext = plaintext.replace('\n', ' ').replace('\t', ' ')
return plaintext
def mecab_wakati(text):
"""
MeCabで分かち書き．
ただし品詞は名詞だけに限定．
"""
t = mc.Tagger()
# t = mc.Tagger('-d /usr/local/lib/mecab/dic/mecab-ipadic-neologd/')
node = t.parseToNode(text)
# print(node)
sent = ""
while(node):
# print(node.surface, node.feature)
if node.surface != "": # ヘッダとフッタを除外
word_type = node.feature.split(",")[0]
# 名詞だけをリストに追加する
if word_type in ["名詞"]:
sent += node.surface + " " # node.surface は「表層形」
# 動詞（の原型），形容詞，副詞もリストに加えたい場合は次の２行を有効にする
#if word_type in [ "動詞", "形容詞","副詞"]:
# sent += node.feature.split(",")[6] + " " # node.feature.split(",")[6] は形態素解析結果の
「原型」
node = node.next
if node is None:
break
return sent
# テキストファイル読み込み
f = open('data/cuc.txt', encoding='utf-8')
raw = f.read()
f.close()
print(raw)
text = strip_CRLF_from_Text(raw)
print(text)
sent = mecab_wakati(text)
print(sent)
# フォントの保存先を指定する（環境によって書き換えてください）
#font_path = "C:\\WINDOWS\\FONTS\\MEIRYO.TTC" ## Windows 版はこちら
font_path = "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc" ## Mac 版はこちら
# WordCloud画像を生成する
wc = WordCloud(max_font_size=36
,font_path=font_path
,background_color='white').generate(sent)
wc.to_file("cuc.png")
```
P22の結果載せる

<br>

WordCloud
* 今回は,頻出単語(文章中の出現回数)ほど大きく文字が表示されている.
* 重要度などの情報を与えることも可能だが,重要度を測るには他の手法が必要.
* Bertの固有単語抽出など

```python
import matplotlib.pyplot as plt
import japanize_matplotlib
from wordcloud import WordCloud
# 日本語のテキストデータ（基本計画の内容を代表するキーワード）
#最初に出てきたものほど大きく表示される
#こういったリストを別の手法で作成することが必要
japanese_text = """
公的統計, 経済統計, 国民経済計算, データ, 統計改革, 統計ニーズ, 国際比較, 統計データ, PDCA, 統計リソース,
デジタル化, 情報基盤, 統計調査, データ審査, 品質管理, ユーザー視点, 効率化, 統計作成, 政策立案, 統計行政,
報告者, 利便性, 統計システム, 可視化, 報告, 支援, モニタリング, 評価, 総合的, 品質表示, 更新, 保存, 管理,
正確性, 新たな統計, 対応, 負担軽減, 変化, 進展, 進化, 計画期間, 目標, 統計委員会, 建議, 方策, 様々な観点,
社会経済, 統計作成方法, 仕様, 整備, デジタル経済, 実態把握, 報告者負担, 利用者利便性, 統計ユーザー,
エラーチェック, 汎用ツール, 改善, 効率的, 統計リソース, 統計改革, 防止, 確保, 第Ⅳ期基本計画, 第Ⅲ期基本計画,
相互関連, 整合性, 進め方, 効果的, 活用
"""
# 日本語フォントの使用
font_path = "/System/Library/Fonts/ヒラギノ角ゴシック W3.ttc" #Mac
#font_path = "C:\\WINDOWS\\FONTS\\MEIRYO.TTC" #Win
# 日本語ワードクラウドの生成
japanese_wordcloud = WordCloud(width=800
, height=400
, background_color='white'
, font_path=font_path
, colormap='viridis').generate(japanese_text)
# 日本語ワードクラウドの表示
plt.figure(figsize=(10, 5))
plt.imshow(japanese_wordcloud, interpolation='bilinear')
plt.axis('off')
plt.show()
```

P23の結果載せる

<br>

BERTについて

BERT
* Bidirectional Encoder Representations from Transformers
* 2018年にGoogleから発表されたニューラル言語モデ ル.• 後継にELECTRAというのがある(こっちのほうが軽く高速) • SEOなど検索エンジンの精度向上に利用 • 文章をトークンに分割したものを入力として受けて,それぞれのトークンに対応するベクトルを出力する
* 既存の方法では, トークン間の関係に関してあ まり上手く処理できなかった.
* 文章に現れるトークンに応じて各トークンの関係を決めるAttention
(注意機構)を散り入れている.
* より深く文脈を考慮したトークンの分散表現を得ることができる

P25の図

<br>

穴埋め
* BERTやchatGPTが行っているのは文章の穴埋め.
* 私はりんごを[MASK]
* MASKの部分に入る文字列の確率計算をしている.
* 私はりんごを行った ← 確率低い
* 私はりんごを食べた ← 確率高い
* コーパス(文例集)から教師あり学習をして,あらゆる語彙の連なりやすさの確率を計算している.
* Wikipediaやスクローリングして取得した文章
* � 食べた 私はりんごを = コーパス中の頻度(私はりんごを食べた)
コーパス中の頻度(私はりんごを)
* � 食べた 私はりんごを = コーパス中の頻度(私はりんごを行った)
コーパス中の頻度(私はりんごを)
* 単語間の確率による距離をベクトル表現する(学習)

<br>

学習
* BERTは二段階の学習を行っている.
* 事前学習
* (日本語など)言語全般に関して大規模なテキストコーパス(Wikipediaなど)で学習
* こちらのモデルがGoogleによって公開されている(ライブラリとして利用可能)
* ファインチューニング
* 事前学習済みのBERTモデルをタスク(穴埋め,ラベリング,校正などの用途)によって追加学習さ
せる.
* タスクに関連した新たなデータセットが必要
* ラベリングをするのであれば,ラベル付けされた教師データが必要

<br>

データセット
* BERTの利用のためには,目的に応じたデータセットが必要
* 日本語データセットとして有名なもの
  * Twitter日本語評判分析データセット
  * https://www.db.info.gifu-u.ac.jp/sentiment_analysis/
  * Twitterの商品に関するポジティブ,ネガティブ,ニュートラルのラベリングデータ
* SNOW D18日本語感情表現辞書
  * 日本語を48の勘定に分類
  * 安らぎ,楽しさ親しみ,尊敬・尊さ,感謝,気持ちが良い,誇らしい,感動,喜び,悲しさ,寂しさ不満,切なさ,苦しさ,不安,憂鬱,辛さ,好き,嫌悪,恥ずかしい,焦り,驚き,怒り,幸福感,恨み,恐れ（恐縮等の意味で）,恐怖,悔しさ,祝う気持ち,困惑,きまずさ,興奮,悩み,願望,失望,あわれみ,見下し,謝罪,ためらい,不快,怠さ,あきれ,心配,緊張,妬み,憎い,残念,情けない,穏やか
* livedorニュースコーパス
  * ニュース記事をサイト別/ジャンル別に分類
* 有価証券報告書ネガポジデータセット
  * https://github.com/chakki-works/chABSA-dataset
  * TIS株式会社が公開している上場企業の有価証券報告書を用いて作成されたマルチラベルのネガポジデータセット
  *  ネガティブ, ポジティブ, ニュートラルの3ラベル

<br>

BERT利用の流れ

トークン化
* MeCab(Fugashi);PythonのMeCabライブラリ
* iadic ; MeCab用のシソーラス
___
ベクトル化
___
学習
* FacebookのPyTorchを利用とニュラルネットワーク言語モデルライブラリであるTransformersを利用
* 事前学習
  * 大規模な文章コーパスを用いて汎用的な言語のパターンを学習する
  * BERTでは, ある単語を周りの単語を予測するタスクを学習
    * ランダムに選ばれた15%のトークンをMASKという特殊トークンに置き換え, その位置のトークンを予測する(マスク付き⾔語モデル)
    * ⼊⼒された2つの⽂が連続したものであるかを学習( Next Sentence Prediction)
  * ⼤抵はこの部分はすでに⾏われたモデルを利⽤する
    * ⽇本語で有名なものは東北⼤がWikipediaの⽇本語記事ので学習したもの(cl-tohoku/bert-base-japanese-whole-word-masking)
* ファインチューニング
  * 事前学習を利⽤して,個別のタスクのラベル付きデータからタスクに特化した学習を⾏う.

<br>

P30の図

文章の穴埋め
*  今日は[MASK]に行く → MASK部分を予測
___
文章分類
* ポジティブ,ネガティブなどに文章を分類(マーケティングやレ
コメンドなど
___
マルチラベル文章分類
* ポジティブかつネガティブなど
___
固有表現抽出
* 文章から人名・組織名といった固有名詞を抽出する
___
文章校正
* Grammary的な
___
データの可視化と類似文章検索
* PCAからクラスタリング

<br>

人工知能の歴史
cf. 寺野隆雄,生成系AIの歴史・原理・現状,千葉商科大学 2023年 第1回FD 「生成系AIに関するFD」 ,2023/05/18

第1次AIブーム
* 1956年: ダートマス会議でスタート
* 汎⽤問題解決機(問題:現状と理想との差異)
* 問題の解決⼿法をプログラム
* 1960年代はじめ:機械翻訳の失敗で収斂
___
第2次AIブーム
* 1980年代はじめ:エキスパートシステム,機械翻訳
* 沢山の知識を詰め込む
* 1980年代はじめ:第5世代コンピュータープロジェクト
* 1990年代はじめ:知識獲得・脆弱性で収斂
___
第3次AIブーム
* 2010年から現在:ANN(ArYfical Neural Network)の復活
* 検索エンジン分野で発達
* ゲームでの成功(チェス,将棋,囲碁)
* パターン認識での成功(画像解析,音声解析,etc…)
___
第4次AIブーム
* 現在:生成系AI
* 言語・絵画・音声

<br>

GPU計算とCPU計算
* PCにおける計算は通常CPUによって行われます.
* これまでに実行してきたPythonプログラムは全てCPUを用いた計算.
* 一方でGPU(Graphics Processing Unit,画像処理装置,いわゆるグラボ)を利用した計算も可能
* ニューラルネットワークモデルは,GPUを用いて計算が行われることが多い.

CPU計算の特徴
* 汎⽤性: CPUは汎⽤的な計算に最適化. 様々な種類のタスクを処理可能.
* シリアル処理: CPUは⼀度に⼀つのタスクを処理するシリアル処理に適している
* 少数のコア: CPUは⽐較的少数のコアを保有.それぞれのコアが⾼速で複雑な計算を実⾏可能

GPU計算の特徴
* 特化した計算: GPUはグラフィックス処理や機械学習のような特定の種類の計算に最適化
* 並列処理: GPUは並列処理に特化.同時に多数の計算を実⾏可能
* 多数のコア: GPUには数百から数千のコアを保有.⼀度に多数の簡単な計算を実⾏可能

<br>

GPU計算でマルチラベル分類
* この講義ではGPU計算はローカル環境(それぞれのPC)で行いません.
  * WindowsとMacで環境構築が全く異なり面倒
  *  学生のPCのGPUが貧弱
  *  処理が重く,時間がかかるのでノートPCには不向き
* なので,Googleの提供しているPython計算用のSaaS Google Colaboratryを利用します.
* 今回はColaboratry上で, 文章のマルチラベル分類を試してみます.
*  マルチラベル分類
   * 選択肢の中から復数のカテゴリーを選ぶ分類
   * multi-hot ベクトル
    文章が属すカテゴリーを表す0,1の2値ベクトル
* 今回は有価証券報告書データを利用して[ネガティブ,ニュートラル,ポジティブ]に分類
  * ネガティブと判定されると[1,0,0]
  * ニュートラルと判定されると[0,1,0]

<br>

Googleのアカウントを作る

P34の図

<br>

Google Driveの設定をする
* データなどはGoogleのクラウドストレージに保存されます.
* Google Drive上にこの授業のフォルダとデータを入れるフォルダ
を作ります

P35の図

<br>

Google Driveの設定をする

P36の図

<br>

Google Driveの設定をする
今回の授業用のフォルダをクリックして中に入ります.Teamsで配布された multi_label.ipynb をドラッグアンドドロップでフォルダに保存します.

P37の図

<br>

Google Colaboratoryが利用できるようにする
* Google のアカウントにGoogle Colaboratoryをインストールします.

P38の図

<br>

Google Colaboratoryの利用
* sldsフォルダのmul9_label.ipynb をダブルクリックするとGoogle Colaboratoryが開く.
* 右上の設定をGPU計算に変更する

P39の図

<br>

Google Colaboratoryの利用
* Runtime → run all でプログラムが動く

P40の図

<br>

Google Colaboratry の利用
出てきたらGoogle Driveのアクセスを許可する

P41の図

<br>



