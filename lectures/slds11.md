---
title: 特別講義DS Ch11 線形回帰分析
description: 資料
tags:
    - datascience
    - statistics
    - python
featured: true
date: 2024-03-29
tableOfContents: true
previousChapter: slds10.html
nextChapter: slds12.html
---

# 線形回帰分析

相関分析では,ある変数間に関係があることを示すことができました. しかし,相関分析で示せるのは,変数Aによって変数Bが増加するか,減少するかということのみです. 具体的に,どの程度変数Aが動くことで,変数Bがどの程度変動するかを式によって**説明する**手法に**回帰分析(Regression Analysis)**があります.

また,回帰分析は検定の手法によって求められた式がどの程度信頼できるのかを検定によって確かめることも可能です.

::: note
回帰分析では,データ

$$
y = \beta_0 + \beta_1 x
$$


のような式で変数yとxの関係を説明し,この式を**回帰式,回帰方程式**と呼びます. このとき,

- 説明される変数yを **被説明変数**,**目的変数**などと呼びます.

- 説明する変数xを**説明変数**,**従属変数**などと呼びます.

- 回帰式における **$\beta_0$**のような変数に乗じられていない値を**切片**といいます.

    切片は, $x = 0$ のときのyの値を意味しています.

- 変数に乗じられている**$\beta_1$**のような値を**傾き**といい,$\beta_0,\beta_1$ などを併せて**回帰係数**といいます.

    傾きは,xが1変化した際のyの変化量を表しています.

![](/images/ch11-regression-image.png)


- 説明変数が一つの回帰式を求める分析を**単回帰分析**, 2つ以上の説明変数を用いる場合を**重回帰分析**といいます.

- yがxの線形関数である場合を **線形回帰(Linear regression)**,それ以外のものを**非線形回帰(non-linear regression)**といいます.

![](/images/ch11-regression-image2.png)


:::

## 発展: 回帰分析は何を行っているのか

回帰分析が何を行っているのかについて,単回帰で行っている最小二乗法を事例に確認していきましょう.
重回帰に関しては, 線形計画問題など異なる学習が必要になるので,今回は扱いません. あくまで,回帰というものがどのような意味であるかに関して簡単に説明します.

こちらの詳細は統計学入門で扱っていますので, この講義ではあまり深く扱いません.興味のある方は読んでみてください.

### 母回帰方程式

体重$y$を慎重$x$によって説明する回帰方程式として, $y = \beta_0 + \beta_1 x$ を考えてみます.
しかし, 実際の体重は身長以外の要素によってばらつきます. そのような**ばらつき**を考慮して,データの$i$人目の体重,身長をそれぞれ,$Y_i,X_i$として,身長以外の要素によるばらつきを$\epsilon_i$とすると,母集団において,以下のような式が立てられます.

$$
Y_i = \beta_0 + \beta_1 X_i + \epsilon_i ~(i=1,2,...,n)
$$

これを**母回帰方程式(Population Regression Equation)**と呼びます.
また, $\beta_0, \beta_1$を**母(偏)回帰係数**といい,これを**推定,検定すること**を回帰分析といいます.

### 誤差項,撹乱項

母回帰方程式

$$
Y_i = \beta_0 + \beta_1 X_i + \epsilon_i ~(i=1,2,...,n)
$$

における $\epsilon_i$は$X_i$で説明できない誤差を表す確率変数であり,**誤差項**,**撹乱項**といいます.


![](/images/ch11-regression-images3.png)

回帰分析において,誤差項は以下の仮定をおいています.

::: note
- 期待値0: $E(\epsilon_i) = 0 ~ (i=1,2,...,n)$
- 分散一定: $V(\epsilon_i) = \sigma^2 ~ (i=1,2,...,n)$
- 無相関: $i \neq j \Rightarrow Cov(\epsilon_i, \epsilon_j) = 0$
- 正規分布: $\epsilon_i \sim N(0,\sigma^2)$
:::

これによって,

$$
E(Y_i) = \beta_1 + \beta_2 X_i
$$

が得られます.

### 最小二乗法

母回帰方程式

$$
Y_i = \beta_0 + \beta_1 X_i + \epsilon_i ~(i=1,2,...,n)
$$
における母回帰係数$\beta_0, \beta_1$は観測できないので,**誤差項を最小化する母回帰係数**を統計的推測することで求めます.

この誤差項を最小化する母回帰係数の推定方法を**最小二乗法(least squares method)**といいます.

母回帰方程式を変形して,

$$
\epsilon_i = Y_i - (\beta_0 + \beta_1 X_i)
$$

が少ないほど, $X_i$による$Y_i$の説明力が上がります(モデルによってよく関係が説明できている.)

なので,モデル全体で,$\epsilon_i$を最小化することを考えてみます.

![least squares method](/images/least-squares-method1.png)

誤差の正負を打ち消すために,モデル全体の誤差項の二乗の和


$$
\begin{align*}
S & = \sum_{i=1} \epsilon_{1}^{2} \\
&= \sum_{i=1} \{Y_i - (\beta_0 + \beta_1 X_i)\}^2
\end{align*}
$$

Sを最小化する**(最小二乗)推定量** $\hat{\beta_0},\hat{\beta_1}$ を求める問題として整理できます.

$S$の偏微分を0とおいて,

$$
\frac{\partial S}{ \partial \beta_0} = -2 \sum (Y_i = \beta_0 - \beta_1 X_i) = 0 \\
\frac{\partial S}{ \partial \beta_1} = -2 \sum (Y_i = \beta_0 - \beta_1 X_i)X_i = 0 \\
$$

これを解いて,

$$
\hat{\beta_0} = \bar{Y} - \hat{\beta_1}\bar{X} \\
\hat{\beta_1} = \frac{\sum(X_i - \bar{X})(Y_i - \bar(Y))}{\sum(X_i - \bar{X})^2}
$$
が得られます.

### 回帰係数の検定

最小二乗推定量によって得られた方程式

$$
Y = \hat{\beta_0} + \hat{\beta_1}X
$$

を**標本回帰方程式**といいます.

求めた標本回帰方程式が,XとYの関係を説明できているのかを考えます. XがYを全く説明できていない場合, $\epsilon_i$ だけで説明ができるため, $\beta_1 \neq 0$ と言えれば,統計的にXがYを説明できていると言えます.

そこで, 帰無仮説 $H_0:\beta_1 = 0$ として,偏回帰係数に関する統計的仮説検定を実施します.

母数 $\beta_1$ に関する仮説検定を行うために, $\beta_1$ の確率分布を考えます.

誤差項 $\epsilon_i \sim N(o,\sigma^2)$ として,

$$
\hat{\beta_0} = \bar{Y} - \hat{\beta_1}\bar{X} \\
\hat{\beta_1} = \frac{\sum(X_i - \bar{X})(Y_i - \bar(Y))}{\sum(X_i - \bar{X})^2}
$$
であるから,

$$
V(\hat{\beta_0}) = \frac{\sigma^2 \sum X_i^2}{n\sum(X_i - \bar{X})^2} \\
E(\hat{\beta_0}) = \beta_0 \\
V(\hat{\beta_2}) = \frac{\sigma^2 }{\sum(X_i - \bar{X})^2} \\
E(\hat{\beta_1}) = \beta_1
$$
なので,

$$
\hat{\beta_1} \sim N(\beta_1,\frac{\sigma^2}{\sum(X_i - \bar{X})^2})
$$

となる.

誤差項の母標準偏差 $\sigma$ が含まれるので,推定する.

標本回帰方程式 $Y=\hat{\beta_0}+\hat{\beta_1}X$ によって求められる各 $i$ の値(回帰値)

$$
\hat{Y_i} = \hat{\beta_0} + \hat{\beta_1}X_i
$$

と実際に観測された実測値 $Y_i$との差を

$$
\hat{e_i} = Y_i - \hat{Y_i} = Y_i - \hat{\beta_0} - \hat{\beta_1}X_i
$$

を**回帰残渣(residual)**といい,Xで説明されなかった残渣を表す.

回帰残渣を母回帰方程式 $Y_i = \beta_0 + \beta_1 X_i + \epsilon_i$ における誤差項 $\epsilon_i$ の推定値として利用する.

求める必要があるのは,誤差項の分散の推定値としての分散

$$
V(\hat{e_i}) = E(\hat{e_i^2}) - (E(\hat{e_i}))^2
$$

であるが,

$$
\frac{\partial S}{\partial \beta_1} = -2 \sum (Y_i - \beta_0 - \beta_1 X_i)X_i = 0
$$
なので,

$$
\sum (Y_i - \beta_0 - \beta_1 X_i) = \sum \hat{e_i} = 0
$$

となり,

$$
\bar{e_i} = \frac{1}{n} \sum \hat{e_i} = 0
$$

なので,

$$
\begin{align*}
V(\hat{e_i}) &= E(\hat{eI^2}) \\
&= \frac{1}{n-2}\sum (\hat{e_i}^2 - \bar{e_i}^2) \\
&= \frac{\sum \hat{e_i}^2}{n-2}
\end{align*}
$$

となります.

これを誤差項の分散$\sigma^2$の推定値

$$
S^2 = \frac{\sum \hat{e_i}^2}{n-2}
$$
として利用します.

なお,この累乗根は回帰式がどの程度実測値に当てはまっているかを表す,**推定値の標準誤差(standard error of estimates)**と呼ばれます.

$$
s.e. = \sqrt{S^2} = \sqrt{\frac{\sum \hat{e_i}^2}{n-2}}
$$

これを誤差項の母標準偏差$\sigma$の推定値として利用して,$\hat{\beta_2}$の標準誤差の推定値は,

$$
V(\hat{\beta_1}) = \frac{\sigma^2 }{\sum(X_i - \bar{X})^2}
$$

から,

$$
s.e.(\hat{\beta_1}) = \frac{s.e.}{\sqrt{\sum(X_i - \bar{X})^2}}
$$

となり,$s.e.(\hat{\beta_2})$を用いて標準化した値は, $t(n-2)$に従うので,

$$
t_2 = \frac{\hat{\beta_1} - \beta_1}{s.e.(\hat{\beta_1})} \sim t(n-2)
$$

あとは得られた推定量を用いて,帰無仮説に関するt検定を実施する.

## 単回帰分析

それでは,実際に単回帰分析を実施してみる. 事例として,以下の分析にどのような問題点があるのかを考えてみよう.

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">今回に限らず毎度毎度のことだけど、日本「経済」新聞を名乗りながら、お粗末な統計リテラシーだからな。。。<br>日経に限らず、本邦の大手紙は例外なく全てお粗末な統計リテラシー。<br>統計の基礎を勉強したい人はツッコミを入れると良いよ。ツッコミを入れるところだらけで勉強になるから。<a href="https://twitter.com/IsayaShimizu?ref_src=twsrc%5Etfw">@IsayaShimizu</a> <a href="https://t.co/cxMAFDPRfM">https://t.co/cxMAFDPRfM</a></p>&mdash; 糸石 浩司 (@itoishi) <a href="https://twitter.com/itoishi/status/1445026338885103618?ref_src=twsrc%5Etfw">October 4, 2021</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>

<blockquote class="twitter-tweet"><p lang="ja" dir="ltr">相関があることを言いたいなら散布図を描くべきでは？ あと，この16カ国はどうやって選んだのか？ <a href="https://t.co/ncWWtQ8A8L">https://t.co/ncWWtQ8A8L</a></p>&mdash; Haruhiko Okumura (@h_okumura) <a href="https://twitter.com/h_okumura/status/1444983348862996485?ref_src=twsrc%5Etfw">October 4, 2021</a></blockquote> <script async src="https://platform.twitter.com/widgets.js" charset="utf-8"></script>


これらのPOSTは何を懸念しているのでしょうか.

元POSTでは, 日本経済新聞が[OECD Family Database](https://www.oecd.org/en/data/datasets/oecd-family-database.html )のデータから作成したグラフを用いて男性の育児・家事時間の少なさが日本の少子化の原因であると主張しています.

![[出典：日本経済新聞,「男性の育児・家事時間、出生率に影響　日本は女性の二割](https://www.nikkei.com/article/DGXZQOFE308UG0Q1A830C2000000/) ](/images/ch11-oecd-nikkei.png)

::: note
この分析の問題点は主に以下の2つです.

1. 元のPOSTの通り,この記事ではグラフを主張の根拠としていますが,棒グラフと散布図を組み合わせたような独自のグラフを作成しており,これを持って何が主張できるのかが明確ではありません. 2つの量的データの関係性を示す場合は散布図が適当です.

2. また,グラフが適当であったとしても,グラフはデータの概観の把握にや役立ちますが,グラフのみから明確な主張が主張できるわけではない場合が多いです.

:::

まずは,グラフに関して考えてみましょう. 実は,内閣府もこのデータを利用して同様の主張をしています. 内閣府のグラフでは,独自のグラフではなく,散布図が作成されています.

![[出典：内閣府　政策統括官（経済社会システム担当）第４回会議資料　選択する未来2.0, 資料２　参考資料②(事務局資料)](https://www5.cao.go.jp/keizai2/keizai-syakai/future2/20200409/shiryou2.pdf)
](/images/ch11-oecd-cabinet.png)

内閣府の資料でも,同じデータを利用して,同じ主張をしています. しかし,こちらの資料では独自のグラフではなく,散布図を作成しており,追加的に回帰式($y=0.0132x + 1.2027$)及びR2も求めています.

::: note
しかし,先程説明した通り,**回帰分析**とは回帰式に関する検定であり,回帰式のみをもって,y(合計特殊出生率)にx(育児・家事労働時間割合)が影響を及ぼしているとは**言えません**.
:::

では,実際に回帰分析を実施してみるとどのような結果になるでしょうか?

### データの準備

まずはデータを準備する必要があります. [OECD Family Database](https://www.oecd.org/en/data/datasets/oecd-family-database.html)から,`SF2.1 Fertility rates`及び`LMF2.5 Time used for work, care and daily household chores`のExcelファイルをダウンロードしました.
それぞれから[`fertility_rates.csv`](https://github.com/yakagika/yakagika.github.io/blob/main/slds_data/fertility_rates.csv)及び,[`time_used.csv`](https://github.com/yakagika/yakagika.github.io/blob/main/slds_data/time_used.csv)を作成しました.以下これらのデータをダウンロードして,作業を進めてください.

::: warn
可能であれば,練習として以下のコードは見ないで,元の内閣府の資料から何をする必要があるのか,自分でどのように計算するかを考えて実行してみましょう.
:::

まずはデータをそれぞれ読み込んでみましょう.

~~~ py
import pandas as pd

df_f = pd.read_csv('fertility_rates.csv')
df_t = pd.read_csv('time_used.csv')

print(df_f)
print(df_t)

"""
            Country  Fertility
0             Korea       0.92
1             Spain       1.23
2             Italy       1.27
3             Japan       1.36
4            Poland       1.42
5          Portugal       1.43
6         Lithuania       1.61
7        Luxembourg       1.34
8            Canada       1.47
9            Greece       1.34
10          Finland       1.35
11          Austria       1.46
12      Switzerland       1.48
13   United Kingdom       1.63
14       Costa Rica       1.63
15            Chile       1.55
16           Norway       1.53
17           Latvia       1.61
18          Germany       1.54
19     OECD average       1.60
20          Hungary       1.49
21          Belgium       1.60
22          Estonia       1.66
23      Netherlands       1.57
24  Slovak Republic       1.57
25      New Zealand       1.72
26         Slovenia       1.61
27    United States       1.71
28           Sweden       1.70
29        Australia       1.67
30          Türkiye       1.88
31         Colombia       1.77
32          Ireland       1.70
33          Denmark       1.70
34           France       1.83
35          Iceland       1.75
36           Mexico       1.92
37   Czech Republic       1.71
38           Israel       3.01
           Country  Paid  Unpaid  Care
0        Australia  20.3    10.5   2.2
1          Austria  22.5     9.4   1.5
2          Belgium  15.6     9.7   0.8
3           Canada  22.1    10.3   1.7
4          Estonia  21.0    11.5   1.8
5          Finland  15.4    11.2   1.0
6           France  14.5     9.7   1.0
7          Germany  17.4     9.8   1.4
8            Italy  14.5     8.2   1.2
9            Japan  26.3     4.7   0.5
10           Korea  26.5     3.0   0.8
11          Latvia  24.2     8.8   1.2
12          Mexico  30.0     4.4   3.5
13     New Zealand  19.9    11.3   1.1
14          Norway  18.6    11.7   1.6
15          Poland  21.7     6.2   1.5
16        Slovenia  18.9    11.4   1.6
17           Spain  14.8     8.9   2.0
18          Sweden  20.1    10.0   1.8
19          Turkey  20.2     2.6   3.5
20  United Kingdom  20.1     9.5   1.6
21   United States  20.4    10.2   1.6
"""

~~~


確認してみると,政府のデータは国ごとに異なる年代を使用しており,年代も国の選択もOECDのデータベースで公開されているものとは異なるようです.実際に,日本経済新聞のデータとも値が異なっていることから,おそらく公開されていないデータを独自に集計しているようです.
今回は日本経済新聞で採用されている値から2019年の出生率及びデータベースで公開されている1999-2013年の家事・育児時間を採用します.


続いて,政府のグラフに合わせて男性の労働時間,家事及び育児の時間(Paid,Unpaid,Care)に占める家事及び育児(Unpaid, Care)の割合を計算します.

~~~ python
df_t['r'] = df_t[['Unpaid','Care']].sum(axis=1) / df_t[['Paid','Unpaid','Care']].sum(axis=1)
print(df_t)

"""
           Country  Paid  Unpaid  Care         r
0        Australia  20.3    10.5   2.2  0.384848
1          Austria  22.5     9.4   1.5  0.326347
2          Belgium  15.6     9.7   0.8  0.402299
3           Canada  22.1    10.3   1.7  0.351906
4          Estonia  21.0    11.5   1.8  0.387755
5          Finland  15.4    11.2   1.0  0.442029
6           France  14.5     9.7   1.0  0.424603
7          Germany  17.4     9.8   1.4  0.391608
8            Italy  14.5     8.2   1.2  0.393305
9            Japan  26.3     4.7   0.5  0.165079
10           Korea  26.5     3.0   0.8  0.125413
11          Latvia  24.2     8.8   1.2  0.292398
12          Mexico  30.0     4.4   3.5  0.208443
13     New Zealand  19.9    11.3   1.1  0.383901
14          Norway  18.6    11.7   1.6  0.416928
15          Poland  21.7     6.2   1.5  0.261905
16        Slovenia  18.9    11.4   1.6  0.407524
17           Spain  14.8     8.9   2.0  0.424125
18          Sweden  20.1    10.0   1.8  0.369906
19          Turkey  20.2     2.6   3.5  0.231939
20  United Kingdom  20.1     9.5   1.6  0.355769
21   United States  20.4    10.2   1.6  0.366460

"""
~~~

`df_t`と`df_f`は表示されている国が異なるので,`df_t`から情報を抽出します.

~~~ py
#照合のためにCountryをindexにします.
df_t.set_index('Country',inplace=True,drop=True)
df_f.set_index('Country',inplace=True,drop=True)
df_f['r'] = pd.NA

for i in df_f.index:
    try:
        df_f.at[i,'r'] = df_t.at[i,'r']
    except:
        pass

print(df_f)

"""
                 Fertility         r
Country
Korea                 0.92  0.125413
Spain                 1.23  0.424125
Italy                 1.27  0.393305
Japan                 1.36  0.165079
Poland                1.42  0.261905
Portugal              1.43      <NA>
Lithuania             1.61      <NA>
Luxembourg            1.34      <NA>
Canada                1.47  0.351906
Greece                1.34      <NA>
Finland               1.35  0.442029
Austria               1.46  0.326347
Switzerland           1.48      <NA>
United Kingdom        1.63  0.355769
Costa Rica            1.63      <NA>
Chile                 1.55      <NA>
Norway                1.53  0.416928
Latvia                1.61  0.292398
Germany               1.54  0.391608
OECD average          1.60      <NA>
Hungary               1.49      <NA>
Belgium               1.60  0.402299
Estonia               1.66  0.387755
Netherlands           1.57      <NA>
Slovak Republic       1.57      <NA>
New Zealand           1.72  0.383901
Slovenia              1.61  0.407524
United States         1.71   0.36646
Sweden                1.70  0.369906
Australia             1.67  0.384848
Türkiye               1.88      <NA>
Colombia              1.77      <NA>
Ireland               1.70      <NA>
Denmark               1.70      <NA>
France                1.83  0.424603
Iceland               1.75      <NA>
Mexico                1.92  0.208443
Czech Republic        1.71      <NA>
Israel                3.01      <NA>

"""
~~~

`df_t`にデータの無い国を除外します.

~~~ py
#NAを削除
df_f.dropna(how='any',inplace=True)
print(df_f)

"""
                Fertility         r
Country
Korea                0.92  0.125413
Spain                1.23  0.424125
Italy                1.27  0.393305
Japan                1.36  0.165079
Poland               1.42  0.261905
Canada               1.47  0.351906
Finland              1.35  0.442029
Austria              1.46  0.326347
United Kingdom       1.63  0.355769
Norway               1.53  0.416928
Latvia               1.61  0.292398
Germany              1.54  0.391608
Belgium              1.60  0.402299
Estonia              1.66  0.387755
New Zealand          1.72  0.383901
Slovenia             1.61  0.407524
United States        1.71   0.36646
Sweden               1.70  0.369906
Australia            1.67  0.384848
France               1.83  0.424603
Mexico               1.92  0.208443
"""

~~~

全てのデータが揃った国はこの21ヵ国となりました.

::: note
このとき問題となるのが,どの国を採用するかです.
元のPOSTにも指摘がありましたが,内閣府の計算ではなぜかメキシコが除外されていますので,それに従って実行してみます.
:::

~~~ py
#メキシコを除外
df_f.drop('Mexico',axis=0,inplace=True)
print(df_f)

"""
               Fertility         r
Country
Korea                0.92  0.125413
Spain                1.23  0.424125
Italy                1.27  0.393305
Japan                1.36  0.165079
Poland               1.42  0.261905
Canada               1.47  0.351906
Finland              1.35  0.442029
Austria              1.46  0.326347
United Kingdom       1.63  0.355769
Norway               1.53  0.416928
Latvia               1.61  0.292398
Germany              1.54  0.391608
Belgium              1.60  0.402299
Estonia              1.66  0.387755
New Zealand          1.72  0.383901
Slovenia             1.61  0.407524
United States        1.71   0.36646
Sweden               1.70  0.369906
Australia            1.67  0.384848
France               1.83  0.424603
"""
~~~

`Fertility`は`%`になっているので単位を揃えて`r`も`%`表示にし,データ型も`float`にしておきます.

~~~ py
df_f['r'] = (df_f['r'] * 100).astype('float')
print(df_f)

"""
                Fertility          r
Country
Korea                0.92  12.541254
Spain                1.23  42.412451
Italy                1.27  39.330544
Japan                1.36  16.507937
Poland               1.42  26.190476
Canada               1.47  35.190616
Finland              1.35  44.202899
Austria              1.46  32.634731
United Kingdom       1.63  35.576923
Norway               1.53  41.692790
Latvia               1.61  29.239766
Germany              1.54  39.160839
Belgium              1.60  40.229885
Estonia              1.66  38.775510
New Zealand          1.72  38.390093
Slovenia             1.61  40.752351
United States        1.71  36.645963
Sweden               1.70  36.990596
Australia            1.67  38.484848
France               1.83  42.460317
"""
~~~

### 散布図の作成

データが完成したので,散布図を作成してみます. ラベルを表示するためにライブラリ`adjustText`を利用しています. この方法で作成する場合は`pip install`してください.

~~~ py
import matplotlib.pyplot as plt
from adjustText import adjust_text

plt.figure(figsize=(10, 6))
plt.scatter(df_f['r'],df_f['Fertility'])
text = [plt.text(df_f.at[c,'r'],df_f.at[c,'Fertility'],c) for c in df_f.index]
adjust_text(text, arrowprops=dict(arrowstyle='-', color='gray', lw=0.5))

plt.xlim=(0,50)
plt.ylim=(0,2)
plt.ylabel('Fertility')
plt.xlabel('Unpaid and Care Work')
plt.title('Fertility vs Unpaid and Care Work with Country Labels')
plt.grid()
plt.show()
~~~

![](/images/ch11_oecd_scatter_no_line.png)

おおよそ政府と同じグラフが完成しました.

### 単回帰

つづいて,これらのデータを利用して単回帰分析を実施してみます. 回帰分析が可能なライブラリはいくつかありますが,ここでは`statsmodels.api`を利用してみます. `pip install statsmodels` をしておきましょう.

まずは,切片 $\beta_0$ として定数1をデータに追加します. `statsmodels`では `.add_constant()`メソッドで追加できます.

~~~ py
import statsmodels.api as sm

# 説明変数(X)と目的変数(y)に分割
X = df_f[['r']]
y = df_f['Fertility']


# 切片(定数項)を追加
X = sm.add_constant(X)

print(X)

"""
                const          r
Country
Korea             1.0  12.541254
Spain             1.0  42.412451
Italy             1.0  39.330544
Japan             1.0  16.507937
Poland            1.0  26.190476
Canada            1.0  35.190616
Finland           1.0  44.202899
Austria           1.0  32.634731
United Kingdom    1.0  35.576923
Norway            1.0  41.692790
Latvia            1.0  29.239766
Germany           1.0  39.160839
Belgium           1.0  40.229885
Estonia           1.0  38.775510
New Zealand       1.0  38.390093
Slovenia          1.0  40.752351
United States     1.0  36.645963
Sweden            1.0  36.990596
Australia         1.0  38.484848
France            1.0  42.460317
"""
~~~

回帰分析を実施するには `OLS(y,X).fit()`を利用します. `OLS`は`Ordinary Least Squares`の頭文字で最小二乗法を意味しています.

回帰分析の結果は`.result()`メソッドで確認します.

~~~ py
# 回帰モデルを作成・フィット
result = sm.OLS(y, X).fit()

# 結果を表示
print(result.summary())

"""
                            OLS Regression Results
==============================================================================
Dep. Variable:              Fertility   R-squared:                       0.282
Model:                            OLS   Adj. R-squared:                  0.243
Method:                 Least Squares   F-statistic:                     7.085
Date:                Sat, 29 Mar 2025   Prob (F-statistic):             0.0159
Time:                        13:27:03   Log-Likelihood:                 6.4233
No. Observations:                  20   AIC:                            -8.847
Df Residuals:                      18   BIC:                            -6.855
Df Model:                           1
Covariance Type:            nonrobust
==============================================================================
                 coef    std err          t      P>|t|      [0.025      0.975]
------------------------------------------------------------------------------
const          1.0390      0.183      5.666      0.000       0.654       1.424
r              0.0134      0.005      2.662      0.016       0.003       0.024
==============================================================================
Omnibus:                        3.229   Durbin-Watson:                   0.868
Prob(Omnibus):                  0.199   Jarque-Bera (JB):                2.595
Skew:                          -0.852   Prob(JB):                        0.273
Kurtosis:                       2.539   Cond. No.                         161.
==============================================================================
"""
~~~

様々な数値が出てきましたが,単回帰において注目すべきは,`(Adj.)R-squared`,`Prob (F-statistic)`,`coef`,`P>|t|`,`[0.025      0.975]`です.
それぞれ何を見ればよいのかを順に見ていきましょう.

### 回帰係数(P値,区間推定値)

~~~ py
"""
                coef    std err          t      P>|t|      [0.025      0.975]
------------------------------------------------------------------------------
const          1.0390      0.183      5.666      0.000       0.654       1.424
r              0.0134      0.005      2.662      0.016       0.003       0.024
"""
~~~

回帰係数から今回推定された標本回帰方程式($y=\hat{\beta}_0+\hat{\beta}_1x$)は

$$
y=1.0390+0.0134x
$$

となります.
この $\hat{\beta}_0$  と $\hat{\beta}_1$  に対して$H_0: \beta_i =0,H_1: \beta_i \neq 0$ で検定した結果が,`P>|t|`として示されています.
また, $\hat{\beta}_0$  と $\hat{\beta}_1$  の下側信頼限界が`0.025`,上側信頼限界が`0.975`として示されています.
2つの値は基本的に同じことを違う方法で表しています.

::: note
それぞれの説明変数のP値(`P>|t|`)は,その説明変数に対する回帰係数が0である(影響がない)という仮説に対する仮説検定のP値を表しています. また, `[0.025      0.975]`はそれぞれ上側と下側の95%信頼区間を表しています.

有意水準5%の場合, `P値 < 0.05`で有意となり, 信頼区間が0をまたぎません.
(ここで両側検定ですが,`P値 < 0.025`とならないのは,両側の外側累積確率を合算した値が算出されるためです.)
したがって,これらの値はいずれも同じ判断の基準となりますが,最近は論文などには両方載せることが主流です.
(仮説検定や区間推定,P値の意味などに関しては,統計学入門で詳細を扱っています.分からない人はそちらを履修しましょう.)
:::

**｢P値 < 0.05｣ かつ ｢下側信頼限界と上側信頼限界が0をまたいでいない｣**場合には,それぞれの回帰係数が有意となります.

したがって今回の回帰分析の結果としては, いずれの回帰係数も有意であり,出生率に影響を及ぼしていると考えることができます.

あまり良くない回帰分析の方法として,有意でない回帰係数が混じっていても,有意な回帰係数のみを分析対象としている場合がしばしばありますが,基本的には,  式を構成するすべての回帰係数が有意である場合にはじめてその式は利用できます.
一つでも有意でない式がある場合には,その回帰式は利用できないとみなしてください.

### 式全体の評価指標($R^2$,F)

回帰係数のP値,区間推定値は回帰係数一つ一つに関する検定の結果でした. 個別の係数が問題ないとしても,切片と傾きを合わせた式全体の評価はどうなのでしょうか. その判断に利用するのが,
$R^2$ と 有意Fです.

~~~ py
"""
R-squared:                       0.282
Adj. R-squared:                  0.243
F-statistic:                     7.085
Prob (F-statistic):             0.0159
"""
~~~


::: note

- **調整済決定係数(Adj. R-squared)**
---

自由度調整済み決定係数 $Adj R^2$ は,**モデルがどの程度当てはまっているかの基準**です.
基本的に $0 \leq Adj R^2 \leq 1$ の値をとり,目安として $0.5 \leq Adj 𝑅^2$ であればある程度予測できていると考えられます.

どんな散布図になっても回帰式自体は作成可能ですが,以下の左右どちらの予測値の方が信頼できそうでしょうか.

![R2](/images/regression14.png)

直感的には左の方が**回帰直線が実際の値にフィットしており**信頼できそうな気がしますね. $R^2$ はその感覚を数値化したものになります.

![R2](/images/regression15.png)

$R^2$ は,式から作られた直線と,実際のデータの点の距離の和を変更したものです.
$R^2 が1に近いほど, 式が点によく当てはまっていることを表します.

- 0.5以上で予測に利用できる.

- 0.8以上でかなり正確に予測ができる という解釈になります.

この値は説明変数の数が多いほど1に近づく特徴があるので,説明変数が多い場合には説明変数の数を補正した 自由度調整済み決定係数 (𝐴𝑑𝑗.$𝑅^2$)を用います.
:::

今回のモデルを見てみると,`Adj. R-squared:0.243`であり,予測精度が高くないことが分かります.


::: note

- **有意F (Prob (F-statistic))**
---

有意Fは求めた標本回帰式でどの程度データの散らばりを説明できるかを検定した結果です.
どのような意味であるか見ていきましょう.

求めた回帰式と,データそれぞれの散らばりは,**全変動**,**回帰変動**,**残渣変動**によって表現可能です.

![](/images/ch11-prob-f.png)

- 全変動:データと平均値の差の合計(の二乗和)

データの散らばり具合

$$
\sum_{i=1}^n (y_i-y)^2
$$

- 回帰変動:予測値とデータの平均値の差(の二乗和)

データに対する回帰式の予測値の散らばり具合
$$
\sum_{i=1}^n (\hat{y}_i-\bar{y})^2
$$

- 残差変動:予測値とデータの差(の二乗和)

回帰式とデータのズレの散らばり具合

$$
\sum_{i=1}^n (y_i-\hat{y})^2
$$

このとき

`全変動 = 回帰変動+残差変動` すなわち,
$$
\sum_{i=1}^n (y_i-y)^2 = \sum_{i=1}^n (\hat{y}_i-\bar{y})^2 + \sum_{i=1}^n (y_i-\hat{y})^2
$$
がなりたちます.

これは言い換えると,

`データの散らばり=回帰式で説明できる散らばり+回帰式で説明できない散らばり`

を表しています.

回帰変動の平均と,残差変動の平均の比率を考えると,kを説明変数の数として,

$$
\begin{align*}
&\frac{回帰変動の平均}{残差変動の平均}
&=\frac{\sum_{i=1}^n (\hat{y}_i-\bar{y})^2 /𝑘}{\sum_{i=1}^n (y_i-\hat{y})^2/(n−k−1)}
\end{align*}
$$

すなわち

$$
\frac{回帰式で説明できる散らばり}{回帰式で説明できない散らばり}= 回帰式が説明できている割合
$$

が成り立ちます.

![](/images/ch11-prob-f2.png)

また,この説明の割合はF分布に従うことが知られており,

$$
F = \frac{\sum_{i=1}^n (\hat{y}_i-\bar{y})^2 /𝑘}{\sum_{i=1}^n (y_i-\hat{y})^2/(n−k−1)} \sim F(k,n-k-1)
$$

$H_0:F=1$(回帰式が説明できる散らばりが説明できないものと等しい)

$H_1:F>1$ (説明できる量の方が大きい)

として検定した結果のP値が有意Fです.

したがって,有意水準5%のとき,
`有意F < 0.05` であれば,回帰式全体はデータを説明しているとみなすことができます.
:::

今回は`Prob (F-statistic):0.0159`であり,有意に説明できていると言えます.

::: note
したがって今回の結果をまとめると,

- $y=1.03+0.01x$

    - 男性の仕事時間に対する家事育児の割合が1%が増えると,出生率が0.01増える

    - 切片,傾き,式全体は有意(回帰式でデータを説明できる)

- 予測精度は低い(予測には使えない)

という結果が得られます.

内閣府の分析では,回帰式と$R^2$のみが記載されており,検定結果は載っていませんでしたが,概ね正しい推論と言えます.
ただし,この結果を持って,日本の男性の家事育児参加時間をXに増やせば出生率がYになるというような予測はできません.
:::

### 結果の可視化

最後に,このモデルを可視化してみましょう. まずは,内閣府の資料と同じ用に散布図に回帰直線を可視化してみます.
`OLS().fit()`で得られた予測モデルは,`.params[]`で値を得ることができます.回帰式をxの値と回帰係数から計算し,`plt.plot()`で直線を引いています.

~~~ py
import numpy as np

# 回帰直線のためのx軸データとy軸データ
x_vals = np.linspace(df_f['r'].min(), df_f['r'].max(), 100)
print(x_vals)
y_vals = result.params['const'] + result.params['r'] * x_vals

# 描画
plt.figure(figsize=(10, 6))

# 散布図
plt.scatter(df_f['r'], df_f['Fertility'], color='blue', label='Data Points')

# ラベル付け
texts = [plt.text(df_f.at[c, 'r'], df_f.at[c, 'Fertility'], c) for c in df_f.index]
adjust_text(texts, arrowprops=dict(arrowstyle='-', color='gray', lw=0.5))

# 回帰直線
plt.plot(x_vals, y_vals, color='red', linestyle='--', label='Regression Line')

# 軸・タイトル・その他
plt.xlim = (0, 50)
plt.ylim = (0, 2)
plt.xlabel('Unpaid and Care Work')
plt.ylabel('Fertility')
plt.title('Fertility vs Unpaid and Care Work with Regression Line')
plt.legend()
plt.grid()
plt.show()
~~~

![](/images/ch11_oecd_scatter_with_line.png)

このようなグラフによって実測値に対する回帰直線の当てはまりを確認することもできますが,もう少しわかりやすい手法として,縦軸に実測値 $Y$ ,横軸に予測値 $\hat{\beta}_0 + \sum \hat{\beta}_i X_i$ をプロットした散布図もよく用いられます.

先ほどとは異なり, `.predict()`メソッドで元データから計算された予測値を求めています.ここにxの等差級数を渡すことで先程のように回帰直線の値を得ることも可能です.

~~~ py
pred = result.predict(X)
plt.figure(figsize=(8, 6))
plt.scatter(y, pred, alpha=0.7, edgecolors="k")
plt.xlabel("Predicted")
plt.ylabel("Actual")
plt.title("Actual vs. Predicted")
plt.plot([y.min(), y.max()], [y.min(), y.max()], color="red", linestyle="--")  # 完全一致のライン
plt.grid()
plt.show()
~~~

![](/images/ch11_oecd_pred_actual.png)

実測値と予測値が一致していれば,この散布図はグラフの45度線(赤い点線)上に一直線になります.
$R^2$の結果通り,あまり予測精度が高くないことがわかります.

実測値と予測値の`カーネル密度プロット(KDE; Karnel Densiti Estimation Plot)`を重ねたグラフもベイズモデルなどでよく利用される手法です.
実測値の分布と予測値の分布を見比べることで,値全体でどの部分が過大/過少に推測されているかモデルの想定する分布がデータの分布と近いかが視覚化されます.

`kde`の描画には`seaborn`ライブラリが必要になるので`pip install`しておきましょう.

~~~ py
import seaborn as sns

plt.figure(figsize=(16,8))
sns.kdeplot(pred, label = 'Predicted')
sns.kdeplot(y, label = 'Actual')
plt.title('Actual/Predicted')
plt.xlabel('Fertility')
plt.ylabel('Density')
plt.legend()
plt.show()
plt.close()
~~~

![](/images/ch11_oecd_density.png)

実測値と予測値で大きく分布が異なることがわかり,やはりあまり良いモデルとはいえなさそうです.

::: note
- 演習問題

内閣府の計算で何故か除外されていたMexicoのデータを加えて回帰分析を行い,その結果を解釈してください.

:::

## 重回帰分析


## 重回帰分析

説明変数も被説明変数も量的変数のときに,

$$
y_i = \beta_0 + \beta_1 x_{1i} + \beta_2 x_{2i} + ... + \beta_n x_{ni}
$$

のような回帰式を求めます.

それでは,Pythonで重回帰分析を行ってみましょう.

題材として以下の｢日本教育新聞｣の記事([https://www.kyoiku-press.com/post-223665/
](https://www.kyoiku-press.com/post-223665/))について考えてみます.

::: note

> **Wi-Fi電磁波で学力低下を懸念,市議ら意見交換会**
>
> 2020年12月7日
>電磁波が人体に影響を与え,学力の低下を招くことなどを懸念する市議会議員らは11月8日,無線LANにより生じる「電磁波過敏症」への対策などについて,意見交換会をオンラインで開催した.
>　GIGAスクール構想でICT環境を整備するに当たって,電磁波による問題点とそれへの対策を話し合った.
>　東京都新宿区議会のよだかれん議員は,学力と健康の2つの観点から,「大人でもICT機器を使用すると前頭前野の機能が低下するという様々な研究報告がある.小学1年生からの使用で脳の発達への影響は懸念されないのか」と指摘した.
>　よだ議員は,9月議会の質疑の一部で,令和元年の全国学力テストの結果に基づき,電子黒板やプロジェクターなどの大型電子機器の整備率が1位の佐賀県は正答率が全国で43位だった一方,整備率最下位の秋田県は正答率が1位だったことを紹介した.
>　意見交換会を主催した「いのち環境ネットワーク」の加藤やすこ代表によると,電磁波過敏症は短い時間でも発症の可能性があり,一度の発症が長期に及んで続くという.
>　埼玉県日高市議会の松尾まよか議員は,GIGAスクール構想を進める上で,Wi-Fiのアクセスポイントの位置を児童・生徒から遠ざけた場所に設置する,使用していない時は電源を落とすことを重要な点に位置付けた.
>　松尾議員は,「発症者が出てからでは遅い.発症後の対策に予算をかけるよりも,事前に対策しておく方がよい」と強調した.
>　今回の意見交換会に参加した市議らは,9月議会の発言内容なども報告した.

こちらの記事では,

> 令和元年の全国学力テストの結果に基づき,電子黒板やプロジェクターなどの大型電子機器の整備率が1位の佐賀県は正答率が全国で43位だった一方,整備率最下位の秋田県は正答率が1位だった

ことから,

｢学校教育におけるICT機器の導入が学力低下を招いている｣ということを主張しています.

:::

最下位と1位の2つの観測対象のデータだけでこのような主張が可能なのでしょうか. データを使ってこの主張を検証してみましょう. 必要なデータは[こちら](https://github.com/yakagika/yakagika.github.io/blob/main/slds_data/math_correct.csv)からダウンロード可能ですが,データの取得手順に興味がある方は,以下から確認して自分でデータを作成してみましょう.

::: note
- 教育データの作成

<details open>
    <summary> 開く/ 閉じる </summary>
まずは全国の県別の学力データを探してみます.

 国立教育政策研究所の行っている全国学力・学習状況調査([https://www.nier.go.jp/18chousakekkahoukoku/index.html](https://www.nier.go.jp/19chousakekkahoukoku/index.html))から,令和元年の県別の小学生の学力データを取得します.

![全国学力･学習状況調査](/images/regression1.png)

ただし,こちらのデータはPDFでのみ公開されているためExcelやAIなどを利用して,CSVに変換する必要があります. 今回私はPDFからテキストエディタにコピー&ペーストして, 不要な記号を置換しましたが,好きな方法でやりましょう.

![全国学力･学習状況調査](/images/regression2.png)

続いて, 記事にある,電子黒板やプロジェクターなどの大型電子機器の整備率に関するデータを[e-stat](https://www.e-stat.go.jp )から取得します.

今回は県別データなので,｢地域｣から,データを取得します.

![e-stat 地域](/images/regression3.png)

｢都道府県データ｣にチェックを入れて｢データ表示｣に進みます.

![都道府県データ](/images/regression4.png)

｢地域選択｣において｢全て選択｣をクリックしたあと選択中地域から全国をクリックし,｢地域を削除｣
を押し,47都道府県のみを選び,確定します.

![地域選択](/images/regression5.png)

｢分野｣から｢教育｣を選び関連のありそうな
    - ｢教育用コンピュータ一台あたりの児童数(小学校)｣
    - ｢普通教室の電子黒板整備率(小学校)｣
    - ｢デジタル教科書の整備率(小学校)｣
などを選択し,｢項目を選択｣を押し,確定します.

![教育関連データの選択](/images/regression6.png)

｢調査年｣からデータが揃っている2017年度を選択し,｢再表示｣を押します.

![調査年の選択](/images/regression7.png)

｢ダウンロード｣から,右の図のように選択して,ダウンロードします.

![ダウンロード](/images/regression8.png)

学力のデータにダウンロードした黒板などのデータをコピーして,適切にヘッダーをつけ,Utf-8で作業フォルダの中のDataフォルダに保存しましょう.例ではmath_correct.csvと名前をつけています.
    - 数学正答率       → math
    - 一台あたりのPC   → pc
    - 黒板             → board
    - 電子教科書       → text

Excelで貼り付けた際に文字列情報になっている場合があり,!マークが表示されていたら数値に変換しておきましょう.

![データの編集](/images/regression9.png)

データが読み込めるか確認してみましょう.

~~~ py

df = pd.read_csv('Data/math_correct.csv')
print(df)

"""
   pref  math   pc  board  text
0   北海道    64  5.8   20.8  38.5
1   青森県    67  5.3   22.4  30.9
2   岩手県    66  5.4   19.6  35.9
3   宮城県    65  6.8   14.0  66.6
4   秋田県    70  5.6   22.4  36.8
5   山形県    65  5.7   16.6  44.0
6   福島県    65  5.3   24.6  47.2
7   茨城県    66  6.5   22.0  49.5
8   栃木県    65  5.9   42.6  70.8
9   群馬県    65  6.1   15.4  39.3
10  埼玉県    66  9.6   23.9  58.8
...
"""
~~~

</details>
:::

それでは,このデータを利用して重回帰分析を行っていきます.

::: note

Pythonで重回帰を行えるライブラリは多数ありますが, 今回は統計モデリングのためのライブラリ`statsmodels`を利用します. pip install し, プログラムの最初に, `import statsmodels.api as sm`と記述し`import`しておきましょう.
:::

ただし, 重回帰を実施する前に,いくつか必要な前処理があります. 順番に見ていきましょう.

### 正規化・標準化

重回帰分析では,複数の説明変数の目的変数に対する影響を比較します. 各説明変数の目的変数への影響力は,回帰係数として現れますが説明変数1の単位がmm,説明変数2の単位がm, 説明変数3の単位がKgなどとなると,それぞれの回帰係数は,それぞれ1mmの変化,1mの変化,1Kgの変化に対する目的変数の変化量を表しているために,同じ基準で比較できません.そこで,**正規化/標準化**というデータの単位などを揃える操作を行う必要があります.


::: note

- 正規化(Normalization)

最大値を1,最小値を0に揃えること.

データを $X = {x_1,x_2,...,x_n}$ とすると, 正規化後の値 $x_i'$ は

$$
x_i' = \frac{x_i - min(X)}{max(X) - min(X)}
$$

`pandas`では`X`が対象のデータ列名のリストだとすると,以下のように正規化列`X_n`が求められる.

~~~ py
X_n = (df[X] - df[X].min()) \
    / (df[X].max() - df[X].min())
~~~


:::

::: note
- 標準化(Standarization)

標準得点を求めて,平均0,分散1に揃える.

$$
\sum
$$

$$
z_i = \frac{x_i - \bar{x}}{\sigma}
$$




`pandas`では`X`が対象のデータ列名のリストだとすると,以下のように標準化列`X_z`が求められる.

~~~ py
X_z = (df[X] - df[X].mean()) \
    / df[X].std()
~~~
:::


それでは, 先ほど得られた教育データを標準化してみましょう.

~~~ py
Y_label = 'math'
#標準化
Y = (df[Y_label] - df[Y_label].mean()) \
    / df[Y_label].std()

plt.hist(Y)
plt.title(Y_label)
plt.show()

# 説明変数のヘッダーを指定
X_labels = ['pc','board','text']
#標準化
X  = (df[X_labels] - df[X_labels].mean()) \
    / df[X_labels].std()

fig, axes = plt.subplots(nrows= 3 #行数の指定
                        ,ncols= 1 #列数の指定
                        ,sharex=True)

#連番に変換
axes = axes.flatten()
for i in range(3):
    col = X_labels[i] #countをiで共通化
    axes[i].hist(X[col])
    axes[i].set_title(col)
plt.show()
~~~



![Yのヒストグラム](/images/regression10.png)
![Xのヒストグラム](/images/regression11.png)



::: warn
本来ならば,ここで`math`が左右対称ではないことから正規分布を仮定した回帰を行うべきではなく,ベータ分布などを仮定した一般化線形モデルにすることを検討します.

また, `text`に外れ値(長崎県)があることなども考慮するべきですが,今回は線形回帰の事例ですのでそのまま進めてみます.
:::



### 多重共線性(multi-colinearlity)

::: note

説明変数 $x_1,x_2,\dots,x_n$ において, 特定の変数 $x_i$ が他の変数によって

$$
x_{i} = \sum_{i \neq j} \alpha_j x_j
$$

として,少なくとも1つが0でない $\alpha_j$ によって表すことができる場合に,すなわち各変数が一次独立でなくなる場合に,説明変数に**完全な多重共線性**が成り立っているといいます.

このとき, 最小二乗法では, $y = \beta_0 + \beta_1 x_1 + \beta_2 x_2 + ... + \beta_n x_n$ を解くことができなくなるため,解が得られなくなります.

:::

例えば, $x_1 = \alpha_2 x_2$ であったとすると,

$$
y = \beta_0 + (\beta_1 + \alpha_2 \beta_2) x_1 + \beta_3 x_3 + ... + \beta_n x_n \\
$$

となり,推定値が $\hat{\beta_{12}} = \hat{\beta_1} + \alpha_2 \hat{\beta_2}$ であったとすると, $\hat{\beta_{12}}$ となる $\hat{\beta_1}$ と $\hat{\beta_2}$ の組み合わせは無数にあるため, $\hat{\beta_1}$ と $\hat{\beta_2}$ を特定することができなくなります.

このような**完全な多重共線性**は,主に特定の説明変数を,他の説明変数の変形によって作成している場合に生じるため,変形した変数を利用するならば,変形前の変数はモデルに利用しないようにしましょう.

例えば, これから使い方を学習する`statsmodels`を利用して,あえて**完全な多重共線性**が存在するような重回帰分析を行ってみます.
(`statsmodels`や重回帰の実施については,後述するのでこの時点では変数の生成以外は意味を理解する必要はありません.)

~~~ py
import pandas as pd
import statsmodels.api as sm
import numpy as np

#乱数で目的変数と説明変数を生成
y = np.random.rand(100)
x_1 = np.random.rand(100)
df = pd.DataFrame({'y':y,'x_1':x_1})

#説明変数x_2をx_1から生成
df['x_2'] = df['x_1'] * 2

X = ['x_1','x_2']

#予測モデルを作成(重回帰)
X = sm.add_constant(df[X])
model = sm.OLS(df['y'],X)
result = model.fit()
print(result.summary())

"""
                            OLS Regression Results
==============================================================================
Dep. Variable:                      y   R-squared:                       0.000
Model:                            OLS   Adj. R-squared:                 -0.010
Method:                 Least Squares   F-statistic:                   0.01870
Date:                Mon, 08 Jul 2024   Prob (F-statistic):              0.892
Time:                        18:24:01   Log-Likelihood:                -11.013
No. Observations:                 100   AIC:                             26.03
Df Residuals:                      98   BIC:                             31.24
Df Model:                           1
Covariance Type:            nonrobust
==============================================================================
                 coef    std err          t      P>|t|      [0.025      0.975]
------------------------------------------------------------------------------
const          0.4896      0.060      8.121      0.000       0.370       0.609
x_1            0.0028      0.021      0.137      0.892      -0.038       0.044
x_2            0.0056      0.041      0.137      0.892      -0.076       0.087
==============================================================================
Omnibus:                       19.023   Durbin-Watson:                   2.250
Prob(Omnibus):                  0.000   Jarque-Bera (JB):                5.214
Skew:                           0.170   Prob(JB):                       0.0738
Kurtosis:                       1.934   Cond. No.                     8.65e+16
==============================================================================

Notes:
[1] Standard Errors assume that the covariance matrix of the errors is correctly specified.
[2] The smallest eigenvalue is 3.44e-32. This might indicate that there are
strong multicollinearity problems or that the design matrix is singular.
"""
~~~

`statsmodels`では,一応重回帰自体は実施できるものの,

> The smallest eigenvalue is 3.44e-32. This might indicate that there are
strong multicollinearity problems or that the design matrix is singular.

のように,多重共線性の存在を教えてくれます. (乱数を利用していることもあり)当然モデルの精度も非常に悪くなっているため, このモデルは利用できません.


一方で,

::: note
$$
x_i \approx \sum_{i \neq j} \alpha_j x_j
$$

で成り立つ(完全ではない/弱い)**多重共線性**という概念もあります. これは簡単に言えば, 説明変数間に相関関係が成り立つような場合を指しています.

変数間に相関がある場合,

- サンプルサイズによって,推定値が大きく変わる

- データによって推定値が大きく変わる

など, 利用するデータに対して推定値が不安定になると言われています. これはサンプルサイズを増やすことで対処できますが,一般には変数間の相関が強い場合には, 片方の変数は説明変数から除外することが望ましいとされています.
:::

それでは,先程の教育データの多重共線性をチェックしてみましょう.

~~~ py
# 多重共線性のチェック
# 散布図行列を作成してみる
pd.plotting.scatter_matrix(df, range_padding=0.2)
plt.show()

#ヒートマップで確認
sns.heatmap(df.corr()
            ,vmax=1
            ,vmin=-1
            ,annot=True)
plt.show()
~~~

![クロスプロット](/images/regression12.png)
![相関係数のヒートマップ](/images/regression13.png)

相関係数を確認すると,`board`と`text`の間に`0.56`の相関があることが分かります.
`0.56`程度であればそれほど影響はないのでそのままにしても構いませんが,練習として片方を除外してみます. `text`のほうが`math`との相関が強いので,`board`を除外したいところですが,市議の主張では電子黒板の普及率が問題と成っていたので`text`を除外します.

### 回帰分析の精度と判断

それでは, データの標準化,多重共線性を考慮して,実際に回帰分析を行ってみましょう.
`statsmodels`では, `sm.add_constant(説明変数)`の形で,定数を追加し,切片をモデルに追加することができます.

`sm.OLS(Y,X)`で線形回帰モデルインスタンスを宣言し, `.fit()`で推定を行います.
推定結果は`.summary()`で確認できます.


~~~ py
#相関が見られるため,textを除外
X.drop('text',axis='columns',inplace=True)

#予測モデルを作成(重回帰)
model = sm.OLS(Y,X)
result = model.fit()

#結果の表示
print(result.summary())

"""
==============================================================================
Dep. Variable:                   math   R-squared:                       0.004
Model:                            OLS   Adj. R-squared:                 -0.042
Method:                 Least Squares   F-statistic:                   0.07867
Date:                Mon, 08 Jul 2024   Prob (F-statistic):              0.924
Time:                        23:55:07   Log-Likelihood:                -66.101
No. Observations:                  47   AIC:                             138.2
Df Residuals:                      44   BIC:                             143.8
Df Model:                           2
Covariance Type:            nonrobust
==============================================================================
                 coef    std err          t      P>|t|      [0.025      0.975]
------------------------------------------------------------------------------
const       3.995e-15      0.149   2.68e-14      1.000      -0.300       0.300
pc             0.0493      0.162      0.305      0.762      -0.276       0.375
board          0.0560      0.162      0.347      0.730      -0.270       0.382
==============================================================================
Omnibus:                       13.834   Durbin-Watson:                   1.557
Prob(Omnibus):                  0.001   Jarque-Bera (JB):               14.866
Skew:                           1.170   Prob(JB):                     0.000591
Kurtosis:                       4.454   Cond. No.                         1.46
==============================================================================
"""
~~~

さて,推定結果には様々な情報が記述されていますが,どこをどのように見ればいいのでしょうか.

`statsmodels`の`.summary()`では中央部分に各説明変数の評価が記載されています.

- 各説明変数の評価

~~~ sh
==============================================================================
                 coef    std err          t      P>|t|      [0.025      0.975]
------------------------------------------------------------------------------
const       3.995e-15      0.149   2.68e-14      1.000      -0.300       0.300
pc             0.0493      0.162      0.305      0.762      -0.276       0.375
board          0.0560      0.162      0.347      0.730      -0.270       0.382
==============================================================================
~~~

それぞれいくつか重要なポイントを順に確認していきましょう.

::: note
- **回帰係数(coef)**
---

回帰係数は,説明変数毎に目的変数にどの程度影響力があるかを示したものになります.
:::

今回の事例では, 切片(`const`)が`3.995e-15`であり,`pc`,`text`が0のとき`math`が`3.995e-15`となります. `pc`の回帰係数が`0.0493`,`text`の回帰係数が`0.0560`であり,それぞれの説明変数が1変化する毎に,回帰係数の分だけ目的変数が変化します.

したがって,回帰式は,

$$
math = 3.995*10^{-15} + 0.0493 pc + 0.0560 text
$$

となり,いずれの変数も正の影響を持っており,市議の主張である電子黒板が普及するほどに成績が下がるという主張と逆の結果が出ています.値が標準化されているため,それぞれの回帰係数は相対的な影響力を表しており,実際の変化ではありません. `pc`の一人あたりの台数よりも,`text`の影響が強いことが分かります.

しかし,回帰係数だけを見て,回帰の結果を判断することはできません. 出てきた値が,信頼に値するか,利用可能であるかを他の値を用いて判断する必要があります.

::: note

- **P値(`P>|t|`)と95%信頼区間(`[0.025 0.975]`)**
---

それぞれの説明変数のP値(`P>|t|`)は,その説明変数に対する回帰係数が0である(影響がない)という仮説に対する仮説検定のP値を表しています. また, `[0.025      0.975]`はそれぞれ上側と下側の95%信頼区間を表しています.

有意水準5%の場合, `P値 < 0.05`で有意となり, 信頼区間が0をまたぎません.
(ここで両側検定ですが,`P値 < 0.025`とならないのは,両側の外側累積確率を合算した値が算出されるためです.)
したがって,これらの値はいずれも同じ判断の基準となりますが,最近は論文などには両方載せることが主流です.
(仮説検定や区間推定,P値の意味などに関しては,統計学入門で詳細を扱っています.分からない人はそちらを履修しましょう.)

:::

今回の値を見てみると,いずれの説明変数も有意ではなく,区間推定の結果も0をまたいでいます. したがって,これらの回帰係数の推定値の解釈は, **｢今回のデータと説明変数の組み合わせでは, 電子黒板や電子教科書が学業成績に影響を与えるかどうかは判断できない.｣**ということになります.

::: warn
有意でない説明変数は, 基本的にモデルから除外することが望ましいので,仮により良いモデルを構築する場合には,別の変数やモデルの組み合わせを探すことになります.

それらの手法は,モデル選択などと呼ばれますが,モデル同士の比較の方法に関してはここでは扱わず,後の一般化線形モデルの章で扱うことにします.

:::

これまで各説明変数の影響を見てみましたが, 変数毎ではなく,モデル全体の評価はどのように行うのでしょうか.
`.summary()`の前半部分にはモデル全体での評価が記載されています.

- モデル全体の評価

~~~ sh
                            OLS Regression Results
==============================================================================
Dep. Variable:                   math   R-squared:                       0.004
Model:                            OLS   Adj. R-squared:                 -0.042
Method:                 Least Squares   F-statistic:                   0.07867
Date:                Mon, 08 Jul 2024   Prob (F-statistic):              0.924
Time:                        23:55:07   Log-Likelihood:                -66.101
No. Observations:                  47   AIC:                             138.2
Df Residuals:                      44   BIC:                             143.8
Df Model:                           2
Covariance Type:            nonrobust
==============================================================================
~~~


::: note

- **調整済決定係数(Adj. R-squared)**
---

自由度調整済み決定係数$Adj R^2$は,**モデルがどの程度当てはまっているかの基準**です.
基本的に$0 \leq Adj R^2 \leq 1$の値をとり,目安として$0.5 \leq Adj 𝑅^2$であればある程度予測できていると考えられます.

どんな散布図になっても回帰式自体は作成可能ですが,以下の左右どちらの予測値の方が信頼できそうでしょうか.

![R2](/images/regression14.png)

直感的には左の方が**回帰直線が実際の値にフィットしており**信頼できそうな気がしますね. $R^2$はその感覚を数値化したものになります.

![R2](/images/regression15.png)

$R^2$は,式から作られた直線と,実際のデータの点の距離の和であり,$Adj R^2$は,0から1に収まるように変換してものになります.
$AdjR^2$が1に近いほど, 式が点によく当てはまっていることを表します.
:::

今回のモデルを見てみると,`Adj. R-squared:-0.042`であり,全く予測精度が高くないことが分かります.

::: note

- **有意F (Prob (F-statistic))**
---

検定にはそれぞれの係数ごとにt検定,全体にF検定を用います.
有意F(Prob (F-statistic))は,モデル全体にF検定を実施した際のp値を表し,値が小さいほど回帰式が有意であることを表します(0.05 > F で有意).
(検定に関しては,検定の章を参照してください.)

以下の回帰式とデータを見るとどちらも,当てはまり方は同じだとしても, 左の方が信頼性が高いように感じます.

![Prob F](/images/regression16.png)

点が少ないと,他のデータを持ってきたら全然違うところに点が行く可能性が高まります.
この感覚=式が偶然の産物ではないかを検定したP値が有意Fです.
0.05 以下で, 信頼できるといえます.

:::

今回のモデルを見てみると`Prob (F-statistic):0.924`であり,回帰式全体は有意ではなく,このモデルは信頼できないことが分かります.


### 結果の図示

これまでは数値によって,モデルの結果を確認してきましたが,結果を図示することでより直感的に説明が可能になります. 回帰分析の結果を図示する方法は様々ありますが,ここでは結果の散布図と,密度プロットによって確認してみましょう.

各説明変数が,どの程度目的変数を説明しているかを確認する方法として,グラフに`Partial Regression Plot`や`Added-Variable Plot(AV Plot)`と呼ばれるグラフがあります.

- 縦軸に$X_i$以外の変数でYを回帰した際の残差(他の変数の影響を取り除いたYの変動)

- 横軸に$X_i$以外の変数で$X_i$を回帰した際の残差(他の変数の影響を取り除いた𝑋_𝑖の変動)

がプロットされています.

このグラフでは他の変数の影響を取り除いた上でのYと$X_i$の関係を可視化する手法であり,回帰直線の傾きが大きいほど他の変数の影響を取り除いた上での$X_i$のYへの影響力が強いことが分かります.


`statsmodels`では,そのための`plot_partregress_grid`というメソッドが準備されています.

~~~ py
#回帰グラフの作成
from statsmodels.graphics.regressionplots import plot_partregress_grid
fig = plt.figure(figsize=(16,8))
plot_partregress_grid(result, fig=fig)
plt.show()
~~~

![3変数 Partial Plot](/images/regression17.png)

どの変数にもあまり説明力がない様子が視覚的に把握できます.

また,モデルによって予測される値と実際の値のヒストグラムや密度プロットを比較することも良く行われます.
ここでは`seaborn`の`kdeplot`を利用してカーネル密度プロットを行ってみます.

~~~py
#予測結果の作成
pred = result.predict(X)
plt.figure(figsize=(16,8))
sns.kdeplot(pred, label = 'Predicted')
sns.kdeplot(Y, label = 'Actual')
plt.title('Actual/Predicted')
plt.xlabel('math')
plt.ylabel('Density')
plt.legend()
plt.show()
~~~

![3変数 Density Plot](/images/regression18.png)

実測値と予測値が全く違う分布をしており,予測がうまく行っていないことが視覚的に把握できます.

結論として, 今回のモデルでは市議の主張に対しては,否定も肯定もできないが,市議の根拠とするデータでは,市議の主張が導かれないということになりました.

実際に,どのような影響があるかを調べるためには,データかモデルのいずれかを変えて,説明可能なモデルを構築する必要があります.


::: note

- 演習問題

数学の成績と関連のありそうなデータをe-statから複数探し,重回帰分析を行い,その結果を解釈してください.

:::




