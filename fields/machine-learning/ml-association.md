---
title: "アソシエーション分析"
kind: code
open: true
requires:
  - ml-clustering
---
## パターン発見: アソシエーション分析 {#パターン発見-アソシエーション分析}

**アソシエーション分析**は, 大量の取引の記録から「おにぎりを買う人はお茶も買う」のような, 同時に起こりやすい項目の組み合わせを取り出す手法です. 買い物かごの中身を分析する用途から, **バスケット分析**とも呼ばれます. 取り出した組み合わせは, 「$X$ を含む取引は $Y$ も含む」という**ルール** $X \Rightarrow Y$ の形で表します. $X$ を条件部, $Y$ を結論部といいます.

売店の購買記録をレシート単位にまとめ, 1 回の買い物で買われた品目のリストにします.

~~~ py
baskets = log.groupby('receipt_id')['item'].apply(list).to_list()
print(len(baskets))
print(baskets[:3])
"""
5877
[['お茶', '弁当'], ['ジュース', '弁当'], ['おにぎり', 'お茶', 'パン']]
"""
~~~

# []{#支持度確信度リフト値}支持度・確信度・リフト値 {#support-confidence-lift}

ルール $X \Rightarrow Y$ の良さは, 次の 3 つの指標で測ります. 取引 (レシート) の総数を $N$ とします.

::: note
- **支持度 (support)**: $X$ と $Y$ を両方含む取引の割合. ルールがどれだけ頻繁に現れるかを表す.

$$ \text{support}(X \Rightarrow Y) = \frac{X \text{と} Y \text{を両方含む取引の数}}{N} $$

- **確信度 (confidence)**: $X$ を含む取引のうち, $Y$ も含む取引の割合. $X$ を条件とする $Y$ の条件付き確率 $P(Y \mid X)$ にあたる.

$$ \text{confidence}(X \Rightarrow Y) = \frac{X \text{と} Y \text{を両方含む取引の数}}{X \text{を含む取引の数}} $$

- **リフト値 (lift)**: 確信度を, $Y$ を含む取引の割合 $P(Y)$ で割った値.

$$ \text{lift}(X \Rightarrow Y) = \frac{\text{confidence}(X \Rightarrow Y)}{P(Y)} = \frac{P(Y \mid X)}{P(Y)} $$

:::

ルール「おにぎり $\Rightarrow$ お茶」の 3 つの指標を計算してみましょう. レシート 5877 枚のうち, おにぎりを含むものは 989 枚, お茶を含むものは 2071 枚, 両方を含むものは 791 枚です.

- 支持度: $791 / 5877 \approx 0.135$
- 確信度: $791 / 989 \approx 0.80$
- リフト値: $0.80 / (2071 / 5877) \approx 0.80 / 0.352 \approx 2.27$

リフト値は, $X$ を含むことで $Y$ の起こりやすさが何倍になるかを表します. 全体ではお茶を含むレシートは 35% ですが, おにぎりを含むレシートに限ると 80% になるので, リフト値は約 2.3 です. リフト値が 1 なら $X$ の有無は $Y$ の起こりやすさに関係せず, 1 未満なら $X$ を含む取引ではかえって $Y$ が起こりにくくなっています.

確信度だけでなくリフト値も見る理由を, ルール「パン $\Rightarrow$ コーヒー」で確かめます. パンを含むレシートは 760 枚, コーヒーを含むレシートは 2706 枚, 両方を含むレシートは 322 枚です.

- 確信度: $322 / 760 \approx 0.42$
- リフト値: $0.42 / (2706 / 5877) \approx 0.42 / 0.460 \approx 0.92$

確信度 0.42 は, パンを買ったレシートの 4 割がコーヒーも含むという意味です. しかしコーヒーはもともと全レシートの 46% に含まれるので, パンを買ったレシートでは, コーヒーを含む割合がむしろ少し下がっています. 確信度は結論部の品目がもともとどれだけ買われているかを考慮しないので, 結論部がよく買われる品目なら, 関係が無くても確信度は高くなります.

アソシエーション分析で得られるのは, 同時に起こりやすいという関係であって, 一方が他方の原因であることは示しません ([第4章](block:num-correlation#因果関係と相関)の「因果関係と相関」と同じ注意です).

## mlxtend によるアソシエーション分析 {#mlxtend-によるアソシエーション分析}

品目の種類が増えると, 調べる組み合わせの数は急激に増えます. **Apriori アルゴリズム**は, 支持度が小さい組み合わせを先に除くことで, 調べる組み合わせを減らします. 組み合わせに品目を 1 つ付け加えると, 付け加えた後の組み合わせを含む取引は, 元の組み合わせを含む取引より多くはなりません. したがって, 支持度が基準に届かない組み合わせは, 品目をいくつ付け加えても基準に届かないので, 調べる前に除けます.

`mlxtend` の `apriori` で支持度が 0.02 以上の品目の組み合わせを求め, `association_rules` でリフト値が 1 以上のルールを取り出します.

~~~ py
from mlxtend.preprocessing import TransactionEncoder
from mlxtend.frequent_patterns import apriori, association_rules

# レシート×品目の表 (買った品目が True) に変換する
te = TransactionEncoder()
onehot = pd.DataFrame(te.fit_transform(baskets), columns=te.columns_)

# 支持度が0.02以上の品目の組み合わせを求める
freq = apriori(onehot, min_support=0.02, use_colnames=True)

# リフト値が1以上のルールを取り出し, リフト値の大きい順に10件表示する
rules = association_rules(freq, metric='lift', min_threshold=1.0)
print(rules[['antecedents', 'consequents', 'support', 'confidence', 'lift']]
      .sort_values('lift', ascending=False).head(10).round(2))
"""
              antecedents            consequents  support  confidence  lift
32    frozenset({パン, お茶})      frozenset({おにぎり})     0.06        0.82  4.90
37      frozenset({おにぎり})    frozenset({パン, お茶})     0.06        0.34  4.90
34  frozenset({お茶, おにぎり})        frozenset({パン})     0.06        0.42  3.25
35        frozenset({パン})  frozenset({お茶, おにぎり})     0.06        0.44  3.25
4         frozenset({パン})      frozenset({おにぎり})     0.07        0.53  3.17
5       frozenset({おにぎり})        frozenset({パン})     0.07        0.41  3.17
14      frozenset({ジュース})      frozenset({カップ麺})     0.02        0.19  2.71
15      frozenset({カップ麺})      frozenset({ジュース})     0.02        0.29  2.71
33  frozenset({パン, おにぎり})        frozenset({お茶})     0.06        0.82  2.32
36        frozenset({お茶})  frozenset({パン, おにぎり})     0.06        0.16  2.32
"""
~~~

`antecedents` が条件部, `consequents` が結論部です. リフト値が同じ行の並び順と, 各部に含まれる品目の並び順は, 実行するたびに変わることがあります. 上位には「パンとお茶を買うレシートはおにぎりも含む」(確信度 0.82) のような, 昼食の組み合わせが並びます.

支持度の基準は結果を大きく左右します. 例えば「ノート $\Rightarrow$ ボールペン」は, ノートを含むレシート 187 枚のうち 78 枚がボールペンも含み, リフト値は約 16 で, 強い関係があります. しかし文具を買うレシート自体が少ないので, 支持度は $78 / 5877 \approx 0.013$ で, 基準の 0.02 に届かず上の表には出てきません. 基準を低くするとこのような, まれだが強い関係を拾えます. 一方で, 偶然いくつか重なっただけの組み合わせもルールとして大量に出てきます. 基準は, 得られるルールの数と中身を見ながら調整します.

この節に対応する演習は, 章末の[Exercise DSP7-3](#exercise-dsp7-3)です.

# []{#演習}演習 {#exercises}

## Exercise DSP7-3 {#exercise-dsp7-3}

**クラスタごとのアソシエーション分析**

対応する節: [非階層クラスタリング: k-means 法](block:ml-clustering#非階層クラスタリング-k-means-法), [支持度・確信度・リフト値](#支持度確信度リフト値)

本文の k-means 法 (2 項目, $K = 4$) で求めた会員のクラスタを使い, ルール「コーヒー $\Rightarrow$ クッキー」の確信度とリフト値を次の 2 通りで計算してください.

1. 全レシートを対象にした場合
2. カフェ利用の会員のクラスタに属する会員のレシートだけを対象にした場合

2 つのリフト値の違いが何を意味するかを説明してください.

提出ファイル名: `dsp7-3.py`

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ py
import pandas as pd
from sklearn.cluster import KMeans

log = pd.read_csv('data/store_log.csv', encoding='utf-8-sig')
g = log.groupby('member_id')
member = pd.DataFrame({
    'visits': g['receipt_id'].nunique(),
    'amount_per_visit': g['price'].sum() / g['receipt_id'].nunique(),
})
z = (member - member.mean()) / member.std(ddof=0)
member['cluster'] = KMeans(n_clusters=4, random_state=0, n_init=10).fit_predict(z)

# merge で2つの表を結合し, 記録の各行に会員のクラスタを付ける
# (記録の member_id と, member の行見出し (会員番号) が一致する行同士をつなぐ)
log = log.merge(member[['cluster']], left_on='member_id', right_index=True)

def confidence_lift(df, x, y):
    # レシートごとの品目の集合
    baskets = df.groupby('receipt_id')['item'].apply(set)
    has_x = baskets.apply(lambda s: x in s)
    has_y = baskets.apply(lambda s: y in s)
    confidence = (has_x & has_y).sum() / has_x.sum()
    lift = confidence / has_y.mean()
    return round(float(confidence), 2), round(float(lift), 2), round(float(has_y.mean()), 2)

# 問1
print(confidence_lift(log, 'コーヒー', 'クッキー'))
"""
(0.56, 2.1, 0.27)
"""
# 問2: 来店回数が最も多いクラスタがカフェ利用の会員
cafe = member.groupby('cluster')['visits'].mean().idxmax()
print(confidence_lift(log[log['cluster'] == cafe], 'コーヒー', 'クッキー'))
"""
(0.57, 1.2, 0.48)
"""
~~~

**問 1**: 確信度 0.56, リフト値 2.10 です.

**問 2**: 確信度 0.57, リフト値 1.20 です.

確信度はほとんど変わりませんが, リフト値は 2.10 から 1.20 に下がります. 全レシートではクッキーを含む割合が 27% なのに対し, カフェ利用の会員のレシートに限ると 48% あるためです. 全体でのリフト値が大きいのは, 「コーヒーを買うとクッキーも買いたくなる」ことよりも, 「コーヒーもクッキーもカフェ利用の会員がよく買う」ことによる部分が大きいと分かります. カフェ利用の会員の中でも, コーヒーを買ったレシートではクッキーが少し買われやすい (リフト値が 1 を上回る) ので, 2 つの品目の関係はゼロではありません. クラスタリングで会員を分けてからアソシエーション分析をすると, 誰が買っているかの違いと, 1 回の買い物の中での組み合わせの関係を分けて見られます.

</details>

