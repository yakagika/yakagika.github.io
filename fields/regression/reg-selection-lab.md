---
title: 変数選択の結果を実験で確かめる
kind: code
open: false
requires:
  - reg-selection-gpa
bibliography: lectures
---

GPA の予測式を 15 通りの組み合わせから選ぶとき, 選ぶために測った誤差と, 選んだ後に測った誤差を並べます.
独立な乱数を使う別の実験では, 無関係な変数から P 値で選んだときに何が残るかを調べます.

## 合成 GPA データの準備 {#selection-lab-data}

[`multiple_regression.csv`](https://github.com/yakagika/yakagika.github.io/blob/main/slds_data/ch11/multiple_regression.csv) には, 合成した 100 人分のデータが入っています.
CSV をダウンロードしてコードと同じフォルダに置き, そのフォルダからコードを実行してください.
`index_col=0` で先頭列の行番号を行のインデックスとして読み込みます.

~~~ python
from itertools import combinations

import pandas as pd
import statsmodels.formula.api as smf
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import KFold, train_test_split

df = pd.read_csv("multiple_regression.csv", index_col=0)
train, test = train_test_split(df, test_size=0.3, random_state=0)
print(f"Training: {len(train)}, held out: {len(test)}")
~~~

~~~ text
Training: 70, held out: 30
~~~

読み込んだ直後に最終評価用の 30 人を取り分けます.
変数の組み合わせを比べるために使うのは, 残った学習用の 70 人です.

# 説明変数の組み合わせを交差検証で比べる {#variable-selection-test-mse}

GPA の予測に使う 4 つの候補は, 奨学生区分, 勉強時間, アルバイト時間, 運動時間です.
1 つ以上の変数を含む組み合わせは, 変数が 1 個の式が 4 通り, 2 個が 6 通り, 3 個が 4 通り, 4 個が 1 通りで, 合計 15 通りです.
どの回帰式にも切片を入れます.

[合成 GPA データの準備](#selection-lab-data)を実行した同じセッションで, 次のコードを順に実行します.
[交差検証で式を選ぶ例](block:reg-selection-gpa#selection-prediction)と同じ 5 分割を使います.
候補を公平に比べるため, すべての式で同じ 14 人ずつの組を順に検証用にします.

~~~ python
candidates = ["Scholarship", "Study_Hours", "Part_time_Work", "Sports_hours"]
folds = KFold(n_splits=5, shuffle=True, random_state=0)
splits = list(folds.split(train))

scores = []
for size in range(1, len(candidates) + 1):
    for columns in combinations(candidates, size):
        terms = " + ".join(columns)
        formula = "GPA ~ " + terms
        validation_errors = []
        for fit_indices, validation_indices in splits:
            fitting = train.iloc[fit_indices]
            validation = train.iloc[validation_indices]
            fit = smf.ols(formula, data=fitting).fit()
            predicted = fit.predict(validation)
            validation_errors.append(mean_squared_error(validation["GPA"], predicted))

        cv_mse = sum(validation_errors) / len(validation_errors)
        scores.append({"Terms": terms, "CV MSE": cv_mse})

ranking = pd.DataFrame(scores).sort_values("CV MSE")
print(ranking.to_string(index=False, float_format=lambda value: f"{value:.5f}"))
~~~

~~~ text
                                                    Terms  CV MSE
               Scholarship + Study_Hours + Part_time_Work 0.06265
Scholarship + Study_Hours + Part_time_Work + Sports_hours 0.06339
                 Scholarship + Study_Hours + Sports_hours 0.08155
                                Scholarship + Study_Hours 0.08188
                             Study_Hours + Part_time_Work 0.12145
              Study_Hours + Part_time_Work + Sports_hours 0.12351
                               Study_Hours + Sports_hours 0.14615
                                              Study_Hours 0.14713
                             Scholarship + Part_time_Work 0.28633
              Scholarship + Part_time_Work + Sports_hours 0.28727
                                           Part_time_Work 0.33731
                            Part_time_Work + Sports_hours 0.34264
                                              Scholarship 1.09765
                               Scholarship + Sports_hours 1.13172
                                             Sports_hours 1.19448
~~~

`combinations(candidates, size)` は順序を区別せず, `size` 個の変数を選ぶ組み合わせを返します.
当てはめ 1 回につき 56 人で係数を求め, 残りの 14 人で MSE を求めます.
候補ごとに 5 組の MSE を平均し, 交差検証の MSE を計算します.
表は表示用に丸める前の MSE が小さい順に並べています.

最小の CV MSE を得た式を 70 人で当てはめ直します.
テストデータの 30 人で予測し, 選んだ式のテスト MSE を求めます.

~~~ python
winner = ranking.iloc[0]
selected_formula = "GPA ~ " + winner["Terms"]
selected_fit = smf.ols(selected_formula, data=train).fit()
test_prediction = selected_fit.predict(test)
test_mse = mean_squared_error(test["GPA"], test_prediction)

print(f"Selected formula: {selected_formula}")
print(f"CV MSE:   {winner['CV MSE']:.5f}")
print(f"Test MSE: {test_mse:.5f}")
~~~

~~~ text
Selected formula: GPA ~ Scholarship + Study_Hours + Part_time_Work
CV MSE:   0.06265
Test MSE: 0.03363
~~~

同じ式でも, 検証用にどの学生を取るかで MSE は変わります.
15 通りから最小値を選ぶと, 新しいデータでの誤差に比べてたまたま MSE が小さく出た式も選ばれやすくなります.
選択に使う CV MSE が新しいデータでの誤差より小さめに出やすいのは, このように最小値を拾うためです.

今回のテスト MSE は CV MSE より小さくなりました.
CV MSE とテスト MSE では評価に使う学生が違い, 係数を求めた人数も 56 人と 70 人で異なります.
1 回の大小関係から, 選択による誤差のずれの大きさや向きは決まりません.
選んで当てはめ直した式の予測誤差は, 選択に使っていない 30 人で得たテスト MSE で報告します.

# 目的変数と無関係な変数を P 値で選ぶシミュレーション {#post-selection-simulation}

100 件の観測について, 50 個の説明変数と 1 個の目的変数を, 互いに独立な標準正規分布の乱数から作ります.
母集団での回帰係数はすべて 0 です.
このデータに [P 値を使った後退法](block:reg-selection-gpa#selection-exploration)を適用し, いくつの変数が残るかを調べます.

観測数と候補数は @freedman1983note [sec. 2] の実験にそろえます.
Freedman は最初の回帰で 25% 水準を満たした変数をまとめて残し, 同じデータ上で 2 回目の回帰と検定を行いました.
この実験では, 5% 水準で P 値が最も大きい変数を 1 つずつ除き, 毎回回帰し直します.
このページのコードは乱数から始まるので, GPA の読み込みは必要ありません.

~~~ python
import numpy as np
import pandas as pd
import statsmodels.api as sm


def make_sample(rng, sample_size, candidate_count):
    """独立な標準正規乱数から説明変数と目的変数を作る."""
    names = [f"x{index}" for index in range(candidate_count)]
    values = rng.normal(size=(sample_size, candidate_count))
    predictors = pd.DataFrame(values, columns=names)
    response = pd.Series(rng.normal(size=sample_size))
    return predictors, response


def backward_eliminate(predictors, response):
    """P 値が 0.05 以上の変数のうち, 最も大きいものを 1 つずつ除く."""
    selected = list(predictors.columns)
    while selected:
        fit = sm.OLS(response, sm.add_constant(predictors[selected])).fit()
        p_values = fit.pvalues.drop("const")
        largest = p_values.idxmax()
        if p_values[largest] < 0.05:
            return selected, fit
        selected.remove(largest)
    return selected, None


rng = np.random.default_rng(0)
predictors, response = make_sample(rng, sample_size=100, candidate_count=50)
selected, fit = backward_eliminate(predictors, response)
print(f"Selected variables: {selected}")
if fit is not None:
    print(fit.pvalues.drop("const").to_string(float_format=lambda value: f"{value:.3g}"))
    print(f"R-squared: {fit.rsquared:.3f}, overall F-test p-value: {fit.f_pvalue:.4f}")
~~~

~~~ text
Selected variables: ['x10', 'x32', 'x49']
x10   0.000291
x32     0.0106
x49     0.0328
R-squared: 0.179, overall F-test p-value: 0.0003
~~~

`backward_eliminate` は残った変数と最後の当てはめ結果を返します.
すべて除かれた場合は, 空のリストと `None` を返します.
1 回目の試行では 3 変数とも P 値が 0.05 未満になり, 式全体の F 検定の P 値も 0.0003 でした.
目的変数と説明変数を独立に生成したので, 残った 3 変数も母集団では目的変数と無関係です.

新たに 200 回の試行を行い, 各回で残った変数の数を記録します.
変数が残った回では, 別に生成した同じ大きさの標本で, 選んだ式を当てはめ直します.
別の標本でも P 値が 0.05 未満だった変数を数えます.
試行数が多いので, 実行には時間がかかる場合があります.

~~~ python
def run_simulation(rng, sample_size, candidate_count, trials):
    """各回の残存数と, 別の標本で P 値が 0.05 未満になった延べ数を返す."""
    counts = []
    confirmed = 0
    for _ in range(trials):
        predictors, response = make_sample(rng, sample_size, candidate_count)
        selected, _ = backward_eliminate(predictors, response)
        counts.append(len(selected))
        if selected:
            new_predictors, new_response = make_sample(rng, sample_size, candidate_count)
            fresh_fit = sm.OLS(
                new_response,
                sm.add_constant(new_predictors[selected]),
            ).fit()
            confirmed += (fresh_fit.pvalues.drop("const") < 0.05).sum()
    return np.array(counts), confirmed


counts, confirmed = run_simulation(rng, sample_size=100, candidate_count=50, trials=200)
with_selection = (counts > 0).sum()
total_selected = counts.sum()
confirmation_rate = confirmed / total_selected

print(f"Trials with at least one variable: {with_selection}/{len(counts)}")
print(f"Mean number of selected variables: {counts.mean():.2f}")
print(
    f"Significant in new samples: {confirmed}/{total_selected} ({confirmation_rate:.1%})"
)
~~~

~~~ text
Trials with at least one variable: 184/200
Mean number of selected variables: 3.86
Significant in new samples: 35/772 (4.5%)
~~~

選択に使ったデータでは, 200 回のうち 184 回 (92%) で 1 変数以上が残りました.
200 回で残った延べ変数のうち, 別の標本でも P 値が 0.05 未満だったのは 4.5% です.
前者の分母は試行数, 後者の分母は選ばれた延べ変数の数です.

後退法は, 同じデータ上で P 値が小さかった変数を残します.
別の標本で式を固定して当てはめ直せば, 標準正規乱数から作ったこの例では, 無関係な変数が 5% 水準で有意になる確率は変数ごとに 5% です.
観測した 4.5% はこの割合に近い値です.
実データで検定するときは, [選んだ後の推論](block:reg-selection#post-selection)で扱うように, 確認用の標本を独立に取り, 使う検定の前提を確かめます.

# 演習 {#exercises}

## Exercise REG-SELECTION-LAB-1 {#exercise-reg-selection-lab-1}

**候補の数を減らしたときに残る変数の数**

標本の大きさを 100, 試行数を 200 に固定し, 候補が 10 個の場合と 50 個の場合を比べてください.
1 変数以上が残った試行の割合と, 試行あたりに残った変数の平均を求めます.
2 つの設定から得た値を比べ, 母集団でどの回帰係数も 0 であることを踏まえて結果を読んでください.

<details class="protected" data-pass="yakagika">
<summary>回答</summary>

[シミュレーション](#post-selection-simulation)の import と 3 つの関数の定義を使います.
それぞれの候補数について乱数生成器を同じ種で作り, 200 回ずつ試行します.

~~~ python
for candidate_count in (10, 50):
    rng = np.random.default_rng(0)
    counts, _ = run_simulation(
        rng,
        sample_size=100,
        candidate_count=candidate_count,
        trials=200,
    )
    with_selection = (counts > 0).mean()
    print(f"{candidate_count} candidates: {with_selection:.1%}, mean {counts.mean():.2f}")
~~~

~~~ text
10 candidates: 42.5%, mean 0.57
50 candidates: 92.5%, mean 3.88
~~~

候補が 50 個の場合は, 10 個の場合よりも多くの試行で何らかの変数が残り, 残った変数の平均も大きくなりました.
どちらの場合も母集団での係数はすべて 0 です.
候補数が増えると, 標本の中でたまたま大きな関連が出た変数を選ぶ機会が増えます.

</details>
