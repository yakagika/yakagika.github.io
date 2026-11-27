---
title: 単回帰の当てはめ
kind: code
open: true
tags: fixture-block-regression
requires:
  - sample-overview
bibliography: lectures
---

FIXTURE_FITTING_INTRO

[当てはめ](block:sample-fitting#fit), [数式](#数式), [当てはめの演習](#exercise-sample-fitting-1)を参照できます.

# 当てはめ {#fit}

## 数式

例のモデルは $y = a + bx$ です. 回帰の文献に [@james2021introduction] があります. [標本の確認](block:sample-extension#check-sample)も参照できます.

<span id="formula-note">当てはめの式を確かめます.</span>

~~~ python
a, b, x = 2, 3, 1
y = a + b * x
~~~

# 残差 {#residuals}

観測値と当てはめ値の差を残差と呼びます.

# 演習 {#exercises}

## Exercise SAMPLE-FITTING-1 {#exercise-sample-fitting-1}

[当てはめ](#fit)の式に数値を代入します.

<details class="protected" data-pass="sample">
<summary>回答</summary>
<p>説明変数に 1 を代入すると a + b になります.</p>
</details>
