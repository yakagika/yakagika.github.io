import numpy as np
import pandas as pd
import statsmodels.api as sm
import matplotlib.pyplot as plt
import matplotlib_fontja
import seaborn as sns
from sklearn.preprocessing import StandardScaler

# 同じ標本を再現できるように, 乱数シードを固定する.
np.random.seed(42)

# ---------------------------
# 1. データ生成
# ---------------------------
N = 100  # 100 人分のデータを生成する.

# 奨学生 (True) と非奨学生 (False) を同じ確率で割り当てる.
scholarship = np.random.choice([True, False], size=N)

# 週あたりの勉強時間を平均 10 時間, 標準偏差 4 時間の正規分布から得る.
study_hours = np.random.normal(loc=10, scale=4, size=N)

# 週あたりの運動時間を平均 5 時間, 標準偏差 2 時間の正規分布から得る.
sports_hours =  np.random.normal(loc=5, scale=2.0, size=N)

# 週あたりの勉強時間が 1 時間長いと, 週あたりのアルバイト時間が 0.6 時間短くなる.
# 20 - 0.6 * Study_Hours に, 平均 0, 標準偏差 2 の正規分布から得た誤差を足す.
part_time_work = 20 - 0.6 * study_hours + np.random.normal(loc=0, scale=2, size=N)

# 切片 0.5 に, 奨学生なら 0.6 を足す.
# 勉強時間 1 時間あたり 0.25 を足し, アルバイト時間 1 時間あたり 0.1 を引く.
# 平均 0, 標準偏差 0.1 の正規分布から得た誤差を足す.
gpa = (
    0.5
    + 0.6 * scholarship
    + 0.25 * study_hours
    - 0.1 * part_time_work
    + np.random.normal(loc=0, scale=0.1, size=N)
)

# GPA が 0 を下回るときは 0, 4 を上回るときは 4 にする.
gpa = np.clip(gpa, 0, 4)

# 生成した値をデータフレームにまとめる.
df = pd.DataFrame({
    'GPA': gpa,
    'Scholarship': scholarship,
    'Study_Hours': study_hours,
    'Sports_hours':sports_hours,
    'Part_time_Work': part_time_work
})

df.to_csv('../../slds_data/ch11/multiple_regression.csv',encoding='utf-8-sig')
# ---------------------------
# 2. 回帰分析
# ---------------------------

pd.plotting.scatter_matrix(df[['GPA'
                              ,'Study_Hours'
                              ,'Sports_hours'
                              ,'Part_time_Work']], range_padding=0.2)
plt.show()

sns.pairplot(df[['GPA'
                ,'Study_Hours'
                ,'Sports_hours'
                ,'Part_time_Work'
                ,'Scholarship']]
                , diag_kind="hist"
                , hue="Scholarship"
                , palette="Set2"
                , diag_kws=dict(bins=8))
plt.suptitle("Scatterplot Matrix with Scholarship", y=1.02)
plt.show()

#ヒートマップで確認
sns.heatmap(df[['GPA'
               ,'Study_Hours'
               ,'Sports_hours'
               ,'Part_time_Work']].corr()
            ,vmax=1
            ,vmin=-1
            ,annot=True)
plt.show()

#joinplotで男女別に密度プロットを表示
fig = sns.jointplot(data = df
                   ,x ="Study_Hours"
                   ,y ="GPA"
                   ,hue='Scholarship'
                   ,joint_kws = dict(alpha=0.5))
plt.show()


# カテゴリカルデータのダミー変数化
# get_dummies関数を使う(dtype='it'で0/1の数値に変換)
df = pd.get_dummies(df,columns=['Scholarship'],dtype='int')
print(df)

#標準化
scaler = StandardScaler()
df[['Scholarship_True'
   ,'Study_Hours'
   ,'Part_time_Work'
   ,'Sports_hours']] = scaler.fit_transform(df[['Scholarship_True'
                                               ,'Study_Hours'
                                               ,'Part_time_Work'
                                               ,'Sports_hours']])

"""
#正規化
from sklearn.preprocessing import MinMaxScaler

# 正規化（0〜1に変換）
scaler = MinMaxScaler()
df[['Scholarship_True',
    'Study_Hours',
    'Part_time_Work',
    'Sports_hours']] = scaler.fit_transform(df[['Scholarship_True'
                                               ,'Study_Hours'
                                               ,'Part_time_Work'
                                               ,'Sports_hours']])
"""

# 説明変数(X)と目的変数(y)に分割
X = df[['Scholarship_True', 'Study_Hours','Sports_hours']]
y = df['GPA']


# 切片(定数項)を追加
X = sm.add_constant(X)

# 回帰モデルを作成・フィット
result = sm.OLS(y, X).fit()

# 結果を表示
print(result.summary())

#回帰グラフの作成
from statsmodels.graphics.regressionplots import plot_partregress_grid
fig = plt.figure(figsize=(16,8))
plot_partregress_grid(result, fig=fig)
plt.show()


#予測結果の作成
pred = result.predict(X)
plt.figure(figsize=(16,8))
sns.kdeplot(pred, label = 'Predicted')
sns.kdeplot(y, label = 'Actual')
plt.title('Actual/Predicted')
plt.xlabel('GPA')
plt.ylabel('Density')
plt.legend()
plt.show()

# 説明変数(X)と目的変数(y)に分割
X = df[['Scholarship_True', 'Study_Hours']]
y = df['GPA']

# 切片(定数項)を追加
X = sm.add_constant(X)

# 回帰モデルを作成・フィット
result = sm.OLS(y, X).fit()

# 結果を表示
print(result.summary())



plt.figure(figsize=(8, 6))
plt.scatter(y, pred, alpha=0.7, edgecolors="k")
plt.xlabel("Predicted")
plt.ylabel("Actual")
plt.title("Actual vs. Predicted")
plt.plot([y.min(), y.max()], [y.min(), y.max()], color="red", linestyle="--")  # 完全一致のライン
plt.grid()
plt.savefig('../../images/ch11-multi-regression5.png')
plt.close()

#予測結果の作成
pred = result.predict(X)
plt.figure(figsize=(16,8))
sns.kdeplot(pred, label = 'Predicted')
sns.kdeplot(y, label = 'Actual')
plt.title('Actual/Predicted')
plt.xlabel('GPA')
plt.ylabel('Density')
plt.legend()
plt.savefig('../../images/ch11-multi-regression6.png')
plt.close()


#回帰グラフの作成
from statsmodels.graphics.regressionplots import plot_partregress_grid
fig = plt.figure(figsize=(16,8))
plot_partregress_grid(result, fig=fig)
plt.savefig('../../images/ch11-multi-regression7.png')
plt.close()



