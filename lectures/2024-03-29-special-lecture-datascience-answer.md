---
title: 特別講義(データサイエンス)演習回答
description: 資料
tags: datascience, lecture, statistics, python
featured: true
tableOfContents: true
---


[特別講義(データサイエンス)](https://yakagika.github.io/lectures/2024-03-29-special-lecture-datascience.html)の演習回答例です.

講義中の学生の回答を参考にSAにまとめてもらったものです.
修正点などありましたら,教員に連絡してください.

# 3.3 演習
```python
# 飴が40個あります.7人で同じ数ずつ分けると1人分は何個で何個あまりますか?
# 1人分の個数
print(40//7)
# 余り
print(40%7)

# 底辺5cm,高さ4cmの三角形の面積はいくつですか?
print(5*4*0.5)

# 2の8乗はいくつですか?
print(2**8)

# 累乗と掛け算の計算順序を丸括弧を使った計算で確かめてください.
print(2+3*4)
print((2+3)*4)
```

# 3.4 演習
```python
# 変数を利用して以下の猫型ロボットのBMIを計算してください
# BMI = 体重(kg)÷身長(m)の2乗
# 猫型ロボットの身長 129.3cm
# 猫型ロボットの体重 129.3kg

height_cm = 129.3
weight = 129.3
height_m = height_cm/100

BMI = weight/(height_m**2)

print(BMI)
```

# 3.5 演習
```python
x = 'abcdefg'

# 'abcdefg' から 'cde'をスライスで抜き出してください.
print(x[2:5])

# x = 'abcdefg'と定義して, xに操作を加えて'abfg'を作ってください.
print(x[0:2] + x[5:])

# x = 'abcdefg'と定義して, xに操作を加えて'bbbeee'を作ってください.
print(x[1]*3 + x[4]*3)
```

# 3.6 演習
```python
xs = [[1,2,3],[4,5,6],[7,8,9]]

# xsの長さを求める
print(len(xs))


# スライスを使って以下を抽出する
## [[4,5,6],[7,8,9]]
print(xs[1:])

## [[1,2,3]]
print(xs[:1])

## [[7,8,9]]
print(xs[2:])

## [8,9]
print(xs[2][1:])


# [4,5,6]を[-4,-5,-6]に更新する
xs[1] = [-4, -5, -6]
print(xs)


# 1 を -1に,9を-9にする
xs[0][0] = -1
xs[2][2] = -9
print(xs)


# [7,8,-9]のあとに,[10,11,12]を追加する
a = [10,11,12]
xs.append(a)
print(xs)
```

# 3.8 演習
```python
fruits = {"リンゴ":"apple"
		 ,"レモン":"lemon"
		 ,"ブドウ":"grape"
		 ,"バナナ":"banana"
		 ,"イチゴ":"strawberry"}
print(fruits)


# keyを指定してvalueにアクセス
print(fruits["リンゴ"])

# 新しいkeyとvalueの追加
fruits["モモ"] = "peach"
print(fruits)

# valueの変更
fruits["リンゴ"] = "ringo"
print(fruits)
fruits["リンゴ"] = "apple"
print(fruits)

# keyを指定して要素を削除
fruits.pop("レモン")
print(fruits)

# keyの一覧を取得
keys = fruits.keys()
print(keys)

# 値の一覧を取得
values = fruits.values()
print(values)

# リストとして値の一覧を取得
print(list(fruits.values()))

# keyとvalueのペアの一覧(タプルのリスト)を取得
items = fruits.items()
print(items)
```

# 3.9 演習
```python
# ある値が偶数かどうかは,2で割った余りが0かどうかを判定することで判定できます.
# x=101,y=202として, 以下の命題の真偽をPythonで計算してください.

x = 101
y = 102

# xが偶数
print(x%2 == 0)

# yが偶数
print(y%2 == 0)

# xが偶数かつyが偶数
print(x%2==0 and y%2==0)

# xが偶数またはyが偶数
print(x%2==0 or y%2==0)

# x+yが奇数
print((x+y)%2 == 1)
```

# 5.9 演習
```python
import pandas as pd

df = pd.read_csv('data/salary.csv')
print(df)

# ①
print(df.loc[1:13, 'industry':'salary'])
print(df.iloc[1:14, 0:])

# ②
print(df.at[4, 'salary'])
print(df.iat[4, 1])

# ③
print(df.loc[[6,8,10], ['industry']])
print(df.iloc[[6,8,10], [0]])
```

# 5.10 演習
```python
import pandas as pd

df = pd.read_csv('data/salary.csv')
df.columns = ['Ind','Sal']
print(df)


# Sal列が偶数のSal列の行
print(df[df['Sal'] %2==0 ]['Sal'])


# Sal列が奇数のInd列の行
print(df[df['Sal'] %2==1 ]['Ind'])


# Ind列がInfo,Edu,Medのいずれかで,Sal列が4000以上5000未満のSal列
x = df.loc[8:10, 'Sal']
print(x)
y = x[(x >= 4000) & (x < 5000)]
print(y)

print(df[(df['Ind'].isin(['Information', 'Academic', 'Medical'])) & (df['Sal'] >= 4000) & (df['Sal'] < 5000)]['Sal'])
```

# 5.11 演習
```python
import pandas as pd

df = pd.read_csv('data/salary.csv')
df.columns = ['Ind','Sal']
print(df)

# columnsを日本語(産業,給与)に変更
df.columns = ['産業','給与']
print(df)

# サービス業だと思われる行のみを抽出して,copy()
df2 = df.loc[[3,9,11,12],['産業','給与']].copy()
print(df2)

# コピーされたDataFrameに0から始まるインデックスを振り直す
df2 = df2.reset_index(drop=True)
print(df2)

# 元のDataFrameとコピーされたデータフレームの中身とidを確認
print('id df:',id(df))
print('id df2:',id(df2))
```

# 5.13 演習
```python
import pandas as pd

values = [['Taro',20]
         ,['Jiro',30]
         ,['Hanako',40]]

df = pd.DataFrame(data=values
                 ,columns=['name'
                          ,'age'])
print(df)

# age列をfloat型に変更
df['age'] = df['age'].astype(float)
print(df)
print(df['age'].dtypes)
```

# 5.17 演習①
```python
import pandas as pd

df = pd.DataFrame(index = range(20))
df['1'] = df.index
df['2'] = df.index*2
df['3'] = df.index*3
df['4'] = df.index*4
df['5'] = df.index*5
print(df)

#df = pd.DataFrame()
#for i in range(1,6):
#    x = []
#    for j in range(20):
#        x.append(i*j)
#    df[str(i)] = x
#print(df)


# '3'列を2倍した'6'という列を追加
df['6'] = df['3']*2
print(df)

# indexが20となる行を追加
df.loc[20] = df.loc[1]*20
print(df)

# 'mean'という各行の平均値からなる列を追加
df['mean'] = df.mean(axis=1)
print(df)

# indexが偶数の列のみを残して,すべての列をint型に変更した後BOM付きのUTF-8のcsvで保存
y = df[df.index%2 == 0]
print(y)

y = y.astype(int)
print(y)
print(y.dtypes)

y.to_csv('data/5.17_ensyu1.csv', encoding='utf-8-sig')
```

# 5.17 演習②
```python
import pandas as pd


#データをrice.csvとしてutf-8で保存します.
#今回は'米相場'列だけを残してあります.
df = pd.read_csv('data/rice.csv')
print(df)

df = pd.read_csv('data/rice.csv')
print(df.isna())

print("#欠損値をNaNに----------------------------------")
# pd.to_numeric'米相場'列を数値型に変換
#'coerce'：変換できない値をNaNに変換します。
df['米相場'] = pd.to_numeric(df['米相場'], errors='coerce')

# DataFrameを確認して、変換後の値を確認
print(df)
print("#NaNを0に〇----------------------------------\n",
"@NaNを0に置き換える（必要に応じて適切な値を選択\n",
"@filledは欠損値を特定の値で埋める\n",
"@df.filled(特定の値,元のデータを直接変える）\n",
"@df['米相場']=df['米相場'].fillna(0))\n"
"@しかしこれではデータのサイズが変わってしまうので平均が狂う\n",
"#NaNの削除◯----------------------------------\n")
print(df.dropna(subset=['米相場'],how='any'))
print(df)
print("#浮動小数点数型に変換〇----------------------------------")
# '米相場'列を浮動小数点型に変換
df['米相場'] = df['米相場'].astype(float)
print(df)
print("#平均値の計算〇----------------------------------")
# 平均値の計算（元の数値データを使うため、再計算）
df.loc["average","米相場"] =df.loc[0:,"米相場"].mean()
print(df) 
```