---
title: 特別講義DS Ch14 画像認識
description: 資料
tags:
    - datascience
    - statistics
    - python
featured: true
date: 2024-09-28
tableOfContents: true
previousChapter: slds13.html
nextChapter: slds15.html
---

# 画像認識(執筆中)

説明

主な画像認識モデルの歴史は以下のようにまとめられます.

| 年代  | モデル名              | 特徴・概要                                                                                       | 主な貢献や革新点                                                |
|-------|-----------------------|---------------------------------------------------------------------------------------------------|-----------------------------------------------------------------|
| 1998  | LeNet                  | 手書き数字認識に使われた初期のCNNモデル. 主にMNISTデータセットで使用.                                | 畳み込み層とプーリング層を使用した初期の画像認識モデル.         |
| 2012  | AlexNet                | ImageNetで初めて畳み込みニューラルネットワークを使用し,トップ5エラー率を大幅に改善.                   | ReLU活性化関数の使用や,GPUによるトレーニングの導入.            |
| 2014  | VGGNet                 | 小さな3x3のフィルタを多層に積み重ねたアーキテクチャ.VGG16とVGG19が特に有名.                         | モデルの深さが精度に大きく寄与することを示す.                  |
| 2014  | GoogLeNet (Inception)  | "Inception Module"を採用し,計算効率と精度を両立.                                                 | 異なるサイズの畳み込みフィルタを同時に適用するInceptionモジュール.|
| 2015  | ResNet                 | 残差ブロックを使用し,非常に深いネットワークのトレーニングを可能に.ImageNetで優勝.                  | 152層の深いネットワークで,勾配消失問題を解決.                |
| 2016  | DenseNet               | 各層がすべての前層からの入力を受け取る密結合アーキテクチャ.                                       | パラメータ効率が良く,勾配の流れが改善される.                  |
| 2017  | Xception               | 畳み込みの代わりに「深さ方向の畳み込み（Depthwise Separable Convolutions）」を使用.                | 計算コストの削減と精度の向上.                                  |
| 2017  | MobileNet              | 軽量かつ効率的なCNNアーキテクチャ.モバイルデバイス向けに最適化.                                  | 計算量を削減し,モバイル環境でのリアルタイム推論を可能に.      |
| 2020  | Vision Transformer (ViT)| 画像をパッチに分割し,トランスフォーマーアーキテクチャを使用したモデル.                            | トランスフォーマーモデルが画像認識タスクでも有効であることを示す.|
| 2021  | Swin Transformer       | 階層的なトランスフォーマーアーキテクチャで,局所的な窓（ウィンドウ）を用いた画像認識モデル.           | トランスフォーマーのスケーラビリティを改善し,高い精度を達成.  |
| 2022  | ConvNeXt               | Vision Transformerのアイデアを取り入れた畳み込みネットワークの進化版.                              | 畳み込みベースのモデルが再び最先端性能を達成できることを示す.   |

今回は,事例としてConvNeXtを利用した画像認識を実行してみます.

[ConvNeXt](https://github.com/facebookresearch/ConvNeXt)は,Meta(旧Facebook)によって発表されたモデルで,Vision Transformer (ViT) を参考にしつつResNet（Residual Network）を基盤として作られたCNNです.

PythonのCNNのライブラリはいくつか存在しますが,ConvNeXtは,Metaによって開発された`PyTorch`上で実装されています.




## 顔による年齢識別

事例として顔画像からの年齢識別を行ってみましょう. データとして,16歳から62歳までの2,000人の有名人の160,000以上の画像が含まれるデータセット[Cross-Age Celebrity Dataset (CACD)](http://bcsiriuschen.github.io/CARC/)を用います.

![The dataset metadata](/images/CACD.png)

`The dataset metadata only can be downloaded`をクリックしてメタデータを, `Original face images (detected and croped by openCV face detector) can be downloaded`をクリックして画像データをダウンロードしてください(3Gあるので通信環境に注意).

`CACD2000.tar.gz`は展開して,`celebrity2000_meta.mat`とともにプログラムを配置するディレクトリ内の`data`ディレクトリに保存しておきましょう.

### 画像ファイルの形式

機械学習において利用されるラベル付き画像データの形式はいくつかあるが,CACDのような`.mat`ファイル,画像とCSVなどのラベルの組み合わせ,ラベル名フォルダ別の画像ファイルなどのパターンが存在する. いずれにも対応できるようにしておく必要があるが, この資料では最も単純な最後のラベル別に名前がつけられたフォルダに保存された画像ファイルを扱う.

先ほどダウンロードした`celebrity2000_meta.mat`は,メタデータのみが含まれており,画像は別になっています.メタデータに従って,年齢別に画像をフォルダに保存してみましょう.

~~~ sh
> ls
face_image.py
data

> ls data
CACD2000
celebrity2000_meta.mat
~~~


::: note
- `.mat`ファイル
---
    - MATLABのファイル
    - 基本的には `scipy` を利用して読み込む.
    - フォーマット形式がMATLAB `v7.3`の場合には,`HDF5`を扱うライブラリ`h5py`を利用する.
    - `HDF5(Hierarchical Data Froamt version 5)`はディレクトリ構造に似た階層型のデータフォーマット
:::


まずは,`h5py`を利用して`celebrity2000_meta.mat`を読み込み,中身を確認してみましょう.

::: warn
以下のライブラリが必要になるので `pip install`しておいてください.

- `pytorch`
    - CNN用ライブラリ(PyTorch)
- `torchvision`
    - PyTorchの画像,動画処理用ライブラリ
- `scipy`
- `h5py`
- `pillow`
    - 画像処理用ライブラリ
- `shutil`
    - ファイル操作用ライブラリ
:::

~~~ py
import h5py #HDF5を扱うライブラリ
from PIL import Image #画像の表示/保存/書き込みなどを扱うライブラリ
import os
import numpy as np
import scipy.io

#画像データの保存先
image_dir = 'data/CACD2000'

# .matファイルの読み込み（古い形式の場合）
## 辞書型として読み込まれる
file = scipy.io.loadmat('data/celebrity2000_meta.mat')

# 辞書のKeyを表示する
print('keys:',file.keys())
# >>> dict_keys(['__header__', '__version__', '__globals__', 'celebrityData', 'celebrityImageData'])

#celebrityImageDataの確認
print(file['celebrityImageData'])

"""
[[(array([[53],
         [53],
         [53],
         ...,
         [23],
         [23],
         [23]], dtype=uint8), array([[   1],
         [   1],
         [   1],
         ...,
         [2000],
         [2000],
         [2000]], dtype=uint16), array([[2004],
         [2004],
         [2004],
         ...,
         [2013],
         [2013],
         [2013]], dtype=uint16), array([], shape=(0, 0), dtype=uint8), array([[ 1],
         [ 1],
         [ 1],
         ...,
         [50],
         [50],
         [50]], dtype=uint8), array([[1],
         [1],
         [1],
         ...,
         [0],
         [0],
         [0]], dtype=uint8), array([[1951],
         [1951],
         [1951],
         ...,
         [1990],
         [1990],
         [1990]], dtype=uint16), array([[array(['53_Robin_Williams_0001.jpg'], dtype='<U26')],
         [array(['53_Robin_Williams_0002.jpg'], dtype='<U26')],
         [array(['53_Robin_Williams_0003.jpg'], dtype='<U26')],
         ...,
         [array(['23_Katie_Findlay_0011.jpg'], dtype='<U25')],
         [array(['23_Katie_Findlay_0012.jpg'], dtype='<U25')],
         [array(['23_Katie_Findlay_0013.jpg'], dtype='<U25')]], dtype=object))                ]]
"""
# 7個目に画像のファイル名が入っているので
# celebrityImageDataから画像ファイル名を抽出
image_data = file['celebrityImageData']
jpg_files = [str(image_name[0][0]) for image_name in image_data[0][0][7]]

# 抽出された.jpgファイル名のリストを上から10個表示
print(jpg_files[:10])

# 画像データの取得と表示
## 名前データを利用して画像をいくつか開いてみます.
for n in jpg_files[:10]:
    img_path = os.path.join(image_dir, n)  # パスを結合し,ファイル名を取得

    if os.path.exists(img_path):
        img = Image.open(img_path)  # 画像ファイルを開く
        img.show()  # 画像を表示
    else:
        print(f"Image file not found: {img_path}")
    #>>> 画像が表示されます
~~~




::: warn
- HDF5の利用例



`CACD`データのうち一番上の`The dataset metadata and features used in this paper`からダウンロードできる`celebrity2000.mat`は,`HDF5`のデータとなっているため,`scipy`で読み込んでみるとエラーが出ます.

~~~ py
file = scipy.io.loadmat('data/celebrity2000.mat')
"""
Traceback (most recent call last):
  File "/Users/akagi/Desktop/face_image.py", line 86, in <module>
    file = scipy.io.loadmat('data/celebrity2000.mat')
           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/akagi/.pyenv/versions/3.12.3/lib/python3.12/site-packages/scipy/io/matlab/_mio.py", line 226, in loadmat
    MR, _ = mat_reader_factory(f, **kwargs)
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/Users/akagi/.pyenv/versions/3.12.3/lib/python3.12/site-packages/scipy/io/matlab/_mio.py", line 80, in mat_reader_factory
    raise NotImplementedError('Please use HDF reader for matlab v7.3 '
NotImplementedError: Please use HDF reader for matlab v7.3 files, e.g. h5py

"""
~~~


今回は`celebrity2000_meta.mat`を利用するので必要ありませんが,試しに同じように画像を表示してみましょう.

<details>
    <summary> 詳細 開く/閉じる </summary>


`HDF5`は多重の辞書型ような構造をしており,`key`によってデータにアクセスできます.

~~~ py
import h5py #HDF5を扱うライブラリ
from PIL import Image #画像の表示/保存/書き込みなどを扱うライブラリ
import os
import numpy as np

# .matファイル(HDF5)の読み込み
with h5py.File('data/celebrity2000.mat', 'r') as file:
    # List all keys in the .mat file
    print('keys:',list(file.keys()))
    # >>> keys: ['#refs#', 'celebrityData', 'celebrityImageData']

    ## ラベルの確認
    print('DataKeys:',file['celebrityImageData'].keys())
    # >>> DataKeys: <KeysViewHDF5 ['age', 'birth', 'feature', 'identity', 'lfw', 'name', 'rank', 'year']>

    # 年齢データの確認
    print('age:',file['celebrityImageData']['age'])
    # >>> age: <HDF5 dataset "age": shape (1, 163446), type "<f8">
    print('age:',file['celebrityImageData']['age'][0])
    # >>> age: [53. 53. 53. ... 23. 23. 23.]

    # 名前データの確認
    print('name:',file['celebrityImageData']['name'])
    # >>> name: <HDF5 dataset "name": shape (1, 163446), type "|O">
    print('name:',file['celebrityImageData']['name'][0])
    # >>> name: [<HDF5 object reference> <HDF5 object reference> <HDF5 object reference>
    #... <HDF5 object reference> <HDF5 object reference>
    #<HDF5 object reference>]
    # ↑ <HDF5 object reference>は他のHDF5オブジェクトへの参照 #refs#に入っている.

    # nameデータを参照して表示
    name_references = file['celebrityImageData']['name'][0]
    names = []
    for ref in name_references:
        name = file[ref][()].tobytes().decode('utf-16')  # utf-16でデコード
        names.append(name)

    # 最初の10件の名前を表示
    print('names:', names[:10])
    # names: ['53_Robin_Williams_0001.jpg'
    # , '53_Robin_Williams_0002.jpg'
    # , '53_Robin_Williams_0003.jpg'
    # , '53_Robin_Williams_0004.jpg'
    # , '53_Robin_Williams_0005.jpg'
    # , '53_Robin_Williams_0006.jpg'
    # , '53_Robin_Williams_0007.jpg'
    # , '53_Robin_Williams_0009.jpg'
    # , '53_Robin_Williams_0010.jpg'
    # , '53_Robin_Williams_0011.jpg']

    # 画像データの取得と表示
    ## 名前データを利用して画像をいくつか開いてみます.
    for n in names[:10]:
        img_path = os.path.join(image_dir, n)  # パスを結合し,ファイル名を取得

        if os.path.exists(img_path):
            img = Image.open(img_path)  # 画像ファイルを開く
            img.show()  # 画像を表示
        else:
            print(f"Image file not found: {img_path}")
    # >>> 画像が表示される
~~~

同じ用にデータを抽出できることが確認できます.

</details>

:::


それでは,`celebrity2000_meta.mat`から年齢別にフォルダを分けて画像を保存してみます.年齢区分は,`10`,`20`,...,`100`としてみましょう. 画像ファイル名の先頭の数字が年齢を表しているので,そちらを利用しても構いませんが,せっかくなのでメタデータを利用してみましょう. 年齢は`image_data[0][0][0]`に入っているようです.

研究であれば画像データの枚数は多いほど良いですが, 今回は一通りの流れを体験してみることが目的なので学生の環境でも利用しやすいように各年代200枚だけコピーします.

~~~ py
import os
import shutil
import scipy.io
from collections import defaultdict
import random

# 画像ディレクトリの設定
image_dir = 'data/CACD2000'
output_dir = 'data/sorted_images'

# .matファイルの読み込み
file = scipy.io.loadmat('data/celebrity2000_meta.mat')

# celebrityImageDataから年齢と画像ファイル名を抽出
image_data = file['celebrityImageData']
# 年齢情報
ages = image_data[0][0][0].flatten()
# 画像ファイル名
jpg_files = [str(image_name[0][0]) for image_name in image_data[0][0][7]]

# 年齢と画像ファイルをペアにする
age_image_pairs = list(zip(ages, jpg_files))

# 年代ごとの画像カウント
age_group_counts = defaultdict(int)

# 年齢別に画像をシャッフル
random.shuffle(age_image_pairs)

# 年齢別のフォルダに画像をコピー（各年代最大200枚）
for age, jpg_file in age_image_pairs:
    age_group = (age // 10) * 10
    if age_group > 100:
        age_group = 100  # 100代以上は100代フォルダに保存

    # 各年代ごとに200枚までコピー
    if age_group_counts[age_group] < 200:
        folder_path = os.path.join(output_dir, f'{age_group}s')
        os.makedirs(folder_path, exist_ok=True)

        src_path = os.path.join(image_dir, jpg_file)
        dst_path = os.path.join(folder_path, jpg_file)

        shutil.copy(src_path, dst_path)
        age_group_counts[age_group] += 1
~~~

結果を確認してみます.

::: warn

Shell コマンドにおける`|` は`パイプ`といって `head -20`は先頭20個のみ
`|` の左側のコマンドによる標準出力を右側のコマンドに渡すことができます.

今回は`ls data/sorted_images/10s`で表示される結果の,先頭20個のみを表示しています.

:::

~~~ sh
> ls data/sorted_images
10s 20s 30s 40s 50s 60s
> ls data/sorted_images/10s |head -20
19_Alison_Pill_0001.jpg
19_Alison_Pill_0002.jpg
19_Alison_Pill_0003.jpg
19_Alison_Pill_0005.jpg
19_Alison_Pill_0006.jpg
19_Alison_Pill_0007.jpg
19_Alison_Pill_0009.jpg
19_Alison_Pill_0011.jpg
19_Amanda_Seyfried_0001.jpg
19_Amanda_Seyfried_0002.jpg
19_Amanda_Seyfried_0004.jpg
19_Amanda_Seyfried_0005.jpg
19_Amanda_Seyfried_0007.jpg
19_Amanda_Seyfried_0008.jpg
19_Amanda_Seyfried_0010.jpg
19_Amanda_Seyfried_0011.jpg
19_Amanda_Seyfried_0013.jpg
19_Amanda_Seyfried_0014.jpg
19_Anna_Kendrick_0002.jpg
19_Anna_Kendrick_0008.jpg
~~~

データには10代から60代までのみが含まれていたようです. 各フォルダの中身を確認してもちゃんと保存できていることがわかりますね.


機械学習モデルの性能を評価するためには,学習に利用する訓練用データと,学習の結果を判定するテスト用データに分ける必要があります. 続いて,学習用とテスト用でフォルダに分割してみましょう.

今回は200枚の画像のうち8割(160枚)を学習用,2割(40枚)をテスト用のデータとして利用します.

学習データの分割には, 指定した割合でデータを分割してくれる`sklearn`の`train_test_split`を用います.

~~~ py
import os
import shutil
from sklearn.model_selection import train_test_split

data_dir = 'data/sorted_images'
output_dir = 'data/sorted_images_split'

# 画像ファイルのパスを収集し,年齢別に分類
age_groups = ['10s', '20s', '30s', '40s', '50s', '60s']
for age_group in age_groups:
    images = os.listdir(os.path.join(data_dir, age_group))
    train_images, val_images = train_test_split(images
                                               ,test_size=0.2 #2割をテスト用データにする
                                               , random_state=42)

    train_dir = os.path.join(output_dir, 'train', age_group)
    val_dir = os.path.join(output_dir, 'val', age_group)
    os.makedirs(train_dir, exist_ok=True)
    os.makedirs(val_dir, exist_ok=True)

    for image in train_images:
        shutil.copy(os.path.join(data_dir, age_group, image), os.path.join(train_dir, image))
    for image in val_images:
        shutil.copy(os.path.join(data_dir, age_group, image), os.path.join(val_dir, image))
~~~

以下のような形でデータが保存されていることを確認しましょう.

~~~ sh
data/sorted_images_split
├── train
│      ├── 10s
│      ├── 20s
│      ├── 30s
│      ├── 40s
│      ├── 50s
│      └── 60s
└── val
        ├── 10s
        ├── 20s
        ├── 30s
        ├── 40s
        ├── 50s
        └── 60s
~~~



### 画像認識の実施

CNNを利用した学習を行うにあたって,コード内で扱われる基本的な概念を説明します.

::: note
- **ハイパーパラメータ**
------------------------------------------------------------------
機械学習では,プログラムが自動で学習を進めてくれますが,良い性能を達成するためには人間がいくつかのパラメータを設定する必要があります. また,様々な改善手法があるため,モデルが上手く学習できない場合には,それらを経験によって調整していく必要があります.

本資料では,それらの細かな内容にはあまり踏み込みませんが,以下,基本的な処理やパラメータに関して説明します.


- **前処理(Data Augmentation)**
------------------------------------------------------------------
学習を行うために,画像サイズや色の内容と無関係の情報を減らし,画像を統一のフォーマットに揃えます.

    - **リサイズ (Resize)**

    画像のサイズを揃えます.今回は224 × 224 ピクセルに統一します.

    - **画像反転 (Horizontal Flip)**

    水平方向に画像を反転させることで,データに多様性を加えます.

    - **テンソル(Tensor)変換**

    データの多次元配列を**Tensor**と呼びます. データを,複数の行列によるテンソルに変換することで効率的に学習を行います.
    例えば,画像の場合は,高さ,幅,チャンネル数(RGBカラー等の色)の3次元テンソルとして表現します.

    - **正規化(Normalize)**

    画像データを特定の範囲や分布に変換してモデルが効率的に学習できるようにする手法を正規化といいます. CNNで学習される画像データは,0から255の範囲のピクセル値(色や濃淡の数値)で表現されますが,ばらつきが大きすぎると学習が不安定になるため,0から1の範囲に変換します.

::: warn
なお,正規化に利用されてい平均や標準偏差の値(`[0.485, 0.456, 0.406]`)などは,ConvNeXtの学習に用いられている,大規模画像データベース[ImageNet](https://www.image-net.org)の平均及び標準偏差です.

実際には,**使用するデータの**平均及び標準偏差を用いる必要がありますが今回はあくまで事例の紹介であり利用する画像データ毎に変更する必要があるので,便宜的にこの値を利用しています.

テスト用の実装などでは,すべて`0.5`にするなどもよく行われていますが,本来は変更すべき値であることに注意しましょう.
:::

該当部分(全体のインデントは省略)

~~~py
# データ変換（前処理）
data_transforms = {
    'train': transforms.Compose([
        transforms.Resize((224, 224)),     #画像のリサイズ
        transforms.RandomHorizontalFlip(), #画像をランダムに反転
        transforms.ToTensor(),             #テンソル(多次元配列)に変換
        transforms.Normalize(mean=[0.485, 0.456, 0.406]
                            ,std=[0.229, 0.224, 0.225]), #正規化(本来は値を変更する必要あり.
    ]),
    'val': transforms.Compose([
        transforms.Resize((224, 224)),
        transforms.ToTensor(),
        transforms.Normalize([0.485, 0.456, 0.406]
                            ,[0.229, 0.224, 0.225]),
    ]),
}

~~~

この他にも画像の彩度や光度の調整, ランダム回転,スケーリングなど,様々な前処理手法があり,状況に応じて使い分ける必要があります.



- **バッチ(batch)処理**
------------------------------------------------------------------

CNNでは学習用のデータ全てを一度に学習するとメモリを大量に消費するため,データを分割して学習を行います. そのような分割処理を**バッチ処理**といいます.

分割された一つあたりのデータの数を**バッチサイズ**といいます. バッチサイズが32個の場合は32個の(今回の場合は画像ファイル)を同時に処理します.

また,バッチ処理ではバッチ毎に並列処理を行うため速度面でも,効率的な学習が行えます.

- **エポック(epoch)数**
------------------------------------------------------------------

CNNではモデルの性能を高めるために同じデータセットを何度も繰り返して学習することがあります.その際に,データを1巡して学習する回数を,**エポック数**といいます. 例えば, 10エポックの場合は,160枚の画像を10回学習することになります.

エポック数を増やすと一般的に性能が高まりすが,多すぎる場合には**過学習**が起きるので,エポック数を変更してある程度誤差がが安定する適切なエポック数を見つけることが重要です.

また,本資料では利用していませんが,PyTorchには自動で過学習を防ぐために途中で学習を打ち切る`Early Stopping`用の機能などもあります.

以下のコードでは,エポック数ごとの誤差を記録して,グラフを出力するようになっています.

- **損失関数(Loss Function)**
------------------------------------------------------------------
学習したモデルの性能を調べるために, モデルの予測と実際のラベルとの誤差(損失)を計算するための関数を**損失関数**といいます.
基本的に, 損失関数で求められた誤差が大きいほど,モデルの正確性が劣っていることを示します.

損失関数にはいくつかの種類がありますが,今回は**クロスエントロピー損失(Coross-Entoropy Loss)**を利用します. これは,予測された確率分布と実際のラベルの分布の不一致度を計測しています.

- **オプティマイザ(Optimizer)**
------------------------------------------------------------------
損失関数の値を最小化するために,どのようにモデルのパラメータを更新するかを決定するアルゴリズムを**オプティマイザ**といいます.

基本的には,損失関数によって求められた誤差の勾配(パラメータに対する誤差の微分)を計算してエポック毎に誤差が減る方向にパラメータを調整します.

よく使われるアルゴリズムには,**SGD(Stochastic Gradient Descent)**や,**Adam(Adaptive Moment Estimation)**などがあります.

特定のオプティマイザで上手くいかない場合はパラメータや,アルゴリズムを変更します.

該当部分(全体のインデントは省略)

~~~ py
# 損失関数とオプティマイザ
    criterion = nn.CrossEntropyLoss() #クロスエントロピー損失
    optimizer = optim.Adam(model.parameters() #Adam
                          ,lr=0.0001) #Learning rate (学習率)

    #SGDを利用する場合
    #optimizer = optim.SGD(model.parameters(), lr=0.001, momentum=0.9)
~~~


- **学習率(learning rate)**
------------------------------------------------------------------
機械学習モデルが重み（パラメータ）を更新する際に,その更新幅を決めるハイパーパラメータを**学習率(learning rate)**といいます. モデルの訓練時に,誤差（損失）を最小化するために重みを調整していきますが,学習率はその調整量を決定します.

学習率が大きい場合には1回の更新で重みが大きく変わるため,学習が速く進むことがありますが,最適な解にたどり着く前に振動してしまったり,安定せずに解に収束しないことがあります.

学習率が小さい場合には,更新幅が小さいので,安定して最適解に近づく可能性が高まりますが,学習に時間がかかりすぎてしまい,訓練が遅くなることがあります.

一般的には`0.001`程度から初めて変更していくのが良いとされていますが,以下の事例では調整の結果`0.0001`を採用しています.

このような特性から**SGD**などのオプティマイザでは学習率の設定が非常に重要であり,オプティマイザとは別に学習率を調整するスケジューリングなどの技法が利用されることがあります. 一方で,**Adam**は,ある程度学習率を自動で調整するため,SGDほど,学習率の初期値が結果に影響しないという特徴があります.


- **ランダムシード(Random Seed)**
------------------------------------------------------------------
CNNは,初期値(重みの設定),学習データのシャッフル,オプティマイザなどで乱数(ランダムな値)を利用しているので,実行毎に異なる結果が出てくることが一般的です.

毎回異なる値が生成されるように乱数は,通常CPU時間(プログラムを実行したときのPC内部の時間)などの外部の情報を利用します.

従って, 通常この資料と同じデータを利用して,同じコードを実行しても結果は異なります.
ただし,それでは検証などにおいて不便な場合があります.
また,講義用資料としても不便なので,同じ乱数を利用してできるだけ同じ結果を再現する必要があります.

そこで,以下のコードでは利用する乱数を固定するために,乱数を生成するための情報**ランダムシード**を固定しています.

シード値は適当な数値で構いません. 西暦(`2024`)や,適当な連番(`1234`など),特定のミームの数字(`42`など)が用いられます.

(ただし,実行環境などの違いにより,ランダムシードを固定しても完全に同じ値にはなりません.)

`set_seed()`行をコメントアウトすることで,通常の乱数が利用できるようになるので,研究に利用する場合などには適宜変更してください.

該当部分(全体のインデントは省略)

~~~py
# 乱数シードを設定
def set_seed(seed):
    torch.manual_seed(seed)
    np.random.seed(seed)              # Numpy用の乱数シードを設定
    random.seed(seed)                 # Pythonの標準乱数シードを設定

    # 再現性を完全に保証するために以下も設定（ただし、若干のパフォーマンス低下の可能性あり）
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

def main():
    # シードを設定する
    #(自分の研究でやる場合は以下の行は消しても問題ない.)
    set_seed(42)
~~~

- **CUDA (Compute Unified Device Architecture)**
------------------------------------------------------------------
機械学習では, GPUを用いた計算を行うことが一般的です. 特にPyTorchなどでは,NVIDIAが開発したGPU向けの並列コンピューティングプラットフォームである**CUDA (Compute Unified Device Architecture)**を前提にライブラリが開発されています.
従って, **CUDA**が搭載されたPCでは,**CUDA**を利用することが望ましいです.

しかし,例えば現在のMacOSは**CUDA**に対応しておらず, WindowsPCでもコストなどの観点から異なるGPUが搭載されている場合があります.

M1〜M3などのApple Siliconを搭載したMacでは,PyTorchの実行にあたり**CUDA**の代わりに**MPS (Metal Performance Shaders)**が利用可能です.

PyTorchでは`torch.device()`で利用するデバイスを設定できますが,以下のコードでは, CUDA, MPSが利用できる場合にはそれらを利用し,利用できない場合にはCPUを利用しています.

該当部分(全体のインデントは省略)

~~~ py
if torch.backends.mps.is_available():
    device = torch.device("mps") #Mac GPU
elif torch.cuda.is_available():
    device = torch.device("cuda:0") #Win GPU
else:
    device = torch.device("cpu") #CPU
~~~

:::

これから,先程分割した画像を利用してConvNeXtによる学習を行い, PCAとt-sneで2次元へ次元削減した後,ラベルごとの特徴を可視化してみます.


::: warn
以下のコードを実行すると,PCのスペックによっては10分以上ほぼ全てのCPU/GPUが使用されます.
他の不必要なアプリを閉じて,時間に余裕があるときに電源に繋いだ状態で実行しましょう.
このコードは学生のローカル環境でも動くようになっていますが,上手くいかない場合はGoogle Colaboratory上で試してみましょう.

:::

::: warn

コードを実行して以下のようなWarningが表示される場合,個別の環境によって対処が異なるので教員に
相談してください.


~~~ sh
Found Intel OpenMP ('libiomp') and LLVM OpenMP ('libomp') loaded at
the same time. Both libraries are known to be incompatible and this
can cause random crashes or deadlocks on Linux when loaded in the
same Python program.
Using threadpoolctl may cause crashes or deadlocks. For more
information and possible workarounds, please see
    https://github.com/joblib/threadpoolctl/blob/master/multiple_openmp.md
~~~
:::


コードの全体像は以下のようになります.

~~~ py
import os
import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import seaborn as sns
import random

# 次元削減用
from sklearn.manifold import TSNE
from sklearn.decomposition import PCA

# CNN用
import torch
import torch.nn as nn
import torch.optim as optim
from torchvision import datasets, transforms, models
from torch.utils.data import DataLoader
from torchvision.models import ConvNeXt_Tiny_Weights


#学習した特徴量を抽出する
def extract_features(model, dataloader, device):
    model.eval()
    features = []
    labels_list = []

    with torch.no_grad():
        for inputs, labels in dataloader:
            inputs = inputs.to(device)
            labels = labels.to(device)
            outputs = model(inputs)
            features.append(outputs.cpu())
            labels_list.append(labels.cpu())

    features = torch.cat(features, dim=0)
    labels = torch.cat(labels_list, dim=0)

    return features, labels

# t-SNEによる次元圧縮と散布図の描画
def plot_tsne(train_features, train_labels, path):
    # train_featuresとtrain_labelsをnumpyに変換
    features_np = train_features.numpy()
    labels_np = train_labels.numpy()

    # t-SNEによる次元圧縮 (2次元)
    tsne = TSNE(n_components=2, random_state=42)
    features_2d = tsne.fit_transform(features_np)

    # 散布図の描画
    plt.figure(figsize=(10, 8))

    # 各ラベルに基づいて色分けしてプロット
    num_classes = len(np.unique(labels_np))  # クラス数を取得
    for label in np.unique(labels_np):
        indices = np.where(labels_np == label)
        plt.scatter(features_2d[indices, 0]
                   ,features_2d[indices, 1]
                   ,label=f'{(label + 1) * 10}s'
                   ,alpha=0.6)

    plt.title('t-SNE of Train Features')
    plt.xlabel('t-SNE Component 1')
    plt.ylabel('t-SNE Component 2')
    plt.legend(title="Age Group")
    plt.grid(True)
    plt.savefig(path)
    plt.close()

# PCAによる次元圧縮と散布図の描画
def plot_pca(train_features, train_labels, path):
    # train_featuresとtrain_labelsをnumpyに変換
    features_np = train_features.numpy()
    labels_np = train_labels.numpy()

    # PCAによる次元圧縮 (2次元)
    pca = PCA(n_components=2)
    features_2d = pca.fit_transform(features_np)

    # 散布図の描画
    plt.figure(figsize=(10, 8))

    # 各ラベルに基づいて色分けしてプロット
    num_classes = len(np.unique(labels_np))  # クラス数を取得
    for label in np.unique(labels_np):
        indices = np.where(labels_np == label)
        plt.scatter(features_2d[indices, 0]
                   ,features_2d[indices, 1]
                   ,label=f'{(label + 1) * 10}s'
                   ,alpha=0.6)

    plt.title('PCA of Train Features')
    plt.xlabel('PCA Component 1')
    plt.ylabel('PCA Component 2')
    plt.legend(title="Age Group")
    plt.grid(True)
    plt.savefig(path)
    plt.close()

# 乱数シードを設定
def set_seed(seed):
    torch.manual_seed(seed)
    np.random.seed(seed)              # Numpy用の乱数シードを設定
    random.seed(seed)                 # Pythonの標準乱数シードを設定

    # 再現性を完全に保証するために以下も設定（ただし、若干のパフォーマンス低下の可能性あり）
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

def main():
    # シードを設定する
    #(自分の研究でやる場合は以下の行は消しても問題ない.)
    set_seed(2024)

    # データのディレクトリ設定
    data_dir = 'data/sorted_images_split'
    batch_size = 32
    num_epochs = 5
    num_classes = 6  # 10代, 20代, ..., 60代

    # データ変換（前処理）
    data_transforms = {
        'train': transforms.Compose([
            transforms.Resize((224, 224)),     #画像のリサイズ
            transforms.RandomHorizontalFlip(), #画像をランダムに反転
            transforms.ColorJitter(brightness=0.2, contrast=0.2),  # 色調変化
            transforms.ToTensor(),             #テンソル(多次元配列)に変換
            transforms.Normalize(mean=[0.485, 0.456, 0.406]
                                ,std=[0.229, 0.224, 0.225]), #正規化(本来は値を変更する必要あり.
        ]),
        'val': transforms.Compose([
            transforms.Resize((224, 224)),
            transforms.ToTensor(),
            transforms.Normalize([0.485, 0.456, 0.406]
                                ,[0.229, 0.224, 0.225]),
        ]),
    }

    # データセットの読み込み
    image_datasets = {x: datasets.ImageFolder(os.path.join(data_dir, x),
                                              data_transforms[x])
                      for x in ['train', 'val']}
    dataloaders = {x: DataLoader(image_datasets[x]
                                ,batch_size=batch_size
                                ,shuffle=True
                                ,num_workers=4) #使用するCore数
                   for x in ['train', 'val']}

    # デバイス設定
    #GPUが利用できる場合はGPUを使う,そうでない場合はCPUを計算に利用します.
    if torch.backends.mps.is_available():
        device = torch.device("mps") #Mac GPU
    elif torch.cuda.is_available():
        device = torch.device("cuda:0") #Win GPU
    else:
        device = torch.device("cpu") #CPU

    print(f'Using device: {device}')

    # ConvNextモデルの読み込みとカスタマイズ
    weights = ConvNeXt_Tiny_Weights.IMAGENET1K_V1  # 最新の重みを指定
    model = models.convnext_tiny(weights=weights)  # ConvNextの小さいモデルを使用
    #モデル分類層の最終層(第3層(0,1,2番目))の入力特徴量を取得
    num_ftrs = model.classifier[2].in_features
    #既に学習されたモデルではクラス数がことなるので ,入力特徴量の数(num_ftrs)はそのまま
    #出力をクラス数に変更
    model.classifier[2] = nn.Linear(num_ftrs, num_classes)

    model = model.to(device)

    # 損失関数とオプティマイザ
    criterion = nn.CrossEntropyLoss() #クロスエントロピー損失
    optimizer = optim.Adam(model.parameters() #Adam
                          ,lr=0.0001) #Learning rate (学習率)

    #SGDを利用する場合
    #optimizer = optim.SGD(model.parameters(), lr=0.001, momentum=0.9)

    #結果の記録用
    train_losses = []
    train_accuracies = []
    val_losses = []
    val_accuracies = []
    results = []

    # 学習ループ
    for epoch in range(num_epochs):
        print(f'Epoch {epoch+1}/{num_epochs}')
        print('-' * 10)

        for phase in ['train', 'val']:
            if phase == 'train':
                model.train()
            else:
                model.eval()

            running_loss = 0.0
            running_corrects = 0

            for inputs, labels in dataloaders[phase]:
                inputs = inputs.to(device)
                labels = labels.to(device)

                optimizer.zero_grad()

                with torch.set_grad_enabled(phase == 'train'):
                    outputs = model(inputs)
                    _, preds = torch.max(outputs, 1)
                    loss = criterion(outputs, labels)

                    if phase == 'train':
                        loss.backward()
                        optimizer.step()

                running_loss += loss.item() * inputs.size(0)
                running_corrects += torch.sum(preds == labels.data)
                if phase == 'val':  # バリデーション時に予測と実際のラベルを保存
                    for i in range(len(labels)):
                        results.append({
                            'epoch': epoch + 1,
                            'pred': preds[i].item(),
                            'acctual': labels[i].item()
                        })

            epoch_loss = running_loss / len(image_datasets[phase])
            epoch_acc = running_corrects.float() / len(image_datasets[phase])

            print(f'{phase} Loss: {epoch_loss:.4f} Acc: {epoch_acc:.4f}')
            if phase == 'train':
                train_losses.append(epoch_loss)
                train_accuracies.append(epoch_acc.item())
            else:
                val_losses.append(epoch_loss)
                val_accuracies.append(epoch_acc.item())

    #結果の表示

    print('Training complete')
    print("Training Losses: ", train_losses)
    print("Training Accuracies: ", train_accuracies)
    print("Validation Losses: ", val_losses)
    print("Validation Accuracies: ", val_accuracies)

    # 結果を可視化してCSVファイルに保存
    loss_acc = pd.DataFrame({'train_losses':train_losses
                            ,'train_accuracies':train_accuracies
                            ,'val_losses':val_losses
                            ,'val_accuracies':val_accuracies})
    plt.title('Losses')
    plt.xlabel('Epoch')
    plt.ylabel('Losses')
    plt.plot(np.arange(num_epochs),loss_acc['train_losses'],c='r',label='train_losses')
    plt.plot(np.arange(num_epochs),loss_acc['val_losses'],c='b',label='val_losses')
    plt.grid(True)
    plt.legend()
    plt.savefig('data/result/convnext_loss.png')
    plt.close()

    plt.title('Accuracies')
    plt.xlabel('Epoch')
    plt.ylabel('Accuracies')
    plt.plot(np.arange(num_epochs),loss_acc['train_accuracies'],c='r',label='train_accuracies')
    plt.plot(np.arange(num_epochs),loss_acc['val_accuracies'],c='b',label='val_accuracies')
    plt.grid(True)
    plt.legend()
    plt.savefig('data/result/convnext_acc.png')
    plt.close()

    loss_acc.to_csv('data/result/convnext_loss_acc.csv'
                   ,encoding='utf_8_sig')

    results_df = pd.DataFrame(results)
    result_max_epochs = results_df[results_df['epoch'] == num_epochs]
    result_heatmap = pd.DataFrame(index=np.arange(6)
                                 ,columns=np.arange(6)
                                 ,data=0)

    #実際のラベルに対する予測された回数をカウント
    for i in result_max_epochs.index:
        p = result_max_epochs.at[i,'pred']
        a = result_max_epochs.at[i,'acctual']
        result_heatmap.at[p,a] +=1

    #列相対度数に変換
    for c in result_heatmap:
        result_heatmap[c] = result_heatmap[c] / result_heatmap[c].sum()

    #ヒートマップとして表現
    sns.heatmap(result_heatmap
               ,annot=True)
    plt.ylabel('pred')
    plt.xlabel('acctual')
    plt.savefig('data/result/pred_acctual_heatmap.png')
    plt.close()
    results_df.to_csv('data/result/pred_acctual.csv'
                     ,encoding='utf_8_sig')

    # 特徴量を取得する
    train_features, train_labels = extract_features(model, dataloaders['train'], device)
    val_features, val_labels = extract_features(model, dataloaders['val'], device)

    # (毎回学習するのは大変なので)特徴量を表示または保存しておく
    # 読み込む場合は
    # train_features, train_labels = torch.load('train_features.pth')
    torch.save((train_features, train_labels), 'data/result/convnext_train_features.pth')
    torch.save((val_features, val_labels), 'data/result/convnext_val_features.pth')

    #散布図の描画
    plot_tsne(train_features, train_labels,'data/result/convnext_tsne.png')
    plot_pca(train_features, train_labels,'data/result/convnext_pca.png')

#スクリプトとして実行された場合(python convnext.py)で実行された場合に,
# if __name__ == '__main__': 以下のみが実行される.
# 並列処理(multiprocessing)を行う場合にこのようにしないと,
# 各処理ですべて同じコードが実行されるため,無限ループなどが起きる.
if __name__ == '__main__':
    main()
~~~

出力されている`convnext_loss.png`と`convnext_acc.png`は`epoch`ごとの`loss`と`acc`の推移を表しています. `acc`は,モデルが予測したラベルの実際のラベルに対する正答率であり,`1`であれば予測が完全にラベルと一致していることを示しています. 今回は10代から60代までの6ラベルなので,完全にランダムにラベルを予測しても`0.16`程度はラベルと予測が一致します.

![lossの推移](/images/convnext_loss_epoch20.png)

![accの推移](/images/convnext_acc_epoch20.png)

グラフを確認してみると`epoch`が`5`をピークとして`loss`も`acc`も低下していることがわかります. そこで, もう一度,`num_epochs`を`5`に変更して,学習してみましょう. `random_seed`が固定されているので,基本的には同じ値が出力されるはずです.

![accの推移(epoch 5)](/images/convnext_acc_epoch5.png)

最終的に今回は, テストデータでの正答率が,`0.4`程度になりました. それほど高い値ではありませんが,ランダムに選択するよりはかなり良い値になったので,今回はこのくらいで良しとします. 実際の研究などでは,データ数を増やす,ハイパーパラメータやアルゴリズムを変更するなどして,もう少し良い値を目指したほうが良いでしょう.

出力されている`pred_acctual_heatmap.png`は, テストデータにおける実際のラベルに対する予測値を予測値のラベル毎にカウントしたものを相対度数として表現したヒートマップです.すべて正確に予測されていた場合,度数は対角線上に集中します.
このように可視化することで,モデルが何をどのように予測しているのかを確認できます.

![accの推移(epoch 5)](/images/pred_acctual_heatmap.png)

ヒートマップを確認すると概ね対角線上に度数が集中していることがわかります. 特に10,20代(y軸の0,1)を50,60代と予測した数は0であり,年齢が離れるほど正確に識別されていることがわかります.

一方で,実際のラベルが10,20,40代であるときに,30代であると誤って予測する確率が高く,30代以前はあまり上手く識別できないことがわかります.

続いて,`PCA`と`t-sne`の結果を確認してみましょう.

![PCA](/images/convnext_pca.png)

![t-sne](/images/convnext_tsne.png)

いずれも左から右に行くにつれて,年齢が高くなっており,ある程度識別できていることがわかります.一方で,30代の緑色が広い範囲に分布しているために識別が困難であること,50代と60代が左右とは別の特徴量で識別されていることなどがわかります.

このように,学習されたモデルの特徴量を分析することで,それぞれのクラスの特徴がある程度見えてきます.

それぞれの横軸,縦軸の特徴量が実際には何であるかは,各学習層でどのような特徴を抽出しているかを**特徴マップ**などによって可視化することが可能ですが,今回は扱いません. 興味がある方は,教員に聞いてみましょう.

yakagika