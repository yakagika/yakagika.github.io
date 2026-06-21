---
title: 関数型プログラミング Ch9 代数的データ型3
description: 資料
tags:
    - algebra
    - lecture
    - statistics
    - haskell
featured: true
katex: true
date: 2025-06-06
tableOfContents: true
previousChapter: fp8.html
nextChapter: fp10.html
open: true
---

[第8章](fp8.html)では, 型クラスと代数構造の対応を見ました. 型 (= 集合) の上に演算と法則を載せると半群・モノイド・群といった代数になり, それを `Semigroup` / `Monoid` などの型クラスで表せる, という流れでした. その最後で「リスト・ツリーも多相データ型なので, 次章で関手と合わせて扱う」と予告しました. 本章はその回収です.

本章は 2 部構成です. 前半 **多相データ型** では, 型引数 `a` を持つデータ型を整理します. リスト・ツリーに加え, 実用的な辞書型 `Map`, 失敗を型で表す `Maybe`, 二者択一を表す `Either` を扱います. これらは [第7章](fp7.html)で「後の章で扱う」と先送りした型でもあります.

後半 **圏論的解釈** では, これらの型を **圏論** の言葉で読み直します. [第7章](fp7.html)で型を集合とみなし, [第8章](fp8.html)で集合に演算を載せて代数とみなしたのに続き, 本章では「型を **対象**, 関数を **射** とみなす」見方を導入します. このとき, 型引数を持つ多相データ型は **関手 (functor)**, 型に依らない一様な多相関数は **自然変換 (natural transformation)** という構造に対応します. すなわち, 次の対応を順に確かめていきます.

| 圏論の概念 | Haskell での対応 |
| --- | --- |
| 対象 (object) | 型 |
| 射 (morphism) | 関数 `a -> b` |
| 関手 (functor) | 型引数を持つ多相データ型 (`fmap` を備える) |
| 自然変換 (natural transformation) | 型に依らない多相関数 `forall a. f a -> g a` |

::: note
[第7章](fp7.html)冒頭の警告で「Haskell の高度な機能は集合論的な理解よりも圏論的な理解のほうが適している. 一旦集合論的に概要を把握し, 後の章で圏論的な解釈を試みる」と述べました. 本章の後半がその「後の章」にあたります. ただし圏論そのものを体系的に扱うのではなく, これまで書いてきたコードを別の角度から眺め直す程度に留めます.
:::

# 多相データ型

## 型引数を持つデータ型

これまで定義してきたデータ型は, `MyDogs` や `Color` のように中身の型が固定されていました. これに対し, **中身の型を後から決められる** データ型を作れます. 型の定義に **型変数** (型引数) を持たせるのです.

~~~ haskell
data Box a  = Box a          -- 任意の型 a を 1 つ包む箱
data Pair a b = Pair a b     -- 型 a と型 b を 1 つずつ持つ組
~~~

`data Box a = Box a` の左辺 `Box a` の `a` が **型引数** です. これは「`a` をどの型にするかは, 使うときに決める」ことを表します. `Box Int` なら `Int` を包む箱, `Box String` なら文字列を包む箱になります. このように型引数を持つデータ型を **多相データ型 (polymorphic data type)** といいます.

ここで, 左辺の `Box` と右辺の `Box` は役割が違う点に注意してください.

- 左辺の `Box` は **型構築子 (type constructor)** です. それ自体は型ではなく, 型 `a` を 1 つ受け取って `Box a` という型を作ります. これを **種 (kind)** という記法で `Box :: * -> *` と書きます (`*` が「具体的な型」を表し, 「型を 1 つ受け取って型を返す」ことを `* -> *` と表現します).
- 右辺の `Box` は **データ構築子 (data constructor)** です. こちらは値を 1 つ受け取って `Box a` 型の値を作る関数で, 型は `Box :: a -> Box a` です.

`Pair a b` は型引数を 2 つ取るので, 型構築子の種は `Pair :: * -> * -> *` です. このように, 型引数の個数が種の矢印の本数に対応します.

実は, すでに使ってきた **リスト `[a]`** も多相データ型です. `[]` が型構築子 (`[] :: * -> *`), 要素の型 `a` を受け取って `[a]` 型を作ります. `[Int]`, `[Char]`, `[Bool]` がすべて同じ `[]` から作られるのは, リストが「中身の型を後から決められる」多相データ型だからです.

以下では, 型引数 `a` を持つ多相データ型を 5 つ見ていきます. まず **リスト `[a]` / ツリー `Tree a`** ([第8章](fp8.html)で学んだ **モノイド** の実例) と, 実用的な辞書型 **`Map`** を扱います. これらは「型引数を持つ多相データ型」であると同時に, モノイドや関手といった構造も備えます (関手は後半 [圏論的解釈](#圏論的解釈) の **Functor** の節で扱います). その後, 失敗を表す **`Maybe`**, 二者択一を表す **`Either`** に進みます.

## リスト

[第7章](fp7.html)では「リストが代数的にどのように定義されるかは後の章で扱う」と予告しました ([第7章](fp7.html)「集合の内包表記と代数的データ型」の警告). その回収です.

リストは, 連結演算 `(++)` と空リスト `[]` を単位元として, **モノイド** になります. 数式で書けば, 連結 $xs \mathbin{+\!\!\!+} ys$ が演算, $[]$ が単位元で, 結合律 $(xs \mathbin{+\!\!\!+} ys) \mathbin{+\!\!\!+} zs = xs \mathbin{+\!\!\!+} (ys \mathbin{+\!\!\!+} zs)$ と単位元律 $[] \mathbin{+\!\!\!+} xs = xs \mathbin{+\!\!\!+} [] = xs$ を満たします. リストでは新しく演算子を定義する必要はなく, **既存の `(++)` がその役割を果たし**, `(<>) = (++)`, `mempty = []` となっています.

~~~ haskell
main :: IO ()
main = do
  print (([1,2] <> [3,4]) :: [Int])          -- [1,2,3,4]    (<> は ++ と同じ)
  print (([] <> [1,2,3]) :: [Int])           -- [1,2,3]      (左単位元)
  print (([1,2,3] <> []) :: [Int])           -- [1,2,3]      (右単位元)
  print (mconcat [[1],[2],[3]] :: [Int])     -- [1,2,3]      (畳み込み)
~~~

リストの `<>` は `(++)`, `mempty` は `[]` です. `mconcat` は `concat` と同じはたらきをします.

リストは, モノイドの中でも特別な位置を占めます. 要素の型 `a` を決めると, `[a]` は **`a` の値から作れる「最も自由な」モノイド** になります. これを **自由モノイド (free monoid)** といいます. 「自由」とは, 結合律と単位元律以外に余計な等式が成り立たない, という意味です. たとえば `[1,2]` と `[2,1]` は (集合ではないので) 別物のままで, 順序や重複が潰れません. [第7章](fp7.html)で「リストには順序があり重複も許され, 集合とは別物」と注意したことが, ここでは「自由モノイド = 余計な等式を課さないモノイド」として代数的に説明できます.

## ツリー

次に, 自分で定義した木構造に代数構造を載せてみます. ここでは二分探索木 (binary search tree) を考え, **「要素の集まりを表す木」** としてモノイドを与えます.

~~~ haskell
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- 二分探索木への挿入 (すでにあれば何もしない)
insert' :: Ord a => a -> Tree a -> Tree a
insert' x Leaf = Node Leaf x Leaf
insert' x t@(Node l y r)
  | x < y     = Node (insert' x l) y r
  | x > y     = Node l y (insert' x r)
  | otherwise = t

-- 中間順走査で要素を昇順に取り出す
toList' :: Tree a -> [a]
toList' Leaf         = []
toList' (Node l x r) = toList' l ++ [x] ++ toList' r

fromList' :: Ord a => [a] -> Tree a
fromList' = foldr insert' Leaf

-- 合併演算 ⊔:  t1 ⊔ t2 = 「t1 の全要素を t2 に挿入した木」
(|+|) :: Ord a => Tree a -> Tree a -> Tree a
t1 |+| t2 = foldr insert' t2 (toList' t1)

-- <> は (|+|), 単位元は空の木
instance Ord a => Semigroup (Tree a) where (<>)   = (|+|)
instance Ord a => Monoid    (Tree a) where mempty = Leaf

main :: IO ()
main = do
  let t = fromList' [5,3,8] <> fromList' [3,9,1]
  print (toList' t)   -- [1,3,5,8,9]
~~~

ここでは木を「要素の集合の入れ物」とみなし, `<>` を「一方の全要素をもう一方に挿入する合併」として定義しています. 単位元は空の木 `Leaf` です. `instance Ord a => Semigroup (Tree a)` のように, インスタンス宣言にも `Ord a =>` という **制約を付けられる** 点に注目してください (挿入のために要素の比較 `Ord` が必要なため).

::: warn
この `<>` が結合律を満たすのは, あくまで **「木が表す要素の集合」のレベル** です. `t1 <> t2` と `t2 <> t1`, あるいは括弧の付け方を変えたものは, 取り出される要素の集合 (`toList'` の結果) は等しくなりますが, **木の内部構造 (枝分かれの形) は異なりうる** 点に注意してください. これは[第7章](fp7.html)で見た「リストと集合は別物」という注意の, 木における対応物です. 法則を「どの同値性のもとで成り立つと見るか」を意識することが大切です.
:::

## Map — キーと値の対応

最後に, 標準ライブラリ `containers` の **`Map`** を扱います. `Map k v` は **キー `k` から値 `v` を引ける辞書 (連想配列)** を表す多相データ型で, 型引数を **2 つ** 取ります (`Map :: * -> * -> *`). リストや木と同じ多相型ですが, 自作せずライブラリの型をそのまま使う点が違います.

~~~ haskell
import qualified Data.Map as Map
import Data.Map (Map)

-- キー (果物名) から 値 (個数) への対応
stock :: Map String Int
stock = Map.fromList [("apple", 3), ("banana", 2)]
~~~

キーで値を引くには `Map.lookup` を使います. キーが無いこともあるので, 結果は **`Maybe v`** で返ります (この `Maybe` は次節で詳しく扱います).

~~~ haskell
main :: IO ()
main = do
  print (Map.lookup "apple" stock)   -- Just 3
  print (Map.lookup "grape" stock)   -- Nothing  (キーが無い)
~~~

**`Map` はモノイド** です. ただし `(<>)` は **左偏 (left-biased) の和** で, 同じキーがあれば **左の値を優先** し, 値そのものは結合しません. 単位元 `mempty` は空の `Map` です.

~~~ haskell
main :: IO ()
main = do
  let a = Map.fromList [("a", 1), ("b", 2)]
      c = Map.fromList [("a", 9), ("d", 4)]
  print ((a <> c) :: Map String Int)
    -- fromList [("a",1),("b",2),("d",4)]   (キー "a" は左の 1 が残る)
~~~

同じキーの値を **結合** したいときは, [第8章](fp8.html)「代数のインスタンスにする利点」で予告したとおり, **値が `Semigroup` であれば** `Map.unionWith (<>)` を使います. 衝突したキーの値を `(<>)` でまとめられます. 典型例が **単語の出現回数の集計** です.

~~~ haskell
import Data.Monoid (Sum (..))

-- 2 つの集計を, 同じキーの値を <> (= 加算) でまとめる
merged :: Map String (Sum Int)
merged = Map.unionWith (<>)
           (Map.fromList [("a", Sum 1), ("b", Sum 1)])
           (Map.fromList [("a", Sum 1), ("c", Sum 1)])
  -- fromList [("a",Sum 2),("b",Sum 1),("c",Sum 1)]

-- 単語リストから出現回数を数える
wordCount :: [String] -> Map String (Sum Int)
wordCount ws = Map.fromListWith (<>) [(w, Sum 1) | w <- ws]
  -- wordCount ["a","b","a"] = fromList [("a",Sum 2),("b",Sum 1)]
~~~

**`Map` は関手 (Functor) でもあります.** ただし `fmap` が作用するのは **値だけ** で, キーは変わりません (`Map k` が関手で, 動かせるのは値 `v` の側です).

~~~ haskell
main :: IO ()
main = do
  print (fmap (* 10) stock)
    -- fromList [("apple",30),("banana",20)]
~~~

このように `Map` は, **多相型であり, モノイドであり, 関手でもある**, 実務で頻出の型です. 関手としての側面は後半の **Functor** の節で他の型と合わせて整理します. なお `lookup` が `Maybe` を返すことは, 次節で `Maybe` を学ぶ動機にもなります.

## Maybe — 失敗を型で表す

[第4章](fp4.html)・[第7章](fp7.html)で何度か「`Maybe` は後の章で扱う」と先送りしてきました. ここで回収します.

[第7章](fp7.html)では, `head` や直和型のレコードセレクタが **部分関数** になる問題を見ました. 空リストに `head` を適用すると `error "Empty List"` で停止し ([第7章](fp7.html)「集合と列挙型」), `Square` の値に `radius` を適用すると `No match in record selector` で停止しました (Exercise CH7-7). どちらも「答えを返せない入力」があるのに, 型の上では `[a] -> a` のように「必ず答えを返す」かのように見えてしまうのが原因です.

`Maybe` は, この「答えを返せないかもしれない」ことを **型で表す** ための多相データ型です. 定義は次の通りで, [第7章](fp7.html)の **直和型** に型引数 `a` を付けたものです.

~~~ haskell
data Maybe a = Nothing | Just a
~~~

- `Nothing` は「値が無い (失敗した)」ことを表すコンストラクタです. 引数を取りません.
- `Just a` は「値 `a` がある (成功した)」ことを表すコンストラクタで, 中身 `a` を 1 つ包みます.

型 `Maybe a` は「`a` の値が 1 つあるか, 何も無いかのいずれか」を表します. たとえば `Maybe Int` の値は `Just 3` や `Just (-1)` や `Nothing` です. 失敗しうる計算の結果を `Maybe` で包むと, **失敗の可能性が型に現れる** ため, 呼び出す側はパターンマッチで両方の場合に対処せざるをえなくなります.

集合論的には, `Just` でくるんだ `a` の値全体に, 値を持たない `Nothing` を 1 つ足したものなので,

$$\mathrm{Maybe}\ a = \{\mathrm{Nothing}\} \cup \{\mathrm{Just}\ x \mid x \in a\}$$

となります. これは $a$ の要素数に 1 を足した集合, すなわち [第7章](fp7.html)の直和の記法で $a + 1$ (要素 1 つの集合 `{Nothing}` との直和) です.

`error` で停止していた `head` を, `Maybe` を返す **全域関数** に書き直してみます. 同様に, 失敗しうる割り算も `Maybe` で安全にできます.

~~~ haskell
-- 空リストでも停止しない安全な head
safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

-- 0 で割るときは Nothing を返す安全な割り算
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

main :: IO ()
main = do
  print (safeHead [1, 2, 3 :: Int])  -- Just 1
  print (safeHead ([] :: [Int]))     -- Nothing
  print (safeDiv 10 2)               -- Just 5
  print (safeDiv 10 0)               -- Nothing
~~~

`safeHead []` が `Nothing` を返すので, 空リストでも停止しません. 型 `[a] -> Maybe a` を見るだけで「失敗しうる」ことが分かり, 呼び出し側は `Just` と `Nothing` の両方を処理する必要があります. 結果を使う側はパターンマッチで取り出します.

~~~ haskell
describeMaybe :: Maybe Int -> String
describeMaybe Nothing  = "値なし"
describeMaybe (Just n) = "値は " ++ show n

main :: IO ()
main = do
  putStrLn (describeMaybe (safeDiv 10 2))  -- 値は 5
  putStrLn (describeMaybe (safeDiv 10 0))  -- 値なし
~~~

`Nothing` のケースを書き忘れると GHC が網羅性の警告を出すため, 「失敗の処理を忘れる」ミスをコンパイル時に気づけます. これが, 部分関数を `Maybe` で書き換える最大の利点です.

::: warn
`Maybe` は「失敗するかもしれない 1 つの値」を表しますが, **なぜ失敗したか** (理由) は持てません. `Nothing` はただの「値なし」です. 失敗の理由を伴わせたい場合は, 次節の `Either` を使います.
:::

::: note

### Exercise CH9-1

**安全な探索関数 `safeLast` / `lookupKey` (Maybe)**

1. リストの **末尾** の要素を返す安全な関数 `safeLast :: [a] -> Maybe a` を実装してください. 空リストには `Nothing`, それ以外は最後の要素を `Just` で返します.

2. キーと値のペアのリストから, 指定したキーに対応する値を探す関数 `lookupKey :: Eq k => k -> [(k, v)] -> Maybe v` を実装してください. 見つかれば `Just 値`, 無ければ `Nothing` を返します (標準の `lookup` と同じ動作を自分で書きます).

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (safeLast [1, 2, 3 :: Int])   -- Just 3
  print (safeLast ([] :: [Int]))      -- Nothing
  print (lookupKey "b" [("a", 1), ("b", 2)])  -- Just 2
  print (lookupKey "z" [("a", 1), ("b", 2)])  -- Nothing
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
safeLast :: [a] -> Maybe a
safeLast []       = Nothing
safeLast [x]      = Just x
safeLast (_ : xs) = safeLast xs

lookupKey :: Eq k => k -> [(k, v)] -> Maybe v
lookupKey _ [] = Nothing
lookupKey key ((k, v) : rest)
  | key == k  = Just v
  | otherwise = lookupKey key rest

main :: IO ()
main = do
  print (safeLast [1, 2, 3 :: Int])           -- Just 3
  print (safeLast ([] :: [Int]))              -- Nothing
  print (lookupKey "b" [("a", 1), ("b", 2)])  -- Just 2
  print (lookupKey "z" [("a", 1), ("b", 2)])  -- Nothing
~~~

どちらも「失敗しうる」ことを返り値の `Maybe` で表しているため, 空リストやキー不在でも実行時エラーになりません. `safeLast` は要素 1 つの場合 `[x]` を基底として末尾まで再帰し, `lookupKey` はキーが一致したところで `Just` を返します.

</details>

:::

## Either — 二者択一とエラー表現

`Maybe` の `Nothing` は「失敗した」ことしか伝えられませんでした. 失敗の **理由** も一緒に運びたいときに使うのが `Either` です. `Either` は「2 つの型のどちらか一方の値を持つ」多相データ型で, 型引数を 2 つ取ります.

~~~ haskell
data Either a b = Left a | Right b
~~~

- `Left a` は型 `a` の値を包むコンストラクタです.
- `Right b` は型 `b` の値を包むコンストラクタです.

`Either a b` の値は `Left <a の値>` か `Right <b の値>` のいずれかです. これは [第7章](fp7.html)の直和型そのもので, 集合論的には $a$ と $b$ の **タグ付き直和**

$$\mathrm{Either}\ a\ b = a + b$$

になります (`Left` / `Right` というタグで両者を区別するので, 共通要素があっても素な和になります).

エラー処理では, **`Left` を失敗 (理由つき) / `Right` を成功** に使うのが Haskell の慣習です. 「正しい (right)」と「右 (right)」を掛けた語呂で, 成功を `Right` に割り当てると覚えるとよいでしょう. 失敗の理由を文字列で持たせるなら, 型は `Either String b` のようになります.

前節の `safeDiv` を, 失敗の理由を伝える形に書き直します.

~~~ haskell
-- 失敗の理由を文字列で返す安全な割り算
safeDivE :: Int -> Int -> Either String Int
safeDivE _ 0 = Left "0 では割れません"
safeDivE x y = Right (x `div` y)

main :: IO ()
main = do
  print (safeDivE 10 2)  -- Right 5
  print (safeDivE 10 0)  -- Left "0 では割れません"
~~~

`safeDiv` が `Nothing` を返していた箇所で, `safeDivE` は `Left "0 では割れません"` と **理由つきの失敗** を返します. 受け取る側はやはりパターンマッチで両方を処理します.

~~~ haskell
report :: Either String Int -> String
report (Left err) = "エラー: " ++ err
report (Right n)  = "結果: " ++ show n

main :: IO ()
main = do
  putStrLn (report (safeDivE 10 2))  -- 結果: 5
  putStrLn (report (safeDivE 10 0))  -- エラー: 0 では割れません
~~~

`Maybe` と `Either` の使い分けは「失敗の理由が要るか」です. 理由が不要なら `Maybe`, 理由を運びたいなら `Either String`(や独自のエラー型) を使います.

::: note

### Exercise CH9-2

**理由つきの検証 `checkAge` (Either)**

年齢を表す `Int` を受け取り, 妥当なら `Right 年齢`, 不正なら `Left 理由` を返す関数 `checkAge :: Int -> Either String Int` を実装してください. 0 未満なら `"年齢が負です"`, 150 より大きいなら `"年齢が大きすぎます"`, それ以外は `Right` でその年齢を返します.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (checkAge 30)    -- Right 30
  print (checkAge (-1))  -- Left "年齢が負です"
  print (checkAge 200)   -- Left "年齢が大きすぎます"
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
checkAge :: Int -> Either String Int
checkAge n
  | n < 0     = Left "年齢が負です"
  | n > 150   = Left "年齢が大きすぎます"
  | otherwise = Right n

main :: IO ()
main = do
  print (checkAge 30)    -- Right 30
  print (checkAge (-1))  -- Left "年齢が負です"
  print (checkAge 200)   -- Left "年齢が大きすぎます"
~~~

`Maybe` では「不正だった」ことしか伝えられませんが, `Either String` なら **どう不正か** を `Left` の文字列で伝えられます. ガード式で条件ごとに別の理由を返すのが定石です.

</details>

:::

## Maybe / Either の基本操作

`Maybe` と `Either` はよく使われるため, 標準ライブラリにパターンマッチを定型化した関数が用意されています. 毎回 `case` やパターンマッチを書かずに済みます.

`maybe` は「`Nothing` のときの既定値」と「`Just x` のときに `x` に適用する関数」を渡すと, `Maybe` を 1 つの値に畳み込みます.

~~~ haskell
maybe :: b -> (a -> b) -> Maybe a -> b
~~~

`fromMaybe` は `Maybe` から値を取り出し, `Nothing` のときは既定値を返します (`Data.Maybe` にあります).

~~~ haskell
fromMaybe :: a -> Maybe a -> a
~~~

`either` は `Either` 版で, `Left` のときと `Right` のときの 2 つの関数を渡します.

~~~ haskell
either :: (a -> c) -> (b -> c) -> Either a b -> c
~~~

これらを使うと, 前節までのパターンマッチを短く書けます.

~~~ haskell
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  -- maybe 既定値 関数 Maybe値
  print (maybe 0 (+ 100) (Just 5))         -- 105
  print (maybe 0 (+ 100) Nothing)          -- 0
  -- fromMaybe 既定値 Maybe値
  print (fromMaybe (-1) (safeDiv 10 2))    -- 5
  print (fromMaybe (-1) (safeDiv 10 0))    -- -1
  -- either 左用関数 右用関数 Either値
  putStrLn (either ("エラー: " ++) (\n -> "結果: " ++ show n) (safeDivE 10 2))  -- 結果: 5
  putStrLn (either ("エラー: " ++) (\n -> "結果: " ++ show n) (safeDivE 10 0))  -- エラー: 0 では割れません
~~~

`Data.Maybe` には, `Maybe` の集まりを扱う関数もあります. `isJust` / `isNothing` は中身の有無を判定し, `catMaybes` は `Maybe` のリストから `Just` の中身だけを集め, `mapMaybe` は「各要素に `Maybe` を返す関数を適用し, `Just` のものだけ残す」操作です.

~~~ haskell
import Data.Maybe (isJust, catMaybes, mapMaybe)

-- 文字列を Int に変換しようとし, 数字でなければ Nothing
parseInt :: String -> Maybe Int
parseInt s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing

main :: IO ()
main = do
  print (isJust (Just 3))                          -- True
  print (isJust (Nothing :: Maybe Int))            -- False
  print (catMaybes [Just 1, Nothing, Just 3])      -- [1,3]
  print (mapMaybe parseInt ["1", "x", "3", "y"])   -- [1,3]
~~~

`catMaybes` は失敗 (`Nothing`) を捨てて成功だけを集め, `mapMaybe` は「変換と絞り込みを同時に行う」関数です. 「数字に変換できる文字列だけを集めて整数にする」といった処理を 1 行で書けます.

::: note

### Exercise CH9-3

**`Maybe` を返す安全な計算の連結 (Either への変換)**

`safeDiv :: Int -> Int -> Maybe Int` (本文の定義) の結果を, 理由つきの `Either String Int` に変換する関数 `toEither :: String -> Maybe a -> Either String a` を実装してください. `Nothing` のときは渡した理由を `Left` に, `Just x` のときは `Right x` にします. これを使い, `safeDiv` の `Nothing` に理由を付ける例を完成させてください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (toEither "0 では割れません" (safeDiv 10 2))  -- Right 5
  print (toEither "0 では割れません" (safeDiv 10 0))  -- Left "0 では割れません"
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

toEither :: String -> Maybe a -> Either String a
toEither reason Nothing  = Left reason
toEither _      (Just x) = Right x

main :: IO ()
main = do
  print (toEither "0 では割れません" (safeDiv 10 2))  -- Right 5
  print (toEither "0 では割れません" (safeDiv 10 0))  -- Left "0 では割れません"
~~~

`toEither` は「理由を持たない失敗 (`Maybe`)」を「理由つきの失敗 (`Either`)」へ橋渡しします. `Maybe` で十分な内側の計算と, 理由を伝えたい外側の境界とで型を使い分けられるのが利点です. `toEither` 自体も中身 `a` には触れない自然変換 (`Maybe` から `Either String` への変換) になっています.

</details>

:::

# 圏論的解釈

ここからは, 本章前半で見た多相データ型を **圏論** の言葉で読み直します. [第7章](fp7.html)で型を集合, [第8章](fp8.html)で集合に演算を載せた代数とみなしたのに続く, 第 3 の見方です. これまで書いてきたコードが「圏」「関手」「自然変換」という構造をなしていることを確かめます.

## データ型は対象, 関数は射

**圏 (category)** とは, おおまかには次の 4 つの組です.

1. **対象 (object)** の集まり.
2. 対象から対象への **射 (morphism)** の集まり. 対象 $A$ から $B$ への射を $f : A \to B$ と書きます.
3. 射の **合成 (composition)**: $f : A \to B$ と $g : B \to C$ から, 繋いだ射 $g \circ f : A \to C$ を作れること.
4. 各対象 $A$ の **恒等射 (identity)** $\mathrm{id}_A : A \to A$.

そして, 次の **圏の法則** を満たすことが要求されます.

- **結合律**: $\forall f, g, h.\ \ (f \circ g) \circ h = f \circ (g \circ h)$
- **恒等律**: $\forall f.\ \ \mathrm{id} \circ f = f \circ \mathrm{id} = f$

Haskell の型と関数は, この条件をそのまま満たします. 対象を **型**, 射を **関数 `a -> b`** とみなすと, 合成は関数合成 `(.)`, 恒等射は恒等関数 `id` です. この圏を慣習的に **Hask** と呼びます.

~~~ haskell
-- 射 = 関数, 合成 = (.), 恒等射 = id
f :: Int -> Int
f = (+ 1)

g :: Int -> Int
g = (* 2)

main :: IO ()
main = do
  print ((g . f) 3)   -- 8   (g (f 3) = g 4 = 8)
  print (id 3)        -- 3   (恒等射)
  print ((f . id) 3)  -- 4   (id . f = f . id = f)
~~~

`(.)` と `id` は [第6章](fp6.html) で関数合成として導入しましたが, 圏論的にはこれが「射の合成」と「恒等射」にあたります. 圏の法則も成り立ちます. `(f . g) . h` と `f . (g . h)` はどちらも `\x -> f (g (h x))` で等しく (結合律), `id . f` と `f . id` はどちらも `f` です (恒等律).

ここで, [第8章](fp8.html)の **モノイド則** と見比べてください. モノイドは「結合律 + 単位元律」を満たす演算 `(<>)` と単位元 `mempty` の組でした. 圏もまた「結合律 + 恒等律」を満たす合成 `(.)` と恒等射 `id` の組です.

$$
\underbrace{(f \circ g) \circ h = f \circ (g \circ h)}_{\text{圏の結合律}}, \quad
\underbrace{\mathrm{id} \circ f = f \circ \mathrm{id} = f}_{\text{圏の恒等律}}
$$

実際, 圏は **モノイドを一般化した構造** です. [第8章](fp8.html)のモノイドは, **対象が 1 つだけの圏** (射 = その型の要素, 合成 = `<>`, 恒等射 = `mempty`) とみなせます. モノイドが「型を 1 つ固定して演算を載せた」構造だったのに対し, 圏は「型と関数のネットワーク全体」を, 合成という演算を持つ構造とみる, より大きな見方です.

## Functor (関手) と fmap

**関手 (functor)** とは, **圏の構造を保つ写像** です. 圏 $\mathcal{C}$ の各対象 $A$ を別の対象 $F\,A$ に, 各射 $f : A \to B$ を射 $F\,f : F\,A \to F\,B$ に対応させ, しかも次のように **合成と恒等射を保つ** ものを関手といいます.

- **恒等射を保つ**: $F\,\mathrm{id} = \mathrm{id}$
- **合成を保つ**: $F\,(g \circ f) = F\,g \circ F\,f$

Haskell では, 型引数を持つ多相データ型 (種が `* -> *` のもの) が関手の候補です. 型構築子 `f` が「対象 (型) `a` を `f a` に移す」役割を果たし, 「射 (関数) を移す」役割を担うのが `fmap` です. これを表す型クラスが `Functor` です.

~~~ haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
~~~

`fmap` は「`a` から `b` への関数」を「`f a` から `f b` への関数」に持ち上げます. これがまさに「射 $f : a \to b$ を $F\,f : f\,a \to f\,b$ に対応させる」操作です. 関手が満たすべき **関手則** は, 圏の恒等射と合成を保つことを Haskell の言葉に直したものです.

$$\mathrm{fmap}\ \mathrm{id} = \mathrm{id}, \qquad \mathrm{fmap}\ (g \circ h) = \mathrm{fmap}\ g \circ \mathrm{fmap}\ h$$

最も身近な関手が `Maybe` です. `fmap` は「`Just` の中身に関数を適用し, `Nothing` はそのまま返す」操作になります.

~~~ haskell
main :: IO ()
main = do
  print (fmap (+ 1) (Just 3))            -- Just 4
  print (fmap (+ 1) (Nothing :: Maybe Int))  -- Nothing
~~~

`fmap (+1) (Just 3)` が `Just 4` になり, `Nothing` には何も起きません. 「箱の中身に関数を適用する」イメージで, 箱が空 (`Nothing`) なら何もしないわけです.

**リストも関手** です. リストに対する `fmap` は, すでに [第4章](fp4.html) で学んだ `map` そのものです.

~~~ haskell
main :: IO ()
main = do
  print (fmap (* 2) [1, 2, 3])  -- [2,4,6]   (fmap = map)
  print (map  (* 2) [1, 2, 3])  -- [2,4,6]
~~~

**`Either a` も関手** です. ただし `Either` は型引数を 2 つ取る (`Either :: * -> * -> *`) ため, 関手にするには片方を固定して `Either a` (種が `* -> *`) の形にします. `fmap` は `Right` の中身にだけ作用し, `Left` (慣習上は失敗) はそのまま素通しします.

~~~ haskell
main :: IO ()
main = do
  print (fmap (+ 1) (Right 3 :: Either String Int))  -- Right 4
  print (fmap (+ 1) (Left "err" :: Either String Int))  -- Left "err"
~~~

エラー処理の文脈では, この性質が便利です. `fmap` で成功値 (`Right`) だけを次々と加工しつつ, 失敗 (`Left`) が出たらそれ以降の加工を素通しできます.

最後に, 本章前半で扱った **`Tree a` を Functor にしてみます**. これが「型引数を持つ多相データ型 = 関手」の実例で, 前半の予告 ([型引数を持つデータ型](#型引数を持つデータ型)) の回収です. `fmap` は「木の構造はそのままに, 各ノードの値だけを関数で変換する」操作として定義します.

~~~ haskell
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- 木の形は保ったまま, 各ノードの値に関数を適用する
instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- 中間順走査で値を取り出す (確認用)
toList' :: Tree a -> [a]
toList' Leaf         = []
toList' (Node l x r) = toList' l ++ [x] ++ toList' r

sample :: Tree Int
sample = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

main :: IO ()
main = do
  print (toList' sample)             -- [1,2,3]
  print (toList' (fmap (* 10) sample))  -- [10,20,30]
~~~

`fmap (*10) sample` は, 木の枝分かれの形 (どこが `Node` でどこが `Leaf` か) を一切変えずに, 各ノードの値だけを 10 倍します. ここで定義の `fmap _ Leaf = Leaf` が「`Leaf` の構造を保つ」, `Node (fmap f l) (f x) (fmap f r)` が「`Node` の構造を保ちつつ値だけ変換し, 部分木にも再帰的に `fmap` する」ことを表しています. リストや木のように, **要素を一様に持つコンテナはたいてい関手になる** わけです.

::: warn
`Functor` クラスも, `Semigroup` などと同様に **関手則をコンパイラは検査しません**. 関手則を破る `fmap` (たとえば木の形を変えてしまうもの) を書いてもコンパイルは通ります. 法則を守るのはプログラマの責任です ([第8章](fp8.html)のコラムで触れた QuickCheck で, `fmap id == id` などを性質として確認できます).
:::

::: note
`fmap` には中置演算子 `(<$>)` という別名があり, `(<$>) = fmap` です. `(+1) <$> Just 3` は `fmap (+1) (Just 3)` と同じで `Just 4` を返します. 関数適用 `$` の関手版という見立てで, 実務ではこちらもよく使われます.
:::

::: note

### Exercise CH9-4

**自作多相型 `Box` を Functor にする**

値を 1 つ包む多相データ型 `Box a` を定義し, `Functor` インスタンスを実装してください. `fmap` は「箱の中身に関数を適用し, 箱に入れ直す」操作です. また, 箱から中身を取り出す関数 `unBox :: Box a -> a` も定義してください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (unBox (fmap (+ 1) (Box 10)))      -- 11
  print (unBox (fmap show (Box (42 :: Int))))  -- "42"
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box x) = Box (f x)

unBox :: Box a -> a
unBox (Box x) = x

main :: IO ()
main = do
  print (unBox (fmap (+ 1) (Box 10)))          -- 11
  print (unBox (fmap show (Box (42 :: Int))))  -- "42"
~~~

`fmap f (Box x) = Box (f x)` が, 箱の構造 (`Box` でくるむこと) を保ちつつ中身 `x` だけを `f` で変換しています. これが関手の「入れ物の形は保ち, 中身に射を作用させる」性質そのものです. `fmap id (Box x) = Box x` で恒等射が保たれていることも確認できます.

</details>

:::

## 多相型と自然変換

関手が「圏どうしの変換」だとすれば, **自然変換 (natural transformation)** は「関手どうしの変換」です. 2 つの関手 `f` と `g` があるとき, **型 `a` に依らず一様に** `f a` を `g a` に変換する操作が自然変換です. Haskell では, 次のような **多相関数** がこれにあたります.

$$\alpha : \forall a.\ f\,a \to g\,a$$

ここで先頭の $\forall a$ ([第7章](fp7.html)で導入した **全称命題** の量化子) が効いています. 「**すべての** 型 `a` について `f a -> g a` が成り立つ」, つまり `a` がどんな型でも同じ仕組みで変換できる関数だけが自然変換になります. 「`a` を覗き見て型ごとに振る舞いを変える」ことはできず, **中身の値には触れず, 入れ物 (関手) の構造だけを組み替える**のが特徴です. この「型に依らない一様さ」は **パラメトリック多相 (parametric polymorphism)** と呼ばれ, 本章冒頭で挙げた「多相型 = 自然変換」という対応の正体です.

代表例が, リストと `Maybe` を相互に変換する 2 つの関数です (どちらも `Data.Maybe` にあります).

~~~ haskell
-- リストを Maybe へ: 先頭があれば Just, 空なら Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x : _) = Just x

-- Maybe をリストへ: Just x は [x], Nothing は []
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

main :: IO ()
main = do
  print (listToMaybe [1, 2, 3 :: Int])  -- Just 1
  print (listToMaybe ([] :: [Int]))     -- Nothing
  print (maybeToList (Just 5 :: Maybe Int))  -- [5]
  print (maybeToList (Nothing :: Maybe Int)) -- []
~~~

`listToMaybe :: forall a. [a] -> Maybe a` は, 関手 `[]` から関手 `Maybe` への自然変換です. 型 `a` が `Int` でも `String` でも何でも, 「先頭要素があれば `Just`, 無ければ `Nothing`」という同じ規則で変換します. `maybeToList` はその逆向き (`Maybe` から `[]` へ) の自然変換です. どちらも中身の値 `a` を加工せず, 入れ物の形だけを移し替えています.

自然変換が満たすべき条件を **自然性条件 (naturality)** といいます. 「先に中身を変換してから入れ物を移す」のと「先に入れ物を移してから中身を変換する」のとで結果が一致する, という条件です. 自然変換 $\alpha$ と任意の関数 `g :: a -> b` について,

$$\mathrm{fmap}\ g \circ \alpha = \alpha \circ \mathrm{fmap}\ g$$

が成り立ちます. 左辺は「$\alpha$ で入れ物を移してから `fmap g` で中身を変換」, 右辺は「`fmap g` で中身を変換してから $\alpha$ で入れ物を移す」です. たとえば `listToMaybe` なら, `[10,20,30]` に対して

- 先に `fmap (+1)` してから `listToMaybe`: `[11,21,31]` → `Just 11`
- 先に `listToMaybe` してから `fmap (+1)`: `Just 10` → `Just 11`

と, どちらの順でも `Just 11` で一致します. パラメトリック多相な関数は中身の値に触れないため, この自然性は自動的に成り立ちます (型さえ合えば自然性が従う, という強い性質があります).

これで, 本章冒頭で掲げた対応が一通り埋まりました. 型を **対象**, 関数を **射** とみる圏 Hask の上で, 型引数を持つ多相データ型は **関手** (`fmap` を備える), 型に依らない一様な多相関数は **自然変換** ($\forall a$ で量化された `f a -> g a`) に対応します. [第7章](fp7.html)の集合論, [第8章](fp8.html)の代数に続く第 3 の見方として, 圏論はこれらの構造を統一的に眺める枠組みを与えてくれます.

::: note

### Exercise CH9-5

**自然変換 `firstTwo` の実装**

リストから先頭 2 要素までを取り出して `Maybe` のペアにする自然変換 `firstTwo :: [a] -> Maybe (a, a)` を実装してください. 要素が 2 つ以上あれば先頭 2 つを `Just (x, y)` で, それ未満なら `Nothing` を返します. この関数は型 `a` に依らず一様に動く (パラメトリック多相な) 点を意識してください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (firstTwo [1, 2, 3 :: Int])  -- Just (1,2)
  print (firstTwo [1 :: Int])        -- Nothing
  print (firstTwo ([] :: [Int]))     -- Nothing
  print (firstTwo "abc")             -- Just ('a','b')
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
firstTwo :: [a] -> Maybe (a, a)
firstTwo (x : y : _) = Just (x, y)
firstTwo _           = Nothing

main :: IO ()
main = do
  print (firstTwo [1, 2, 3 :: Int])  -- Just (1,2)
  print (firstTwo [1 :: Int])        -- Nothing
  print (firstTwo ([] :: [Int]))     -- Nothing
  print (firstTwo "abc")             -- Just ('a','b')
~~~

`firstTwo` は要素の型 `a` を一切覗かず (`Int` でも `Char` でも同じ規則), 「先頭 2 つがあるか」という入れ物の形だけで結果を決めています. このため `[a]` から `Maybe (a, a)` への自然変換になっており, `firstTwo "abc"` のように文字列でもそのまま動きます.

</details>

:::
