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

本章は 2 部構成です. 前半 **多相データ型** では, 型引数 `a` を持つデータ型を整理します. まず, 失敗を型で表す `Maybe` と二者択一を表す `Either` ([第7章](fp7.html)で「後の章で扱う」と先送りした型) を扱い, 続いて既習のリスト, 実用的な辞書型 `Map`, その内部構造を自作で確かめるツリー (二分探索木) を扱います.

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

以下では, 型引数 `a` を持つ多相データ型を 5 つ見ていきます. まず, 失敗を表す **`Maybe`** と二者択一を表す **`Either`** ([第7章](fp7.html)で先送りした型の回収) から始めます. 続いて, **リスト `[a]`** ([第8章](fp8.html)で学んだ **モノイド** の実例), 実務でよく使う辞書型 **`Map`**, そして `Map` の **内部構造** と「自作の型への **`Monoid`** 定義」を理解するための **ツリー `Tree a`** (二分探索木) を順に扱います. これらはいずれも「型引数を持つ多相データ型」であると同時に, モノイドや関手といった構造も備えます (関手は後半 [圏論的解釈](#圏論的解釈) の **Functor** の節で扱います).

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

エラー処理では, **`Left` を失敗 (理由つき) / `Right` を成功** に使うのが Haskell の慣習です. 「正しい (right)」と「右 (right)」を掛けた語呂で, 成功を `Right` に割り当てると覚えるとよいでしょう.

では, 失敗の理由 `a` はどんな型で持たせるべきでしょうか. いちばん手軽なのは `Either String b` のように **文字列** で持たせることですが, 純粋関数型らしく設計するなら, **起こりうるエラーを列挙した専用の直和型** を作るほうが適切です. 「ある計算で起こりうる失敗」はふつう有限個しかありません. その候補を [第7章](fp7.html) の **直和型 (列挙型)** としてあらかじめ書き出しておくのです. 割り算の失敗が「0 除算」1 種類なら, 候補全体を直和で書けば $\mathrm{DivError} = \mathrm{DivByZero}$ (エラーの種類が増えれば $e_1 + e_2 + \cdots$ と構築子を足していく) となります. 前節の `safeDiv` を, この専用エラー型で書き直します.

~~~ haskell
-- 起こりうる失敗を列挙した専用のエラー型 (いまは 1 種類)
data DivError = DivByZero
  deriving (Show, Eq)

safeDivE :: Int -> Int -> Either DivError Int
safeDivE _ 0 = Left DivByZero
safeDivE x y = Right (x `div` y)

main :: IO ()
main = do
  print (safeDivE 10 2)  -- Right 5
  print (safeDivE 10 0)  -- Left DivByZero
~~~

`safeDiv` が `Nothing` を返していた箇所で, `safeDivE` は `Left DivByZero` と **理由つきの失敗** を返します. 理由が「文字列という値」ではなく **専用の型** になったことで, 3 つの利点が生まれます.

- **失敗が閉じた集合になる**: 起こりうるエラーが型の構築子として列挙されるので, 「どんな失敗がありうるか」を型を見るだけで把握できます. 文字列だと理由は無限に書けてしまい, 呼び出す側は一覧を追えません.
- **網羅性を検査できる**: `Left` を受けてパターンマッチするとき, 構築子を書き漏らすと GHC が警告します ([第7章](fp7.html) の直和型と同じ利点). 文字列にはこの検査が効きません.
- **表示 (プレゼンテーション) を値から分離できる**: 「何が起きたか」は `DivByZero` という値で持ち, 「どう見せるか」は **別の関数** で与えます.

最後の点を担うのが, エラー値を人間向けの文字列へ変換する関数です. 受け取る側は, これと `Right` の整形を組み合わせて表示します.

~~~ haskell
-- エラー値を人間向けメッセージへ (表示を値から分離する)
renderDivError :: DivError -> String
renderDivError DivByZero = "0 では割れません"

report :: Either DivError Int -> String
report (Left e)  = "エラー: " ++ renderDivError e
report (Right n) = "結果: " ++ show n

main :: IO ()
main = do
  putStrLn (report (safeDivE 10 2))  -- 結果: 5
  putStrLn (report (safeDivE 10 0))  -- エラー: 0 では割れません
~~~

`renderDivError` を差し替えれば, 同じ `DivByZero` を英語で出すことも, ログ用に整形することもできます. **「何が起きたか (値)」と「どう見せるか (文字列)」を分けられる** のが, 専用エラー型の実務上の効きどころです.

::: note

**文字列でもエラーは書ける.** 手早く試すだけなら, 理由をそのまま文字列で持たせて `Either String Int` としてもかまいません.

~~~ haskell
-- 文字列版: 手軽だが「閉じた集合」でも「網羅検査」でもない
safeDivS :: Int -> Int -> Either String Int
safeDivS _ 0 = Left "0 では割れません"
safeDivS x y = Right (x `div` y)
~~~

この書き方は簡単ですが, 失敗の集合が閉じず (どんな文字列でも `Left` にできてしまう), 網羅性検査も効かず, さらに次の warn で述べる **`print` の日本語エスケープ** にも直面します. プロトタイプや使い捨ての計算では文字列で十分ですが, エラーを設計の一部として扱うなら専用の直和型を選ぶ, と使い分けるとよいでしょう.

:::

::: warn

**日本語のエラー文字列を `print` すると化ける.** [第8章](fp8.html) で見たとおり `print x = putStrLn (show x)` であり, `show` は ASCII の範囲外の文字を `\12391` (「で」) のような十進エスケープに置き換えます (`show` の結果をそのまま Haskell コードに貼り戻せるようにするため). 第8章の例は「`String` を返す関数」だったので `putStrLn` に替えれば済みましたが, `Either String Int` のように **`Show` 表現の内側に日本語 `String` が入れ子** になっている場合はやっかいです.

~~~ haskell
ghci> print (Left "0 では割れません" :: Either String Int)
Left "0 \12391\12399\21106\12428\12414\12379\12435"   -- ASCII の "0 " は残り, 日本語だけ化ける
~~~

値全体は `String` ではなく `Either` なので, `putStrLn` に単純に替えることはできません (`putStrLn :: String -> IO ()` は `String` しか受け取れない). 対処は 2 通りあります.

1. **専用エラー型 + `render` 関数で組む (本節の方針)**: `Left DivByZero` は `Show` しても `Left DivByZero` という ASCII だけの表現になり, そもそもエスケープが起きません. 日本語は `renderDivError` が返す `String` にだけ現れ, それを `putStrLn` で出します. 純粋関数型として設計する動機に, この表示上の利点も加わるわけです.
2. **`unicode-show` パッケージの `uprint` / `ushow` を使う**: どうしても日本語を含む `Show` 表現をそのまま出したいときは, エスケープせず表示する `uprint` (= `putStrLn . ushow`) が使えます.

~~~ haskell
ghci> import Text.Show.Unicode (uprint)
ghci> uprint (Left "0 では割れません" :: Either String Int)
Left "0 では割れません"   -- エスケープされない
~~~

`unicode-show` は標準ライブラリ外なので, 使うにはプロジェクトの依存に加える必要があります (`stack` なら `package.yaml` の `dependencies` に `unicode-show` を足す). GHCi で常用したいなら `:set -interactive-print Text.Show.Unicode.uprint` で既定の表示器を差し替える手もあります.

:::

`Maybe` と `Either` の使い分けは「失敗の理由が要るか」です. 理由が不要なら `Maybe`, 理由を運びたいなら `Either` を使い, その理由の型は **専用の直和型を第一候補** に (手軽さを優先するなら `String` でも) 選びます.

::: note

### Exercise CH9-2

**理由つきの検証 `checkAge` (専用エラー型)**

年齢の検証で起こりうる失敗を, まず **専用の直和型** `AgeError` として列挙してください. 「負の年齢」と「大きすぎる年齢」の 2 種類なので, 直和で書けば $\mathrm{AgeError} = \mathrm{Negative} + \mathrm{TooLarge}$ です. これを使い, `Int` を受け取って妥当なら `Right 年齢`, 不正なら対応する `Left` を返す関数 `checkAge :: Int -> Either AgeError Int` を実装してください (0 未満なら `Negative`, 150 より大きいなら `TooLarge`). さらに, エラー値を日本語メッセージへ変換する `renderAgeError :: AgeError -> String` も書いてください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (checkAge 30)                                    -- Right 30
  print (checkAge (-1))                                  -- Left Negative
  print (checkAge 200)                                   -- Left TooLarge
  putStrLn (either renderAgeError show (checkAge (-1)))  -- 年齢が負です
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
-- ① 起こりうる失敗を直和型で列挙する
data AgeError = Negative | TooLarge
  deriving (Show, Eq)

-- ② その型を Left に載せて検証する
checkAge :: Int -> Either AgeError Int
checkAge n
  | n < 0     = Left Negative
  | n > 150   = Left TooLarge
  | otherwise = Right n

-- ③ 表示は値と分離し, render 関数で与える
renderAgeError :: AgeError -> String
renderAgeError Negative = "年齢が負です"
renderAgeError TooLarge = "年齢が大きすぎます"

main :: IO ()
main = do
  print (checkAge 30)                                    -- Right 30
  print (checkAge (-1))                                  -- Left Negative
  print (checkAge 200)                                   -- Left TooLarge
  putStrLn (either renderAgeError show (checkAge (-1)))  -- 年齢が負です
~~~

`Maybe` では「不正だった」ことしか伝えられませんが, 専用エラー型 `AgeError` なら **どう不正か** を型の構築子で伝えられます. `Negative` / `TooLarge` と名前が付くので, 受け取る側は文字列を照合せず構築子でパターンマッチでき, 構築子を書き漏らせば網羅性の警告が出ます. 日本語メッセージは `renderAgeError` に切り出し, 値と表示を分けています. (文字列で `Left "年齢が負です"` と持たせることもできますが, その場合は本文の warn で述べた `print` のエスケープに注意が要ります.)

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
  -- either 左用関数 右用関数 Either値 (Left は renderDivError で文字列化)
  putStrLn (either (\e -> "エラー: " ++ renderDivError e) (\n -> "結果: " ++ show n) (safeDivE 10 2))  -- 結果: 5
  putStrLn (either (\e -> "エラー: " ++ renderDivError e) (\n -> "結果: " ++ show n) (safeDivE 10 0))  -- エラー: 0 では割れません
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

**`Maybe` を `Either` へ変換 (エラー型を選べる `toEither`)**

`safeDiv :: Int -> Int -> Maybe Int` (本文の定義) の結果を, 理由つきの `Either e a` に変換する関数 `toEither :: e -> Maybe a -> Either e a` を実装してください. `Nothing` のときは渡した理由 (エラー値) を `Left` に, `Just x` のときは `Right x` にします. 理由の型を **`e` のまま多相** にしておくと, 本文の `DivError` のような専用エラー型でも, 手軽な `String` でも, **同じ関数** で変換できます.

~~~ haskell
-- 実行例 (理由には本文の DivError を使う)
main :: IO ()
main = do
  print (toEither DivByZero (safeDiv 10 2))          -- Right 5
  print (toEither DivByZero (safeDiv 10 0))          -- Left DivByZero
  print (toEither "0 では割れません" (safeDiv 10 2))  -- Right 5   (同じ toEither が String でも動く)
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data DivError = DivByZero
  deriving (Show, Eq)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- 理由の型 e を固定しない (専用エラー型でも String でも動く)
toEither :: e -> Maybe a -> Either e a
toEither reason Nothing  = Left reason
toEither _      (Just x) = Right x

main :: IO ()
main = do
  print (toEither DivByZero (safeDiv 10 2))          -- Right 5
  print (toEither DivByZero (safeDiv 10 0))          -- Left DivByZero
  print (toEither "0 では割れません" (safeDiv 10 2))  -- Right 5
~~~

`toEither` は「理由を持たない失敗 (`Maybe`)」を「理由つきの失敗 (`Either`)」へ橋渡しします. 理由の型 `e` を固定しないことで, 専用エラー型と文字列のどちらにも同じコードで対応できます. 型を多相にできるのは, `toEither` が中身 `a` にも理由 `e` にも触れず ただ運ぶだけだからで, これは `Maybe` から `Either e` への自然変換になっています.

</details>

:::

## リスト

[第7章](fp7.html)では「リストが代数的にどのように定義されるかは後の章で扱う」と予告しました ([第7章](fp7.html)「集合の内包表記と代数的データ型」の警告). その回収です.

### リストは再帰的な代数的データ型

組込のリスト `[a]` は, [第7章](fp7.html)で作った再帰型 `Nat` (`Zero | Succ Nat`) と同じ仲間 — **自分自身を参照する代数的データ型** です. 違いは, `Succ` が「1 つ前の自然数」を抱えるだけだったのに対し, リストは各段に **要素の値も一緒に** 抱える点です. リストの構築子は次の 2 つで,

- `[]` : **空リスト**. 要素を 1 つも持たない基底.
- `(:)` : **cons**. 先頭の要素 `x` と残りのリスト `xs` から `x : xs` を作る再帰.

`[1,2,3]` は糖衣にすぎず, 実体は `1 : (2 : (3 : []))` です. この構造をそのまま `data` で書くと, 組込リストの正体が見えます. `[a]` と同型な自作版 `List a` を書いてみましょう.

~~~ haskell
-- 組込リストの正体:  data [a] = [] | a : [a]
-- それを自作の名前で書いたもの (Nil ↔ [], Cons ↔ (:))
data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

-- [1,2,3] に対応するのは  Cons 1 (Cons 2 (Cons 3 Nil))

-- 長さ: Nil で 0, Cons で「1 + 残りの長さ」
len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

-- 連結: 左が空なら右をそのまま, Cons なら先頭を残して残りを再帰的に連結
append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

main :: IO ()
main = do
  let xs = Cons 1 (Cons 2 (Cons 3 Nil))
      ys = Cons 4 (Cons 5 Nil)
  print (len xs)        -- 3
  print (append xs ys)  -- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))
~~~

`len` も `append` も, **型の再帰構造に沿って** 定義されています. 構築子が `Nil` (基底) と `Cons` (再帰) の 2 通りなので, 関数もその 2 通りに場合分けし, `Cons` の枝で「残りのリスト `xs`」へ再帰します. これが[第6章](fp6.html)・[第7章](fp7.html)で繰り返した **構造的再帰** で, データの形と関数の形がそのまま対応します.

そして, この `append` こそが組込の `(++)` にほかなりません (`Nil ↔ []`, `Cons ↔ (:)` と読み替えれば `(++)` の定義そのものです). つまりリストの連結は「型の再帰構造をたどって末尾までつなぐ」再帰関数として定義され, その `(++)` と `[]` が, 次に見るリストのモノイド構造を与えます.

### リストはモノイド

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

## Map — キーと値の対応

次に, 連想的なデータを扱うとき実務で最もよく使う, 標準ライブラリ `containers` の **`Map`** を見ます. `Map k v` は **キー `k` から値 `v` を引ける辞書 (連想配列)** を表す多相データ型で, 型引数を **2 つ** 取ります (`Map :: * -> * -> *`). リストと同じ多相型ですが, 自作せずライブラリの型をそのまま使う点が違います (次節では逆に, この種の構造を自分で作って中身を理解します).

~~~ haskell
import qualified Data.Map as Map
import Data.Map (Map)

-- キー (果物名) から 値 (個数) への対応
stock :: Map String Int
stock = Map.fromList [("apple", 3), ("banana", 2)]
~~~

キーで値を引くには `Map.lookup` を使います. キーが無いこともあるので, 結果は **`Maybe v`** で返ります (`Maybe` は前の節で扱いました).

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

このように `Map` は, **多相型であり, モノイドであり, 関手でもある**, 実務で頻出の型です. 関手としての側面は後半の **Functor** の節で他の型と合わせて整理します.

## ツリー

前節の `Map` (やその仲間の `Set`) は, 実は内部的に **平衡二分探索木 (balanced binary search tree)** で実装されています. 普段はライブラリの `Map` / `Set` を使えば十分ですが, ここでは ① その **内部構造** がどうなっているか, ② 自作のデータ型に **`Monoid` をどう定義するか** の 2 点を理解するために, 簡単な二分探索木 (binary search tree) を自分で作ってみます.

二分探索木は, 各 **節点 (node)** が 1 つの値と左右 2 つの部分木を持つ木で, どの節点でも

**左部分木のすべての値 $<$ 節点の値 $<$ 右部分木のすべての値**

という **不変条件** を保ちます. この順序のおかげで, 中間順 (左 → 節点 → 右) に走査すると値が **昇順** に並びます. 値 $\{1, 3, 5, 8, 9\}$ を持つ二分探索木の例を図に示します (同じ要素集合でも, 挿入順によって木の形は変わりえます. 詳しくは下の注意を参照).

<svg viewBox="0 0 460 340" width="100%" style="max-width: 520px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="二分探索木の例: 根 5, 左部分木の根 3 (左の子 1), 右部分木の根 8 (右の子 9). 各節点で 左 < 節点 < 右 を満たし, 中間順走査で 1 3 5 8 9 と昇順になる">
  <line x1="210" y1="44" x2="120" y2="120" stroke="currentColor" stroke-width="1.5"/>
  <line x1="210" y1="44" x2="300" y2="120" stroke="currentColor" stroke-width="1.5"/>
  <line x1="120" y1="120" x2="72" y2="196" stroke="currentColor" stroke-width="1.5"/>
  <line x1="300" y1="120" x2="348" y2="196" stroke="currentColor" stroke-width="1.5"/>
  <g stroke="currentColor" stroke-width="1" opacity="0.4">
    <line x1="120" y1="120" x2="168" y2="190"/>
    <line x1="300" y1="120" x2="252" y2="190"/>
    <line x1="72" y1="196" x2="44" y2="264"/>
    <line x1="72" y1="196" x2="100" y2="264"/>
    <line x1="348" y1="196" x2="320" y2="264"/>
    <line x1="348" y1="196" x2="376" y2="264"/>
  </g>
  <g fill="none" stroke="currentColor" stroke-width="1" stroke-dasharray="3 2" opacity="0.55">
    <rect x="162" y="184" width="12" height="12" rx="2"/>
    <rect x="246" y="184" width="12" height="12" rx="2"/>
    <rect x="38" y="258" width="12" height="12" rx="2"/>
    <rect x="94" y="258" width="12" height="12" rx="2"/>
    <rect x="314" y="258" width="12" height="12" rx="2"/>
    <rect x="370" y="258" width="12" height="12" rx="2"/>
  </g>
  <g fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.5">
    <circle cx="210" cy="44" r="19"/>
    <circle cx="120" cy="120" r="19"/>
    <circle cx="300" cy="120" r="19"/>
    <circle cx="72" cy="196" r="19"/>
    <circle cx="348" cy="196" r="19"/>
  </g>
  <g fill="currentColor" font-size="15" font-weight="600" text-anchor="middle">
    <text x="210" y="49">5</text>
    <text x="120" y="125">3</text>
    <text x="300" y="125">8</text>
    <text x="72" y="201">1</text>
    <text x="348" y="201">9</text>
  </g>
  <g font-size="11">
    <circle cx="22" cy="24" r="7" fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.2"/>
    <text x="34" y="28" fill="currentColor">= Node (左部分木・値・右部分木)</text>
    <rect x="16" y="40" width="12" height="12" rx="2" fill="none" stroke="currentColor" stroke-width="1" stroke-dasharray="3 2"/>
    <text x="34" y="50" fill="currentColor">= Leaf (空の木)</text>
  </g>
  <g fill="currentColor" font-size="12" text-anchor="middle">
    <text x="230" y="300">不変条件: 左部分木の値 &lt; 節点の値 &lt; 右部分木の値</text>
    <text x="230" y="320" font-weight="600">中間順走査 toList' → 1  3  5  8  9 (昇順)</text>
  </g>
</svg>

Haskell では, この構造をそのまま **再帰的な代数的データ型** で表せます ([第7章](fp7.html)で作った再帰型 `Nat` と同じく型が自分自身を参照しますが, ここではさらに **型引数** `a` を載せて, 任意の要素型を持つ木に多相化します). 空の木を `Leaf` (図の破線の四角), 値と左右の部分木を持つ節点を `Node (Tree a) a (Tree a)` (図の丸) とします. 続いて, 不変条件を保つ挿入 `insert'`, 中間順走査 `toList'` を定義し, **「要素の集まりを表す木」** としてモノイドを与えます.

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

もう一つ, 対象が相異なる具体例で **合成** を図にしてみましょう. 3 つの型 `Bool`, `Int`, `String` を **対象**, それらをつなぐ 2 つの関数を **射** とします.

~~~ haskell
-- 対象 = 型, 射 = 関数.  2 つの射:
--   fromEnum :: Bool -> Int      False → 0, True → 1
--   show     :: Int  -> String   数を文字列へ
showBit :: Bool -> String
showBit = show . fromEnum        -- 合成した射 g . f

main :: IO ()
main = do
  print (fromEnum True)    -- 1     (Bool -> Int)
  print (show (1 :: Int))  -- "1"   (Int -> String)
  print (showBit True)     -- "1"   (show . fromEnum, Bool -> String)
~~~

`show . fromEnum` は, `Bool` から `String` への **1 本の射** です. `True` を渡すと `fromEnum` で `Int` の `1` を経由し, `show` で `"1"` になります. この「対象・射・合成」の関係を描いたのが次の **可換図式 (commutative diagram)** です. `Bool` から `String` へ至る 2 つの経路 — 上を回って `fromEnum` してから `show`, あるいは対角の `show . fromEnum` を直接たどる — が **同じ射** になる (これを「図式が **可換** である」といいます) ことを表しています.

<svg viewBox="0 0 460 250" width="100%" style="max-width: 520px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="圏の合成を表す可換図式. 対象 Bool から fromEnum で Int へ, Int から show で String へ矢印が伸び, さらに Bool から String へ直接 show . fromEnum の対角の矢印が伸びる. fromEnum してから show をたどる経路と, 対角の show . fromEnum をたどる経路は, どちらも同じ射 Bool から String を表し, 図式は可換である.">
  <defs>
    <marker id="cd-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <g stroke="currentColor" stroke-width="1.5" fill="none">
    <line x1="104" y1="52" x2="358" y2="52" marker-end="url(#cd-arrow)"/>
    <line x1="388" y1="72" x2="388" y2="181" marker-end="url(#cd-arrow)"/>
    <line x1="80" y1="74" x2="350" y2="184" marker-end="url(#cd-arrow)"/>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="17" font-weight="600" text-anchor="middle">
    <text x="72" y="58">Bool</text>
    <text x="388" y="58">Int</text>
    <text x="388" y="205">String</text>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="13">
    <text x="231" y="42" text-anchor="middle">fromEnum</text>
    <text x="400" y="132" text-anchor="start">show</text>
    <text x="180" y="158" text-anchor="middle">show . fromEnum</text>
  </g>
  <text x="230" y="238" fill="currentColor" font-size="12" text-anchor="middle">2 経路はどちらも同じ射 Bool → String — 図式は可換</text>
</svg>

矢印が **射**, 頂点が **対象**, 対角線が **合成した射** です. 合成 $g \circ f$ とは, まさに「この図で `f` の矢印と `g` の矢印を継いで得られる対角の矢印」にほかなりません.

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

別の見方をすると, `fmap` は [第4章](fp4.html) でリストに使った `map` を **任意の関手に一般化** したものです. `map` の型 `(a -> b) -> [a] -> [b]` の `[]` を一般の型構築子 `f` で置き換えると, ちょうど `fmap :: (a -> b) -> f a -> f b` になります.

$$\underbrace{(a \to b) \to [a] \to [b]}_{\text{map (リスト専用)}} \;\;\Longrightarrow\;\; \underbrace{(a \to b) \to f\,a \to f\,b}_{\text{fmap (任意の関手)}}$$

つまり `fmap` は「**`f a` という入れ物の中身 `a` だけに関数を作用させ, 入れ物の形は保つ `map`**」です. 関手ごとに「形をどう保つか」 — `Maybe` なら `Nothing` はそのまま, リストなら各要素に, 木なら各ノードに — が決まります. 以下, `Maybe`・`Either`・リスト・木の順に実例を見ます.

まず, 本章前半で扱った `Maybe` を関手として見ます. `fmap` は「`Just` の中身に関数を適用し, `Nothing` はそのまま返す」操作になります.

~~~ haskell
main :: IO ()
main = do
  print (fmap (+ 1) (Just 3))            -- Just 4
  print (fmap (+ 1) (Nothing :: Maybe Int))  -- Nothing
~~~

`fmap (+1) (Just 3)` が `Just 4` になり, `Nothing` には何も起きません. 「箱の中身に関数を適用する」イメージで, 箱が空 (`Nothing`) なら何もしないわけです.

**`Either a` も関手** です. ただし `Either` は型引数を 2 つ取る (`Either :: * -> * -> *`) ため, 関手にするには片方を固定して `Either a` (種が `* -> *`) の形にします. `fmap` は `Right` の中身にだけ作用し, `Left` (慣習上は失敗) はそのまま素通しします.

~~~ haskell
main :: IO ()
main = do
  print (fmap (+ 1) (Right 3 :: Either String Int))  -- Right 4
  print (fmap (+ 1) (Left "err" :: Either String Int))  -- Left "err"
~~~

`Maybe` と `Either a` は, どちらも「失敗するかもしれない値」を表す関手です. `fmap` で成功側 (`Just` / `Right`) の中身だけを加工しつつ, 失敗 (`Nothing` / `Left`) が出たらそれ以降の加工を素通しできます.

次に **リストも関手** です. リストは組込みで `Functor` のインスタンスになっており, リストに対する `fmap` は, すでに [第4章](fp4.html) で学んだ `map` そのものです. 「`fmap` は `map` の一般化」と述べたことが, リストでは文字どおり `fmap = map` という形で現れます.

その実装も, [第4章](fp4.html) の `map` の再帰定義をそのまま書いたものです. 組込みのインスタンスと同じ定義を `fmapList` として書き下し, `fmap`・`map` と一致することを確かめます.

~~~ haskell
-- リストの fmap の中身 = 第4章の map の再帰定義そのもの
fmapList :: (a -> b) -> [a] -> [b]
fmapList _ []       = []
fmapList f (x : xs) = f x : fmapList f xs

main :: IO ()
main = do
  print (fmapList (* 2) [1, 2, 3])  -- [2,4,6]
  print (fmap     (* 2) [1, 2, 3])  -- [2,4,6]   (組込みの fmap = map)
  print (map      (* 2) [1, 2, 3])  -- [2,4,6]
~~~

3 つの結果はすべて一致します. `fmapList` は「空リストには何もせず, `(x : xs)` の各要素に関数を適用して同じ形のリストに組み直す」操作で, リストの長さや並び (入れ物の形) は保ったまま, 中身だけを変えています. これが, リストという関手の「形を保つ」やり方です.

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
