module FpA1.SortSpec (spec) where

import Test.Hspec
import Test.QuickCheck
import Data.List (partition, sort)

-- 補足A「ソートで計算量を確かめる」の掲載コード ---------------------------------

insertionSort :: Ord a => [a] -> [a]
insertionSort = foldr insert []
  where
    insert x [] = [x]
    insert x (y:ys)
      | x <= y    = x : y : ys
      | otherwise = y : insert x ys

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort l) (mergeSort r)
  where
    (l, r) = splitAt (length xs `div` 2) xs
    merge [] ys = ys
    merge xs [] = xs
    merge (x:xs) (y:ys)
      | x <= y    = x : merge xs (y:ys)
      | otherwise = y : merge (x:xs) ys

quickSort :: Ord a => [a] -> [a]
quickSort []     = []
quickSort (p:xs) = quickSort smaller ++ [p] ++ quickSort larger
  where (smaller, larger) = partition (<= p) xs

-- 比較回数を数える計器つき版 (本文では挿入のみ掲載, 他は spec に収録) -----------

insertionSortC :: Ord a => [a] -> (Int, [a])
insertionSortC = foldr step (0, [])
  where
    step x (c, s) = let (c', s') = insertC x s in (c + c', s')
    insertC x [] = (0, [x])
    insertC x (y:ys)
      | x <= y    = (1, x : y : ys)
      | otherwise = let (c, zs) = insertC x ys in (c + 1, y : zs)

mergeSortC :: Ord a => [a] -> (Int, [a])
mergeSortC []  = (0, [])
mergeSortC [x] = (0, [x])
mergeSortC xs  =
  let (l, r)       = splitAt (length xs `div` 2) xs
      (cl, l')     = mergeSortC l
      (cr, r')     = mergeSortC r
      (cm, merged) = mergeC l' r'
  in (cl + cr + cm, merged)
  where
    mergeC [] ys = (0, ys)
    mergeC xs [] = (0, xs)
    mergeC (x:xs) (y:ys)
      | x <= y    = let (c, zs) = mergeC xs (y:ys) in (c + 1, x : zs)
      | otherwise = let (c, zs) = mergeC (x:xs) ys in (c + 1, y : zs)

quickSortC :: Ord a => [a] -> (Int, [a])
quickSortC []     = (0, [])
quickSortC (p:xs) =
  let (sm, lg) = partition (<= p) xs
      cPart    = length xs
      (cs, ss) = quickSortC sm
      (cl, ls) = quickSortC lg
  in (cPart + cs + cl, ss ++ [p] ++ ls)

-- 表で使う入力 ----------------------------------------------------------------

descending :: Int -> [Int]
descending n = [n, n-1 .. 1]

scrambled :: Int -> [Int]           -- 奇数の昇順 ++ 偶数の昇順 (整列も逆順もしていない)
scrambled n = [1,3 .. n] ++ [2,4 .. n]

count :: (Int, [Int]) -> Int
count = fst

spec :: Spec
spec = describe "FpA1.Sort" $ do

  describe "正当性: 3 つのソートは Data.List.sort と一致する" $ do
    let samples = [ [], [1], [3,1,2], [5,2,8,1,7,3,6,4], descending 10, scrambled 9 ]
    it "insertionSort" $ mapM_ (\xs -> insertionSort xs `shouldBe` sort xs) samples
    it "mergeSort"     $ mapM_ (\xs -> mergeSort xs     `shouldBe` sort xs) samples
    it "quickSort"     $ mapM_ (\xs -> quickSort xs     `shouldBe` sort xs) samples
    it "任意のリストでも一致 (QuickCheck)" $ property $ \xs ->
      insertionSort xs == sort (xs :: [Int])
      && mergeSort xs == sort xs
      && quickSort xs == sort xs

  describe "計器つき版も同じ結果を返す" $ do
    it "並べ替え結果は sort と一致 (QuickCheck)" $ property $ \xs ->
      snd (insertionSortC xs) == sort (xs :: [Int])
      && snd (mergeSortC xs) == sort xs
      && snd (quickSortC xs) == sort xs

  describe "計算量の確認: 比較回数が表と一致する" $ do
    -- 挿入ソート最悪 = n(n-1)/2 (逆順入力)
    it "挿入(逆順)は厳密に n(n-1)/2" $
      map (count . insertionSortC . descending) [4,8,16,32,64]
        `shouldBe` [ n*(n-1) `div` 2 | n <- [4,8,16,32,64] ]
    it "挿入(逆順) の実測値" $
      map (count . insertionSortC . descending) [4,8,16,32,64]
        `shouldBe` [6, 28, 120, 496, 2016]
    -- マージソート = O(n log n): 逆順入力での実測値
    it "マージ(逆順) の実測値" $
      map (count . mergeSortC . descending) [4,8,16,32,64]
        `shouldBe` [4, 12, 32, 80, 192]
    -- クイックソート最悪 = n(n-1)/2 (整列済入力で軸が毎回最小)
    it "クイック(整列済=最悪)は挿入最悪と同じ n(n-1)/2" $
      map (\n -> count (quickSortC [1..n])) [4,8,16,32,64]
        `shouldBe` [ n*(n-1) `div` 2 | n <- [4,8,16,32,64] ]
    -- クイックソート平均: 散在入力では最悪よりずっと少ない
    it "クイック(散在) は最悪より少ない" $
      map (count . quickSortC . scrambled) [4,8,16,32,64]
        `shouldBe` [5, 19, 71, 271, 1055]

  describe "伸び方がオーダーを裏づける" $ do
    it "挿入(最悪): n 2 倍で比較は約 4 倍 (O(n^2))" $ do
      let cs = map (count . insertionSortC . descending) [8,16,32,64]
          ratios = zipWith (\a b -> fromIntegral b / fromIntegral a :: Double) cs (tail cs)
      all (> 3.9) ratios `shouldBe` True
    it "マージ: n 2 倍で比較は 3 倍未満 (O(n log n))" $ do
      let cs = map (count . mergeSortC . descending) [8,16,32,64]
          ratios = zipWith (\a b -> fromIntegral b / fromIntegral a :: Double) cs (tail cs)
      all (< 3.0) ratios `shouldBe` True
