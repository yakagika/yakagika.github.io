-- | fp12.md 総合演習 本体「式評価器 + REPL」の純粋コア (eval / parseRPN / run / Stats).
--   IO の REPL (runRepl) はコンパイル確認のみで, hspec の検証対象外.
module Fp12.CalcSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck
import Text.Read (readMaybe)
import Data.Bifunctor (first)
import System.IO (hFlush, stdout, isEOF)

-- 式 (再帰的 ADT, fp7)
data Expr
  = Num Double
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  deriving (Show, Eq)

-- 評価エラー (fp9 Either / fp10 monad)
data EvalError = DivByZero
  deriving (Show, Eq)

-- 木をたどって評価する. 0 除算は Either で持ち上げて伝播させる.
eval :: Expr -> Either EvalError Double
eval (Num x)   = Right x
eval (Add a b) = do { x <- eval a; y <- eval b; Right (x + y) }
eval (Sub a b) = do { x <- eval a; y <- eval b; Right (x - y) }
eval (Mul a b) = do { x <- eval a; y <- eval b; Right (x * y) }
eval (Div a b) = do
  x <- eval a
  y <- eval b
  if y == 0 then Left DivByZero else Right (x / y)

-- RPN (逆ポーランド記法) パース: words を畳んでスタック上に Expr 木を組む (fp6 + fp7).
data ParseError
  = BadToken String
  | StackUnderflow
  | LeftoverOperands
  deriving (Show, Eq)

parseRPN :: String -> Either ParseError Expr
parseRPN input = foldStack [] (words input) >>= final
  where
    foldStack stack []       = Right stack
    foldStack stack (t : ts) = step stack t >>= \s -> foldStack s ts

    step stack tok = case tok of
      "+" -> binop Add stack
      "-" -> binop Sub stack
      "*" -> binop Mul stack
      "/" -> binop Div stack
      _   -> case readMaybe tok of
               Just n  -> Right (Num n : stack)
               Nothing -> Left (BadToken tok)

    binop f (b : a : rest) = Right (f a b : rest)
    binop _ _              = Left StackUnderflow

    final [e] = Right e
    final _   = Left LeftoverOperands

-- パースと評価の 2 種類のエラーを 1 つの直和型にまとめる (fp7 直和型 + fp9 Either).
data Error = Parse ParseError | Eval EvalError
  deriving (Show, Eq)

run :: String -> Either Error Double
run input = do
  expr <- first Parse (parseRPN input)
  first Eval (eval expr)

-- 評価結果の統計 (fp8 章末 Stats の Double 版を再利用).
data Stats = Stats { statCount :: Int, statSum :: Double }
  deriving (Show, Eq)

instance Semigroup Stats where
  Stats c1 s1 <> Stats c2 s2 = Stats (c1 + c2) (s1 + s2)

instance Monoid Stats where
  mempty = Stats 0 0

record :: Double -> Stats
record x = Stats 1 x

summarize :: [Double] -> Stats
summarize = foldMap record

mean :: Stats -> Double
mean (Stats c s) = s / fromIntegral c

-- REPL 本体 (IO, fp10/11). コンパイル確認のみ (hspec では実行しない).
showStats :: Stats -> String
showStats st = "件数 " ++ show (statCount st)
            ++ " / 合計 " ++ show (statSum st)
            ++ (if statCount st > 0 then " / 平均 " ++ show (mean st) else "")

runRepl :: Stats -> IO ()
runRepl stats = do
  putStr "> " >> hFlush stdout
  eof <- isEOF
  if eof
    then putStrLn ("\n--- 評価結果の統計 ---\n" ++ showStats stats)
    else do
      line <- getLine
      case line of
        ":stats" -> putStrLn (showStats stats) >> runRepl stats
        _        -> case run line of
          Right v  -> putStrLn ("= " ++ show v) >> runRepl (stats <> record v)
          Left err -> putStrLn ("エラー: " ++ show err) >> runRepl stats

-- 検証: 公理をそのまま property に (fp12 改稿: 代数ファースト)

instance Arbitrary Expr where
  arbitrary = sized gen
    where
      gen 0 = Num <$> arbitrary
      gen n = oneof [ Num <$> arbitrary
                    , Add <$> sub' <*> sub', Sub <$> sub' <*> sub'
                    , Mul <$> sub' <*> sub', Div <$> sub' <*> sub' ]
        where sub' = gen (n `div` 2)

prop_num :: Double -> Bool
prop_num x = eval (Num x) == Right x

prop_add :: Expr -> Expr -> Bool
prop_add a b = eval (Add a b) == ((+) <$> eval a <*> eval b)

prop_sub :: Expr -> Expr -> Bool
prop_sub a b = eval (Sub a b) == ((-) <$> eval a <*> eval b)

prop_mul :: Expr -> Expr -> Bool
prop_mul a b = eval (Mul a b) == ((*) <$> eval a <*> eval b)

prop_div_zero :: Expr -> Bool
prop_div_zero a = eval (Div a (Num 0)) == Left DivByZero

prop_div_ok :: Expr -> NonZero Double -> Bool
prop_div_ok a (NonZero y) = eval (Div a (Num y)) == ((/) <$> eval a <*> pure y)

prop_stats_hom :: [Int] -> [Int] -> Bool
prop_stats_hom xs ys =
  summarize (map fromIntegral (xs ++ ys))
    == summarize (map fromIntegral xs) <> summarize (map fromIntegral ys)

spec :: Spec
spec = describe "Fp12.Calc (式評価器の純粋コア)" $ do
  describe "評価の公理 (QuickCheck property)" $ do
    prop "eval (Num x) = Right x" prop_num
    prop "eval (Add a b) = (+) <$> eval a <*> eval b" prop_add
    prop "eval (Sub a b) = (-) <$> eval a <*> eval b" prop_sub
    prop "eval (Mul a b) = (*) <$> eval a <*> eval b" prop_mul
    prop "eval (Div a (Num 0)) = Left DivByZero" prop_div_zero
    prop "y /= 0 なら eval (Div a (Num y)) = (/) <$> eval a <*> pure y" prop_div_ok
    prop "summarize は準同型: summarize (xs++ys) = summarize xs <> summarize ys" prop_stats_hom
  describe "eval (Expr -> Either EvalError Double)" $ do
    it "eval (Add (Num 1) (Mul (Num 2) (Num 3))) == Right 7" $
      eval (Add (Num 1) (Mul (Num 2) (Num 3))) `shouldBe` Right 7
    it "0 除算は Left DivByZero" $
      eval (Div (Num 1) (Num 0)) `shouldBe` Left DivByZero
    it "入れ子の 0 除算も伝播する" $
      eval (Add (Num 1) (Div (Num 2) (Sub (Num 3) (Num 3)))) `shouldBe` Left DivByZero

  describe "parseRPN (String -> Either ParseError Expr)" $ do
    it "\"3 4 +\" は Add (Num 3) (Num 4)" $
      parseRPN "3 4 +" `shouldBe` Right (Add (Num 3) (Num 4))
    it "\"3 4 + 5 *\" は (3+4)*5 の木" $
      parseRPN "3 4 + 5 *" `shouldBe` Right (Mul (Add (Num 3) (Num 4)) (Num 5))
    it "未知トークンは BadToken" $
      parseRPN "3 x +" `shouldBe` Left (BadToken "x")
    it "演算子過多は StackUnderflow" $
      parseRPN "3 +" `shouldBe` Left StackUnderflow
    it "オペランド過多は LeftoverOperands" $
      parseRPN "3 4" `shouldBe` Left LeftoverOperands

  describe "run (パース→評価をまとめる)" $ do
    it "\"10 2 /\" == Right 5.0" $
      run "10 2 /" `shouldBe` Right 5.0
    it "評価エラーは Eval に包む" $
      run "1 0 /" `shouldBe` Left (Eval DivByZero)
    it "パースエラーは Parse に包む" $
      run "3 x +" `shouldBe` Left (Parse (BadToken "x"))

  describe "Stats (fp8 章末 Monoid の回収, 評価結果の統計)" $ do
    it "record 3 <> record 4 == Stats 2 7" $
      record 3 <> record 4 `shouldBe` Stats 2 7
    it "summarize [3,4,5] == Stats 3 12" $
      summarize [3, 4, 5] `shouldBe` Stats 3 12
    it "空は mempty" $
      summarize [] `shouldBe` (mempty :: Stats)
    it "平均は合計/件数" $
      mean (summarize [2, 4, 9]) `shouldBe` 5.0
