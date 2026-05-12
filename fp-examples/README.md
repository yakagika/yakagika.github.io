# fp-examples

`lectures/fp/fp3.md` 〜 `fp7.md` 内に掲載されている Haskell コード例の **機械的検証用** テストスイートです.

教材改訂で例を壊した場合に `stack test` でただちに検知することを目的としています.

詳細な背景・設計・ロードマップは [`plans/in-progress/fp-examples-verification.md`](../plans/in-progress/fp-examples-verification.md) を参照.

## 実行

```bash
cd fp-examples
stack test
```

初回は GHC 9.6 と各依存パッケージのダウンロードがあるため時間がかかります.

## ディレクトリ構成

```
fp-examples/
├── stack.yaml          # resolver: lts-22.43 (GHC 9.6.6)
├── package.yaml
├── test/
│   ├── Spec.hs         # hspec-discover で *Spec.hs を自動収集
│   └── Fp4/
│       ├── TotalSpec.hs
│       ├── FibSpec.hs
│       ├── SignSpec.hs
│       └── FizzBuzzSpec.hs
└── README.md
```

## 新しい例を追加する

1. `test/<Chapter>/<Name>Spec.hs` を新規作成する.
2. ファイル冒頭で `module <Chapter>.<Name>Spec (spec) where` を宣言する.
3. 教材に掲載した関数定義をそのまま module 内に書く.
4. `spec :: Spec` を定義し, `hspec` の `shouldBe` などで期待値を assert する.
5. `stack test` がパスすることを確認.

例: `test/Fp4/SignSpec.hs`

```haskell
module Fp4.SignSpec (spec) where

import Test.Hspec

sign :: Int -> String
sign n | n > 0     = "正"
       | n < 0     = "負"
       | otherwise = "零"

spec :: Spec
spec = describe "Fp4.Sign" $ do
  it "sign 5 == 正"     $ sign 5     `shouldBe` "正"
  it "sign (-3) == 負"  $ sign (-3)  `shouldBe` "負"
  it "sign 0 == 零"     $ sign 0     `shouldBe` "零"
```

## 検証粒度

各サンプルは以下のいずれか以上の層で検証する.

| 層 | 内容 | 例 |
| --- | --- | --- |
| L1: コンパイル可否 | `stack test` のビルドが通る | 型/データ定義のみの断片 |
| L2: 出力一致 | `shouldBe` で期待値と一致 | `total`, `fizzBuzz`, `sign`, `area` |
| L3: プロパティ検査 | QuickCheck で性質を検証 | `length2 == length` 等 (任意) |

## 検証対象外

- ghci セッション例 (`ghci> :t add` など)
- 故意に失敗する例 (Stack overflow, 型エラーの説明用)

これらは目視レビューで担保する.

## マークダウンとの同期

教材 (`lectures/fp/*.md`) のコード例を編集した場合は **必ず** このディレクトリ内の対応する `*Spec.hs` も更新してください. CLAUDE.md のルールも参照.
