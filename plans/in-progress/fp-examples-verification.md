# fp 講義サンプルの検証用 Stack プロジェクト

## メタ情報

- **状態**: in-progress (Phase 0-2 完了 / Phase 3 以降未着手)
- **作成日**: 2026-05-12
- **最終更新**: 2026-05-12 (Phase 1-2 完了反映)

## 概要

`lectures/fp/fp3.md` 〜 `fp7.md` に掲載されている Haskell コード例を機械的にコンパイル・実行検証するための独立した Stack プロジェクトを、リポジトリ内に新規作成する.

## 動機

これまでの章レビューで, **コードを実際に走らせていれば即座に検知できた誤り** が複数発見されている.

代表例:

- `tatal` (typo of `total`) — `fp4.md`
- `f(x-2)` (Python 例での `fib(x-2)` typo) — `fp4.md`
- 出力値の不一致 (回答例の `False` が実は `True`) — `fp3.md` 練習問題
- 真偽値表のセル取り違え — `fp3.md`

これらは目視レビューで偶然見つかったが, 教材の信頼性を担保するには **CI で機械的に検証** したい. また、今後新規例を追加する際にも回帰防止の盾になる.

## ゴール

- 各章の **完全実行可能なサンプル** (`main` を持つもの) が `stack build && stack test` で検証できる.
- 教材改訂で例を壊した場合に CI が落ちる.
- 例の追加コストを可能な限り低く保つ.

## 非ゴール

- **ghci セッション例** (`ghci> :t add` などのインタラクティブ例) は対象外. 目視レビューに委ねる.
- **故意にエラーになる例** (Stack overflow, 型エラー等の説明用) は対象外, あるいは「失敗を期待する」専用カテゴリで扱う.
- **断片的な型・データ定義例** (本体やフィールドだけのもの) は対象外, あるいはコンパイル可否のみの軽い検証に留める.
- マークダウン側から例を自動抽出する仕組みは初期段階では作らない.

## 設計

### Stack プロジェクトの構成判断

既存 `stack.yaml` (リポジトリルート) は `lts-20.10` を resolver とした単一パッケージ Hakyll プロジェクト. fp-examples をどう同居させるかは以下の 2 択.

| 案 | 内容 | 利点 | 欠点 |
| --- | --- | --- | --- |
| **A. 分離** (推奨) | `fp-examples/stack.yaml` を独立に持ち, 完全に別 Stack プロジェクトとする | 依存・resolver を独立管理. blog 側の build に影響しない. 削除/移動も容易. | `.stack-work` が二重 (ディスク使用量増). CI で 2 つビルド対象. |
| B. 統合 | ルート `stack.yaml` の `packages: ['.', 'fp-examples']` に追加 | resolver 統一. `.stack-work` 共有でビルドキャッシュ流用可. | blog (Hakyll) のビルド時に fp-examples も巻き込まれる可能性. 依存衝突リスク. |

**判断**: 教育目的サンプルと Hakyll サイトは独立して進化させたいので **A (分離)** を採用. resolver は blog 本体と必ずしも揃える必要はなく, 「教材で前提とする学生環境」に近い LTS を選ぶ (具体的バージョンはフェーズ 0 で決定).

### ディレクトリ構成 (案)

```
fp-examples/
├── package.yaml
├── stack.yaml          # resolver を独立指定
├── stack.yaml.lock     # コミット対象 (再現性のため)
├── .gitignore          # 「.gitignore 方針」セクション参照
├── README.md           # 例の追加手順, 動かし方
├── test/
│   ├── Fp3/
│   │   ├── ListBasics.hs
│   │   ├── BoolExercise.hs
│   │   └── ...
│   ├── Fp4/
│   │   ├── Total.hs
│   │   ├── FibGuard.hs
│   │   ├── Otherwise.hs
│   │   ├── Currying.hs
│   │   ├── FizzBuzz.hs
│   │   └── ...
│   ├── Fp5/
│   │   ├── DogAge.hs
│   │   ├── Pythagoreans.hs
│   │   └── ...
│   └── Spec.hs       # hspec-discover でサンプルを自動収集
└── (app/ は不要)
```

- 各サンプルは **独立した module** として記述し, シンボル衝突を回避.
- 期待出力との一致は `hspec` の `shouldBe` 系で確認.
- 例ごとに何を検証するかは「検証対象の分類」を参照.

### 検証対象の分類と扱い

検証粒度は以下の **3 層** とし, 例ごとに最低どの層まで保証するかを明示する.

| 層 | 内容 | 例 | 実現方法 |
| --- | --- | --- | --- |
| **L1: コンパイル可否** | 型エラーなくビルドできる | `data MyDogs = ...`, 単独関数定義 | モジュールを test-suite に含めれば自動 |
| **L2: 実行・出力一致** | `main` 相当を走らせて期待値が出る | `total`, `fizzBuzz`, `sign`, `area` | hspec `shouldBe`, `shouldReturn` |
| **L3: プロパティ検査** | 性質単位での網羅検証 | `length2 == length`, `mean` の不変量 | hspec + QuickCheck (任意) |

例ごとの扱い:

| カテゴリ | 例 | 保証レベル |
| --- | --- | --- |
| 完全プログラム | `total`, `fizzBuzz`, `sign`, `area` | L2 (出力 assert) |
| 純粋関数 (例題) | `mean`, `stddev`, `divisors`, `pythagoreans` | L2 + (任意で L3) |
| 型/データ断片 | `data MyDogs = ...`, レコード定義 | L1 のみ |
| ghci セッション | `ghci> :t add`, `:set -X...` | **検証対象外** (目視) |
| 故意のエラー | Stack overflow, 型エラー, Non-exhaustive | **検証対象外** (コメントで明示) |

### .gitignore 方針

リポジトリルート `.gitignore` は既に Haskell 標準成果物をカバー済みなので, fp-examples が新規に持ち込む除外項目は限定的. ただし「fp-examples を別リポジトリに切り出した場合でも壊れない」よう, `fp-examples/.gitignore` を **独立して** 配置する.

#### ルート `.gitignore` の既存カバレッジ (確認済み)

```
.stack-work/    # サブディレクトリにも有効 (パターンマッチ)
*.hi
*.o
*.hie
*.dyn_o
*.dyn_hi
.ghc.environment.*
dist
dist-*
.DS_Store
```

→ これらは fp-examples 配下でも自動的に除外される.

#### `fp-examples/.gitignore` で明示するもの

独立性確保と将来の切り出しに備え, 以下を明示する.

```gitignore
# Stack build artifacts (root の .gitignore でもカバーされるが明示)
.stack-work/
*.hi
*.o
*.hie
*.dyn_o
*.dyn_hi

# Cabal (将来 cabal で扱う場合に備え)
dist/
dist-newstyle/

# GHC env files
.ghc.environment.*

# Editor / IDE
.vscode/
*.swp
*~

# OS
.DS_Store

# hspec / tasty 実行時の一時ファイル (使う場合)
.hspec-failures
```

#### コミット **する** もの (誤って ignore しない)

- `stack.yaml`
- `stack.yaml.lock` (再現性のため必須)
- `package.yaml`
- `*.cabal` (`hpack` 生成だが, 再生成可なので任意. 競合回避のため通常はコミット推奨)
- `README.md`
- `test/**/*.hs` (全サンプル)

#### `hpack` 利用時の選択

`package.yaml` を真実の単一情報源として使う場合, 生成される `fp-examples.cabal` の扱いを決める:

- **コミットする**: ローカル `stack build` が `hpack` を持っていなくても動く. ただし `package.yaml` と `.cabal` が二重管理になる.
- **`.gitignore` に追加**: `.cabal` を生成成果物として除外. 全員が `hpack` を使う前提.

→ 推奨は **コミット** (低コストで CI 簡素化).

### CI 連携

- GitHub Actions に `fp-examples/` を `stack build && stack test` するワークフローを追加.
- PR で fp-examples が壊れたら lectures 側 PR も止める運用.

### マークダウンとの同期

初期は手動同期で運用する. 将来的に余裕ができたら以下を検討:

- `<!-- example-file: test/Fp4/Total.hs -->` のようなコメントアンカーをマークダウンに入れ, **CI で差分を検出する** ツールを追加.
- マークダウンから抽出するのではなく, ハスケル側を真実とし, マークダウンで `include` する仕組みも候補.

## ロードマップ

### フェーズ 0: PoC (半日 〜 1日) — **完了 (2026-05-12)**

- ✅ `fp-examples/` を作成. resolver `lts-22.43` (GHC 9.6.6) + 最小 deps (base, hspec).
- ✅ 代表 4 例 (`total` / `fib` / `sign` / `fizzBuzz`) を `test/Fp4/` 配下に `*Spec.hs` 形式で移植.
- ✅ `hspec-discover` で自動収集する構成.
- ✅ `stack test` で 14 examples 全合格を確認.
- ✅ blog 本体も同じ resolver `lts-22.43` に更新 (旧 `lts-20.10` から). `hakyll-4.16.3.0` snapshot 同梱版に切替済み.

### フェーズ 1: fp4.md 全面カバー (1-2 日) — **完了 (2026-05-12)**

- ✅ fp4.md の検証可能な全例を移植 (16 ファイル, 100 テストケース).
    - 関数と演算子の基礎 + 練習問題, 結合性練習問題, カリー化
    - fib (パターン/case/if 版), strHead, return10
    - describe (4 スタイル), length2, applyFToList
    - filter / foldl / zipWith / zip, 高階関数練習問題
    - ラムダ式練習問題, 関数合成, ローカル変数
    - 統計量 (mean/stddev/correlation), パーセプトロン
- ✅ 既存 Phase 0 と合わせて 100 examples 全合格.
- ✅ 既存バグ(`tatal`, `f(x-2)` 等)は事前に修正済みで, テストでも回帰防止.

### フェーズ 2: fp3.md / fp5.md (各半日 〜 1 日) — **完了 (2026-05-12)**

- ✅ fp3.md: ghci 中心のため練習問題ベースで 3 ファイル (数値型, リスト, 論理型) 計 19 ケース.
    - 論理型練習問題のテストは過去のバグ (回答例の False vs True) の回帰防止.
- ✅ fp5.md: データ型・代数的データ型の例で 7 ファイル 計 33 ケース.
    - Weekday + isWeekend + nextDay (列挙型練習問題)
    - リスト内包表記 (evens / pairs / divisors / pythagoreans)
    - DogAge + アクセサ関数 + 関数合成事例
    - Person + birthday + totalAge (レコード構文練習問題)
    - IntOrDog (直和型例)
    - Shape + area (直和型練習問題, ヘロンの公式)
    - DogInfo (部分関数アクセサの実行時エラー検証)
- ✅ 全体で **152 examples, 0 failures**.
- スマートコンストラクタ (Mult3 モジュール例) は単一ファイル構成と相性が悪いため**対象外**.

### フェーズ 3: fp6 / fp7 (各 1 日 〜)

- 代数構造・モナドが入るため依存が増える.

### フェーズ 4 (任意): CI 統合とマークダウン同期

- GitHub Actions 化.
- マークダウン⇔.hs の同期チェック.

## コスト見積もり

- **初期投資**: 3 〜 4 日 (フェーズ 0 〜 2 完了まで).
- **継続コスト**: 例追加ごとに 5 〜 15 分.
- **完全 1:1 同期を諦め, 主要例だけを選別すれば工数は半減**.

## 未解決事項 / リスク

1. ~~**resolver / GHC バージョンの選定**~~ → **解決 (2026-05-12)**: blog 本体・fp-examples ともに `lts-22.43` (GHC 9.6.6) に統一. `hakyll-4.16.3.0` (snapshot 同梱) で blog 動作確認済み.
2. **Hakyll との競合確認**: `src/Main.hs` の `match` パターン (Hakyll site rules) が `fp-examples/**/*.hs` を拾わないことを確認する必要あり. `publish.sh` の cleanup (`docs`, `_cache` 削除) にも fp-examples が巻き込まれないか確認.
3. **CLAUDE.md は `.gitignore` 対象**: 既存ルート `.gitignore` で `CLAUDE.md` は除外されており, 共有されない. 共有可能な「真実の単一情報源」としては `plans/README.md` を扱い, CLAUDE.md は所有者ローカル運用のリマインダーと位置づける.
4. **既存講義に未公開のバグが残っている可能性**: 移植中に追加発見される. 各章担当回ごとに修正 PR を別立てするか, fp-examples 立ち上げ PR に同梱するかの方針を決める.
5. **fp-examples 自体が肥大化したときの整理**: フェーズ 4 で抽出自動化を入れるか, あるいは「主要例のみ」と割り切るかの判断ポイント.
6. **GHC 拡張機能**: `OverloadedStrings`, `EmptyCase` 等がブロックごとに必要. ファイル先頭に集約する.
7. **テストフレームワーク選定**: `hspec` を第一候補とするが, `tasty`, `doctest`, `tasty-hunit` 等の代替も視野. フェーズ 0 の PoC で確定する.
8. **`haskell-language-server` 対応**: fp-examples 配下を HLS で開きたい場合, `hie.yaml` (multi-cradle) を用意するか, ルートと fp-examples で別エディタセッションにするかを決める.

## 関連ファイル

- `lectures/fp/fp3.md` 〜 `fp7.md`
- `stack.yaml`, `package.yaml` (blog 本体 — resolver `lts-20.10` を参考)
- `src/Main.hs` (Hakyll site — `match` パターンの確認対象)
- `.gitignore` (ルート — 既に Haskell artifacts をカバー)
- `publish.sh` (公開フロー — fp-examples を巻き込まないか確認)
- `CLAUDE.md` (個人ローカル運用ルール; 共有不可)
- `plans/README.md` (共有可能な真実の単一情報源)

## 変更履歴

- 2026-05-12 (初版): 提案作成.
- 2026-05-12 (再評価): 以下を反映.
    - Stack プロジェクト構成判断 (分離 vs 統合) を明示し, **分離** を推奨と決定.
    - 検証粒度を **L1/L2/L3 の 3 層** に整理.
    - `.gitignore` 方針セクションを新規追加 (fp-examples 配下に独立した `.gitignore` を置く方針).
    - 未解決事項に Hakyll 競合, CLAUDE.md 共有不可問題, テストフレームワーク選定, HLS 対応を追加.
    - 関連ファイルを精緻化.
- 2026-05-12 (Phase 0 完了):
    - resolver を blog 本体・fp-examples ともに `lts-22.43` (GHC 9.6.6) に更新. blog 本体のビルドは `hakyll-4.16.3.0` で成功.
    - `fp-examples/` ディレクトリを作成 (stack.yaml, package.yaml, .gitignore, README.md, test/Spec.hs).
    - `test/Fp4/{Total,Fib,Sign,FizzBuzz}Spec.hs` を実装. hspec-discover で自動収集.
    - `stack test` で 14 examples 全合格.
    - `CLAUDE.md` に「fp 講義編集時のルール」セクションを追加 (テスト環境との同期義務).
    - 状態を `proposed` → `in-progress` に更新.
- 2026-05-12 (Phase 1 完了):
    - fp4.md の検証可能な残り全例を `test/Fp4/` 配下に追加 (12 ファイル). 累計 16 ファイル 100 テストケース.
- 2026-05-12 (Phase 2 完了):
    - fp3.md: 数値型 / リスト / 論理型の練習問題を `test/Fp3/` 配下に追加 (3 ファイル, 19 ケース).
    - fp5.md: 列挙型 / リスト内包表記 / 直積型 (レコード) / 直和型 / 部分アクセサの各例を `test/Fp5/` 配下に追加 (7 ファイル, 33 ケース).
    - 部分アクセサの実行時例外は `RecSelError` であるため `anyException` で捕捉.
    - 累計 **26 ファイル 152 テストケース, 0 failures**.
