---
plan_id: fp-typeclass-monad-arc
status: in-progress
created: 2026-06-15
updated: 2026-06-21
priority: medium
next_actor: agent
next_action: "Phase 2 以降 (fp9〜fp11) の章執筆 — 型クラス→代数→多相データ型→モナド→効果の流れで fp9 から"
---

# fp 後半 (型クラス→代数→多相データ型→モナド→効果) の章構成と執筆計画

## メタ情報

- **状態**: in-progress (Phase 0 完了 / Phase 1 = fp8 完了 / Phase 2 以降 未着手)
- **作成日**: 2026-06-15
- **最終更新**: 2026-06-19
- **対象**: `lectures/fp/fp8.md`〜`fp11.md` (および fp7 からの接続)
- **関連計画**: [fp-examples-verification.md](fp-examples-verification.md) (コード例の機械検証), [teaching-reification-cognitive-skills.md](teaching-reification-cognitive-skills.md) (認知スキルの可視化方針)

## 概要

fp 講義後半 (fp8〜fp11) を **「型クラス → 代数構造 → 多相データ型と圏論的構造 → モナドと IO → 可変状態と効果」** という一直線の流れで構成する. あわせて, これまで複数の章から「後の章で扱う」と先送りされてきた 3 つの宿題 — **(a) `deriving` の中身 (型クラスのインスタンス宣言)**, **(b) `Maybe` / `Either` (多相な直和型)**, **(c) `IO` / `State` / `ST` (副作用・可変状態)** — の回収先を確定する.

## 動機

調査により, 現行教材には次の構造的ギャップと「張られた伏線」があることが判明した.

### 先送りされている宿題 (伏線)

| 宿題 | 予告箇所 | 回収先 (本計画) |
| --- | --- | --- |
| `deriving Show` の中身 | fp7:95「詳細に関しては後ほど扱います」 | **fp8** Show 節 |
| `Maybe` (失敗を型で表す) | fp4:538/817, fp7:182/602/1018「後の章で扱う `Maybe`」 | **fp9** 多相データ型 |
| `Either` | (全 fp で未登場) | **fp9** 多相データ型 |
| `IO` とは何か / 副作用を型で分ける仕組み | fp1:434/600「具体的方法は後の章の `IO`」「モナドの仕組みを通す」, fp2:190「`IO ()` の詳細は省く」 | **fp10** 入出力 (IO) |
| `State` / `ST` (再代入・可変状態) | fp5:1074「後にでてくる `State` や `ST`」 | **fp11** 可変状態と効果 |

### 前提となる未導入概念

- **型引数を持つデータ型 (多相データ型)** が, fp5〜fp7 のどこでも正式導入されていない. `Maybe`/`Either` を扱うにはこの節が前提になるため, fp9 の冒頭に置く.
- **`do` / `main` / `putStrLn`** は fp2 (おまじない) と fp4〜fp7 で**実用的に多用済み**だが「なぜそう書くか」は未説明. fp10 はこの "種明かし" に徹する (新しい使い方の導入ではなく概念の回収).

## 設計

### Maybe / Either の配置 = 案A (fp9 集約)

検討した 3 案のうち **案A** を採用 (会話で確定):

- **案A (採用)**: `Maybe`/`Either` を fp9「多相データ型」で正式初出させ, Functor/Monad への土台にする. fp8 は自作型＋リスト (既習) で型クラス・代数を完結させ, `Maybe` を必要としない.
- 案B (不採用): fp8 冒頭に最小導入しインスタンス題材に流用 → fp8 が過積載.
- 案C (不採用): fp7 直和型直後に移設 → fp7 が肥大, かつ fp7 は明示的に「後の章」と先送りしている設計意図に反する.

### IO / ST の配置

- **IO (アクション・副作用)** は典型的なモナドであり, Monad を導入する文脈でしか formal に説明できない. → **fp10** (Monad の章) の後半に「入出力 (IO)」として置き, fp1/fp2/fp4 の伏線を回収.
- **State / ST / IORef** は Monad と IO の理解を前提とする発展トピック. ST の `runST` と領域型 `s` (ファントム型) は型システムの高度な応用. → **fp11** (可変状態と効果). fp5 の伏線を回収.
- Monad は分量が大きく Functor とテーマが分かれるため, fp9 に同居させず **fp10 を独立章** とする.

### 章ごとの責務 (確定)

| 章 | タイトル | 主題 | 回収する伏線 |
| --- | --- | --- | --- |
| fp7 | 代数的データ型1 | 集合論的解釈 (列挙・直積・直和・**再帰的データ型**・type/newtype) | — (既存) |
| fp8 | 代数的データ型2 | 型クラスと代数 (class/instance, Show/Eq/Ord/Enum/**Num**, 半群/モノイド/群) | (a) deriving の中身, 数値リテラル=`fromInteger` (fp3:382/fp4) |
| fp9 | 代数的データ型3 | 多相データ型と圏論的構造 (型引数, Maybe/Either, Functor/関手, 自然変換) | (b) Maybe/Either |
| fp10 | モナドと入出力 | Monad (>>=/do/Maybe・Either モナド/モナド則), IO (副作用を値として) | (c) IO |
| fp11 | 可変状態と効果 | IORef, State, ST (runST/領域型 s), Reader/Writer・モナド変換子 (概観) | (c) State/ST |

### スケルトン (Phase 0, 整備済み) の見出し構成

各 `.md` の H1/H2 見出しは整備済み. 詳細は各ファイル参照. 設計上のキーポイント:

- **fp8**: `Eq` を明示追加 (ご提示は Show/Enum/Ord だが `Ord` は `Eq` を superclass に要求するため Eq→Ord 順が必要). モノイドの正準例は**リスト** (`++` と `[]`, fp1-6 で既習) にして Maybe 非依存で完結. ブール代数は (∧, 単位元 `True`) と (∨, 単位元 `False`) の **2 つのモノイドが対になる例** として代数節に新規執筆 (2026-06-19. 旧 skeleton の「PDFのブール代数の例」TODO を回収. `All`/`Any` newtype で実装). **交換代数は fp8 から分離**し, かなり後半の補足的な独立章で扱う (ユーザ指示 2026-06-19. 旧 skeleton の「# 発展:交換代数」見出しは fp8 から削除済み. 別repo の ExchangeAlgebra ライブラリと対応).
- **fp9**: 既存の著者ノート (圏論的定義: データ型=対象, 関数=射, 関手=Functor, 多相型=自然変換) を温存し, 「多相データ型」節 (型引数→Maybe→Either→基本操作) を前段に追加.
- **fp10/fp11**: 伏線回収の対応関係を各ファイル冒頭のノートに明記済み.

## ロードマップ

- **Phase 0 — 全章スケルトン整備** ✅ 完了 (2026-06-15)
  - fp8/fp9/fp10/fp11 の見出し (タイトル) を本計画に沿って整備. 章ナビ (`previousChapter`/`nextChapter`) を fp7→…→fp11 で接続.
- **Phase 1 — fp8 本文 (型クラスと代数)** ✅ 完了 (2026-06-19)
  - ✅ 本文執筆: 型クラス (型クラスとは / Show / Eq / Ord / Enum・Bounded) + 代数とクラス (マグマ / 半群 / モノイド / ブール代数 / 群). ※ リスト / ツリー / ネットワークは 2026-06-20 に fp9 へ移設 (下記).
  - ✅ 演習 CH8-1〜6 を**各対応節の直後**に配置 (ユーザ指示 2026-06-19. 章末一括でなく節ごと. 文書順に連番を振り直し. CH8-2/3 はエラー修正演習). 配置: Show→CH8-1・CH8-2 / Eq→CH8-3 / Ord→CH8-4 / Enum・Bounded→CH8-5 / モノイド→CH8-6.
  - ✅ `fp-examples/test/Fp8/*Spec.hs` を 17 本新規作成し `cd fp-examples && stack test` グリーン (274 examples, 0 failures). 教材との不一致なし.
  - ✅ 他章参照を `[第N章](fpN.html)` リンク形式に統一 (ユーザ指示).
  - 交換代数は本章から分離 (上記「スケルトン … fp8」参照).
  - ✅ (2026-06-20) 全称・存在命題 (∀/∃): **導入は [第7章](fp7.html) へ** (述語論理 $\{x\mid p(x)\}$ の直後に小節「述語と量化」を新設. 連続性重視. ついでに部分集合 $\subset$ と集合の等しさ $=$ も ∀ で厳密化). fp8 側は Eq/Ord の則を ∀ つきで集合論的に書き直し, **各代数定義 (半群=$\forall x,y,z$ / モノイド=$\exists e\forall x$ / 群=$\forall x\exists y$) で ∀/∃ を直接使用** (量化子の順序の差は群の定義で対比. 当初は群節末の summary note にまとめていたが, ユーザ指示で各定義へ分散し summary note は撤去). リスト/ツリー/ネットワークの 3 節は fp9 へ移設 (下記). 末尾は fp9 への前送りで締め.
  - ✅ (2026-06-20) コラム「**法則を Haskell でどう守るか — QuickCheck**」を群節末に追加 (半群 warn の伏線回収). 結合律・単位元律を property として書き「テストであって証明ではない」点・`quickcheck-classes` も明記. fp-examples に **QuickCheck 依存 + `Fp8.LawQuickCheckSpec`** を追加し property を実走 (`stack test` 285 examples green).
- **Phase 2 — fp9 本文 (多相データ型と圏論的構造)** ✅ 初稿完了 (2026-06-20)
  - ✅ 章導入 + 圏論対応表, 型引数を持つデータ型 (Box/Pair/種 `* -> *`), リスト/ツリー (fp8 から移設) + **Map** (ネットワークを差し替えて新設), Maybe (fp4/fp7 の伏線回収), Either, 基本操作 (`maybe`/`either`/`fromMaybe` 等), 圏 (対象・射, モノイドとの対応), Functor/fmap (移設 Tree も関手化), 自然変換 (`forall a. f a -> g a` を [第7章] の ∀ に接続). 演習 CH9-1〜5 を各対応節の直後に配置 (文書順に振り直し).
  - ✅ fp-examples に Fp9 spec (Maybe/Either/基本操作/圏/TreeFunctor/自然変換/Map/演習×5) + 移設分 (ListMonoid/TreeMonoid). `stack test` 345 examples green (containers 依存追加). `open: true`, サイトビルド成功.
  - 残課題: ユーザ推敲, fp4/fp7 の Maybe 脚注→fp9 リンク. (Data.Map は本章に組込み済み.)
- **Phase 3 — fp10 本文 (モナドと入出力)** 未着手
  - Applicative→Monad→do の脱糖→Maybe/Either モナド→モナド則, IO モナド (アクションと参照透過性), 標準入出力, main の構造.
  - IO を含む例の検証方針は要検討 (下記リスク).
- **Phase 4 — fp11 本文 (可変状態と効果)** 未着手
  - IORef, State, ST (runST/領域型 s/STRef), Reader/Writer・モナド変換子の概観.

各 Phase 完了時に演習を `### Exercise CHN-K` 形式 (CLAUDE.md の演習見出し規約) で追加する.

## コスト見積もり

- Phase 0: 完了 (軽).
- Phase 1〜4: 各章, 本文 + コード例 + 演習 + fp-examples spec で 1〜数セッション規模. fp10 (モナド) と fp11 (効果) は概念難度が高く, fp8/fp9 より重い見込み.

## 未解決事項 / リスク

1. **章タイトルの命名方針**: fp7-9 は「代数的データ型1/2/3」の系列名を維持 (ユーザ指示). 新規の fp10/fp11 は内容に即した記述的タイトル (「モナドと入出力」「可変状態と効果」) を採用した. 系列名 (「代数的データ型4/5」) に揃えるか, fp8/fp9 も記述的タイトルに改名するかは未確定. → **要ユーザ判断**.
2. **Monad の置き場所**: 本計画は fp10 を独立章とした. fp9 が薄くなる場合は Functor+Monad を fp9 に寄せ IO 以降を fp10 にする折衷も可 (会話で言及済み).
3. **fp11 の nextChapter**: 現時点の終端章として `nextChapter` を省略した. fp12 以降を作る際に追記する.
4. **IO/ST 例の機械検証**: fp-examples は純粋関数の評価検証が中心. IO アクションや `runST` を含む例は `hspec` での検証方法 (期待出力のキャプチャ等) を別途設計する必要がある. fp-examples-verification.md 側との調整が必要.
5. **型引数 (多相) の導入深度**: fp9 で初めて多相データ型を扱うが, 多相「関数」(fp5/fp6 で未導入) との関係をどこまで遡って説明するか要検討.

## 関連ファイル

- `lectures/fp/fp7.md` — 接続元 (代数的データ型1)
- `lectures/fp/fp8.md` — 型クラスと代数 (スケルトン整備済)
- `lectures/fp/fp9.md` — 多相データ型と圏論的構造 (スケルトン整備済)
- `lectures/fp/fp10.md` — モナドと入出力 (新規, スケルトン整備済)
- `lectures/fp/fp11.md` — 可変状態と効果 (新規, スケルトン整備済)
- `fp-examples/` — コード例の機械検証プロジェクト (`test/Fp8`/`Fp9` 以降を追加予定)
- `plans/in-progress/fp-examples-verification.md` — 検証基盤の計画

## 変更履歴

- 2026-06-15: 作成. 案A (Maybe/Either→fp9) と IO→fp10 / State・ST→fp11 を確定. fp8-fp11 のスケルトンを整備 (Phase 0 完了).
- 2026-06-19: Phase 1 完了. fp8 本文を執筆 (型クラス + 代数とクラス + 演習 CH8-1〜6). ユーザ判断で交換代数を fp8 から分離し後半の補足章へ, ブール代数を 2 モノイドの例として新規執筆. fp-examples に Fp8 spec 17 本を追加し `stack test` グリーン (274 examples, 0 failures). 他章参照を `[第N章](fpN.html)` リンク形式に統一.
- 2026-06-19: fp8 改稿 (ユーザ指示). (1) 演習を章末一括から各対応節の直後へ移し連番を振り直し. (2) ブール代数を組込 `Bool` でなく自作列挙型 `Bool2 = T | F` で再構成. (3) 代数節全体で各実装に数式 (法則) を併記し, 新規 ASCII 2 項演算子 (`|*|` `|+|` `.&.` `.|.` `.+.` `.*.`) を定義して `(<>) = 演算子` で数式↔コード対応を明示 (Unicode は不使用). Fp8 spec を同期し `stack test` グリーン (283 examples, 0 failures). 体裁は [[fp-algebra-example-style]] に記録.
- 2026-06-20: fp8→fp9 再編 (ユーザ指示). リスト/ツリー/ネットワークの 3 節を fp8 代数節から fp9「多相データ型」へ移設 (案(b): 多相型かつモノイドとして bridge intro 付き). 併せて fp8 に全称・存在命題 (∀/∃) を導入 (代数階層の note + Eq/Ord の則に一言, 「中」案). 対応 spec を Fp8→Fp9 へ rename・移動し `stack test` グリーン (283 examples). fp8 は型クラス + 代数階層 (マグマ〜群) で完結し, 末尾に fp9 への前送りを置いた.
- 2026-06-20: ∀/∃ の導入位置を見直し (ユーザ指示). 連続性のため **導入を fp7** (述語論理の直後の新小節「述語と量化」) へ移し, 部分集合・集合の等しさも ∀ で厳密化. fp8 は Eq/Ord の則を ∀ つきの集合論的形に書き直し, 群節末 note を「fp7 で導入した ∀/∃ の適用」に再フレーム. さらに **QuickCheck コラム**を fp8 群節末に追加し, fp-examples に QuickCheck 依存 + `Fp8.LawQuickCheckSpec` (結合律・単位元律の property) を追加して実走 (`stack test` 285 examples, 0 failures). fp7 はコード未変更のため Fp7 spec への影響なし.
- 2026-06-20: ∀/∃ を群節末の summary note でまとめる形から, **半群・モノイド・群の各定義で直接使う**形に変更 (ユーザ指示). 結合律=$\forall x,y,z$, 単位元=$\exists e\forall x$ ($\exists$ が外), 逆元=$\forall x\exists y$ ($\exists$ が内) をそれぞれの定義式として記述し, 量化子の順序の対比は群の定義に統合. 群末の ∀/∃ summary note は撤去 (階層まとめ note・束の発展 note・QuickCheck コラムは存置). 束 (lattice) の定義 note も meet/join + 結合・交換・吸収律 / 順序による下限・上限の両定義 + 有界・分配・可補→ブール代数 まで詳述. コード非関与で `stack test` 285 のまま.
- 2026-06-20: 「**代数のインスタンスにする利点**」節を fp8 群末 (QuickCheck コラムの後) に追加 (ユーザ指示, 候補提示→全採用). foldMap/Foldable (`foldMap` が Monoid を要求) と stimes を実例つきで詳述, 結合律→並列 (MapReduce) を概念で, `Data.Map` の `unionWith (<>)` (値が Semigroup) と Writer モナドは予告に留める (Map は導入前のため [第9章] で扱う方針). - 2026-06-20: fp9 初稿を作成 (blog-author-jp に委譲 → 私がレビュー). 全節 (多相データ型 / Maybe / Either / 基本操作 / 圏 / Functor / 自然変換) + 演習 CH9-1〜5 を執筆, Fp9 spec 11 本で検証 (`stack test` 345 green), `open: true`, サイトビルド成功. レビュー修正: 演習を章末一括→各節直後へ分散 (CH9-1〜5 振り直し, spec Ex93/94/95 も対応), 教材の `describe`→`describeMaybe` (hspec 衝突回避 + spec verbatim 化), 「圏 = 対象 1 つのモノイドの一般化」の表現を厳密化.
- 2026-06-20: fp9 の多相型実例を **ネットワーク → Map に差し替え** (ユーザ選択, list/tree/Map の 3 点構成). ネットワーク (代数的グラフ) 節と `Fp9.GraphSpec` を削除し, `## Map — キーと値の対応` を新設 (型引数 2 つ, lookup→Maybe, 左偏 union の Monoid, `unionWith (<>)`/`fromListWith` で値結合 = fp8 Map 予告の回収, 値への fmap). `Fp9.MapSpec` + containers 依存追加, fp8 末尾の forward-pointer から「ネットワーク」を除去. `stack test` 345 green, サイトビルド成功.
- 2026-06-21: **fp8 完成度・整合性 audit** (Claude + codex クロスチェック). 検出 5 件を修正: (1) `:1038` 「本章/第7章で自作した木」→ 木は fp9 初出のため「次章 fp9 で自作する木」に修正 (両者検出), (2) foldMap/stimes/QuickCheck コード片の前提 (Add + `Data.Foldable`/`Data.Semigroup` import + `Arbitrary Add`) を明記, (3) `instance Num Nat` の `fromInteger` を負値で `error` に統一 (「負は表せない」と整合. NumNatSpec に負値 throw テスト追加), (4) `Max` を `Int`→`Integer` (ℤ に忠実かつ「最小元なし=単位元のない半群」の真の例に. minBound 文を書換, SemigroupMaxSpec 同期), (5) Fp8 spec 冒頭コメントの演習番号修正 (DirectionTurn CH8-5/Medal CH8-4/Stats CH8-6). `stack test` 355 green.
- 2026-06-21: **fp8 に `## Num — 数値リテラルの正体` を新設** (ユーザ提案→評価→codex クロスチェック→承認). 数値リテラル `3` = `fromInteger 3 :: Num a => a` の種明かし (fp3:382「自作型にリテラルを定める話は扱わない」+ fp4 の `No instance for Num` 伏線回収). 位置 = Enum・Bounded の後・`# 代数とクラス` の直前. fp7 の `Nat` に `instance Num Nat` を**デモ**として与え `3 :: Nat` を実現 (`fromInteger`/`(+)`/`(*)` 主役, `negate = error` で「ℕ は加法逆元なし→群でない」を群節へ1文接続. fp7 Nat 節にも前方ポインタ). **codex 指摘で `Max` は Nat 化せず Int 据置** (max on ℕ は Zero を単位元に持ち「半群だが単位元なし」の例を弱めるため). 数学的厳密化として **`Add`/`Mul` を `Int`→`Integer`** (有界 Int でなく ℤ に忠実. codex 提案). 全面 Nat 化は不採用 (実用性・spec チャーン). Fp8 spec を Integer 化 (Monoid/InstanceBenefits/LawQuickCheck) + `Fp8.NumNatSpec` 追加, `stack test` 354 green.
- 2026-06-21: **再帰的データ型を fp7 に新設** (ユーザ検討依頼→評価→承認). 再帰 (自己参照) と多相 (型引数) を軸で分割: fp7 は直和型の直後に `## 再帰的データ型` を新設し **単相の `Nat = Zero | Succ Nat`** (ペアノ自然数, `toInt`/`add`) で「型が自分を参照する」概念・再帰関数での処理・帰納的集合の見方を導入. リスト `[] | x:xs` が再帰型である事実も明かす. fp7:334 のリスト先送り note を本節+第9章への参照に更新. fp9 の Tree 導入を「第7章の再帰型 `Nat` に型引数を載せて多相化」と接続. `fp-examples/test/Fp7/NatSpec.hs` を追加 (`stack test` 349 examples green). 多相な再帰型 (`Tree a`/`[a]`) と代数構造は fp9 のまま (案C の Maybe→fp7 とは別件: 再帰は fp7 のスコープ内で重複も生じないため採用).
- 2026-06-20: fp8 の `Fp8.InstanceBenefitsSpec` (foldMap/fold/stimes を実走) を追加し `stack test` 291 examples green. その後ユーザ指示で, この節を **トップレベル `#` に昇格** (型クラス / 代数とクラス と並ぶ第3章節) し foldMap/stimes/並列/予告 を `##` 小節化 (目次対応), fold/foldMap のシグネチャのコメントは各関数の上に改行配置, foldMap 説明は「コンテナ概念の導入 + fold との差を [第6章] の畳み込みからの一般化として」加筆.
- 2026-06-24: fp9 Functor 節に改善メモ 3 件を反映 (inbox triage 2026-06-24, task d1c44cdd0; Todoist 6gwWm2J.../6gwWm34.../6gwWpPj...). (1) `fmap` を「[第4章] の `map` を任意の関手に一般化したもの」として明示 (`map :: (a->b)->[a]->[b]` と `fmap :: (a->b)->f a->f b` を `\underbrace` で対比する数式 + 「入れ物の形を保つ map」という言い換えを追加). (2) Functor の実例順を Maybe→**Either**→リスト→Tree に並べ替え (失敗系 2 型 Maybe/Either を隣接させ, リスト/Tree を後段に). (3) リストの `fmap` 実装例 `fmapList` (= `map` の再帰定義そのもの) を新設し `fmap`/`map` と一致することを確認. `Fp9.TreeFunctorSpec` を同期 (`fmapList` 定義 + 3 ケース追加, describe を Maybe/Either/リスト 順に整列), `cd fp-examples && stack test` = 362 examples / 0 failures. サイトビルド成功 (KaTeX 数式 intact, `\\[` 衝突なし). fp10/11 は対象外 (ユーザ確認済).
