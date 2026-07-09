---
plan_id: fp8-structure-capability-table
status: landed
created: 2026-07-09
updated: 2026-07-09
priority: medium
next_actor: none
next_action: "承認 scope (P1+P2) を landed (commit bd58924). P3/演習は将来の別提案として繰延べ"
---

# fp8 に「構造 → 能力」対応表 + semiring/加重集計 を追補する

## メタ情報

- **状態**: proposed (handoff A の受信 → plan/proposed 化. ユーザ承認を経て fp8 本体へ反映)
- **作成日**: 2026-07-09
- **対象**: `lectures/fp/fp8.md` (代数とクラス / 代数のインスタンスにする利点) + `fp-examples/test/Fp8/`
- **由来**: cross-repo handoff `learn-2026-07-08T11-08-50…-圏論の能力対応表を-fp-講義資料へ反映し章構成を見直す` (from learn, class=substantive). learn repo の学習 note「圏論の工学的利点」Q&A で整理した「代数構造 → 工学的能力」の対応を fp 講義へ反映する依頼.
- **関連計画**: [fp-typeclass-monad-arc.md](../in-progress/fp-typeclass-monad-arc.md) (fp8 は同 plan の Phase 1 = 完了済み. 本提案はその**追補**), [fp-examples-verification.md](../in-progress/fp-examples-verification.md) (コード例の機械検証)

## 背景 (handoff の要旨)

learn note の Q&A は, 代数構造が生む「能力」を次のように整理していた:

- **モノイド** (結合的演算 + 単位元) → 畳み込みの能力 (`foldMap` で合計/連結)
- **可換モノイド** → 並列化・順序不同の能力 (map-reduce 型の分散集計が結果に効かない)
- **群** → 逆元による**取り消し**の能力 (トランザクション取消・差分計算)
- **semiring / module** → **加重集計**の能力 (加重平均・線形結合)

依頼: この「構造 → 能力」対応表 + Haskell 具体例を fp 講義に反映し, その進行順に沿うよう章立てを見直す.

## 現状分析 (fp8 は既に大半をカバー済み)

handoff は fp8 が現在どこまで書けているかを知らずに送られた (07-08). 実際の fp8 は Phase 1 完了済みで極めて充実しており, 上記の能力の多くは **既に実装済み**:

| handoff が挙げる能力 | fp8 の現状 | 判定 |
| --- | --- | --- |
| モノイド = 畳み込み | `## foldMap と Foldable` (`fold`/`foldMap`/`mconcat`, `Add`) | ✅ 収録済 |
| 結合律 = 並列化 (MapReduce) | `## 結合律と準同型が可能にすること` (分割統治・MapReduce) | ✅ 収録済 |
| 可換律 = 順序不同 | note「並列には結合律で足り, 並べ替えには可換律が要る」 | △ **note 内のみ**. 独立した能力階層としては未提示 |
| 群 = 逆元 | `## 群 (Group)` (`Z3` 時計群, `invert`) | △ 数学的定義はある. **「取り消し/差分」の工学的能力**としては未提示 |
| 差分集計 (incremental) | `### 差分集計` (`acc <> stats ys`) | ✅ ただし**モノイドの加算のみ** (群の減算=取消ではない) |
| 1 パス統計 (fusion) | `Moments` (件数・合計・二乗和で平均/分散) | ✅ 収録済 |
| **semiring / 加重集計** | — | ❌ **完全に未収録** |
| **体系的な「構造→能力」対応表** | 構造階層 note (材料と法則) はある | ❌ **能力軸の集約表は無い** |

**結論**: 真のギャップは (1) semiring/加重集計 の新規収録, (2) 能力軸の集約表, (3) 可換モノイドの階層明示, (4) 群の「取消/差分」フレーミング の 4 点. 「章の全面再編」は**不要** — 既存の `型クラス → 代数 (マグマ→半群→モノイド→ブール代数→群) → 利点` の流れが, そのまま「構造 → 能力」の進行になっている.

## 提案する変更 (優先度つき 3 パート)

### P1 (核): 「構造 → 能力」対応表を capstone として追加 — 低コスト・高価値

`# 代数のインスタンスにする利点` 章 (または `## 群` 節末) に, 散在している能力を 1 枚に集約する表を新設する. 既存の構造階層 note (材料と法則) と相補的に, **能力軸**でまとめる:

| 代数構造 | 足す法則/材料 | 生まれる「能力」 | fp8 の具体例 |
| --- | --- | --- | --- |
| 半群 | 結合律 | 分割統治・並列 (順序は保持) | `Max`, 文字列連結 |
| モノイド | + 単位元 | **畳み込み** (空も畳める) | `foldMap Add`, `Stats` |
| 可換モノイド | + 可換律 | **順序不同で畳める** (分散集計) | `Add`, `Stats`, `Moments` |
| 群 | + 逆元 | **取り消し・差分** | `Z3`, ℤ 上の `Sum` の減算 |
| 半環 (semiring) | 2 演算 (⊕,⊗) + 分配律 | **加重集計・線形結合** | 加重平均, ブール代数 (∨,∧) |

各行は本文の既存節へリンクし, 表を「章全体の地図」として機能させる.

### P2: semiring / 加重集計 の新節 — 新規収録 (`stimes` の一般化として接続)

`## stimes — 同じ演算を n 回` の直後 or `# 代数のインスタンスにする利点` 章内に新節を置く. 教育的な接続の勘所:

- **`stimes n x`** (既収録) は「モノイド元 `x` を `n` 回足す」= **ℕ でスケールする**操作. これを重み `n` を実数へ一般化すると **加重集計** になる.
- **半環 (semiring)**: 同じ台集合に 2 つの演算 — 可換モノイド `(S, ⊕, 0)` と モノイド `(S, ⊗, 1)` — があり, ⊗ が ⊕ に分配し `0` が吸収元. 例: `(ℤ, +, ×)`, **ブール代数 `(Bool, ∨, ∧)`** (fp8 で既出のブール代数がそのまま半環の例になる = 既存節の伏線回収).
- **能力 = 加重集計**: 各観測を `w ⊗ x` (重み × 値, 半環の ⊗) で持ち上げ, `⊕` で畳む. 加重平均・線形結合・内積がこの形.

具体例 (新規完全プログラム, 要 spec):

~~~ haskell
-- 加重平均: (Σ w, Σ w*x) を 1 つのモノイドに束ねる (Moments と同じ直積の発想)
data WMean = WMean { wTotal :: Double, wxTotal :: Double } deriving (Show, Eq)

instance Semigroup WMean where
  WMean w1 wx1 <> WMean w2 wx2 = WMean (w1 + w2) (wx1 + wx2)
instance Monoid WMean where
  mempty = WMean 0 0

-- (重み, 値) -> 1 点分.  w * x に半環の ⊗ (重み付け) が効く
weighted :: (Double, Double) -> WMean
weighted (w, x) = WMean w (w * x)

wmean :: WMean -> Double
wmean (WMean w wx) = wx / w

-- foldMap weighted [(2,10),(1,4),(1,2)] = WMean 4 24,  wmean = 6.0
~~~

ポイント: **畳み込み自体はモノイド** (`WMean` = 2 つの `Sum` の直積) だが, 各要素の `w * x` に**半環の乗法**が効いている. これが「semiring = 加重集計」の具体. `stimes` (整数の重み) との連続性も 1 文で結ぶ.

### P3 (任意): 群節に「取り消し・差分」の工学的能力を 1 節追記

現状の `## 群` は `Z3` 時計群で逆元を数学的に導入するのみ. 「逆元があると **打ち消せる**」という能力を工学例で足す:

- `x <> invert y` = 「`y` の寄与を取り消す」. ℤ 上の合計 (`Sum Integer`) は群なので, いったん足した取引を後から**減算で取り消せる** (モノイドの差分集計は加算のみ = 履歴を消せない, との対比).
- 応用: running total からの項目削除, sliding-window 合計, undo. 既存 `### 差分集計` (モノイド, 加算のみ) の**次の段**として位置づく.

## 章構成の判断

- **全面再編はしない**. 既存 flow が既に構造→能力の進行. 追補は既存章の中に差し込む.
- **肥大化への配慮**: fp8 は現時点で最長 (~80KB). P1 (表) は軽いが, P2 (semiring) を足すとさらに伸びる. 分量が過大と判断されれば, semiring/加重集計を **fp8 末尾**でなく **後半の補足章** (交換代数と同様の「発展」枠) へ逃がす選択肢もある → **要ユーザ判断** (下記).
- 半環はブール代数 (既出) を例に使えるため, 収録位置はブール代数節と群節の間 (階層の自然な延長) も候補.

## fp-examples への影響 (CLAUDE.md fp ルール)

- P2 で新規の完全プログラム (`WMean` 等) を掲載する → **対応 spec `fp-examples/test/Fp8/WeightedMeanSpec.hs` (仮) を新規作成**が必須 (CLAUDE.md「fp 講義編集時のルール」#1/#3).
- P1 (表) は本文のみ・コード追加なし → spec 不要.
- P3 で `Sum Integer` の減算例を完全プログラム化する場合は spec を 1 本追加.
- 変更後 `cd fp-examples && stack test` を実行し全緑を確認してから完了 (#2).

## 未解決事項 / 要ユーザ判断

1. **scope**: P1 のみ (集約表だけ, 最小) / P1+P2 (表 + semiring, 推奨) / P1+P2+P3 (全部) のどれで進めるか.
2. **semiring の配置**: fp8 内 (ブール代数〜群の間 or 利点章) か, 別の補足「発展」章へ分離か (肥大化次第).
3. **演習の要否**: P2/P3 に `### Exercise CH8-K` 形式の演習を新設するか (現状 CH8-1〜6). 追加するなら連番と配置を確定.

## 検証手順

1. fp8 本文を追補後, `cd fp-examples && stack test` で全緑 (現状 362 examples から, 追加 spec 分だけ増える想定).
2. サイトビルド (`stack build && stack exec site build` 相当) で KaTeX 数式 (`\otimes`/`\oplus` 等) が壊れないこと・目次に新節が出ることを確認.
3. 他章参照は `[第N章](fpN.html)` リンク形式に統一 (既存規約).

## 変更履歴

- 2026-07-09: 作成. handoff A (learn→haskell-blog, 圏論の能力対応表) を plan/proposed 化. fp8 の現状分析で「大半カバー済み・真のギャップは semiring/集約表/可換モノイド明示/群の取消フレーミング」と判定し, 全面再編でなく追補として設計. scope/配置/演習を要ユーザ判断として残置.
- 2026-07-09: ユーザ承認 (scope=P1+P2, semiring は fp8 内 `stimes` 直後) → **landed (commit bd58924)**. fp8「代数のインスタンスにする利点」章に `## 半環 (semiring) と加重集計` (WMean 加重平均) と `## 構造と能力の対応 (まとめ)` 表を追補. `fp-examples/test/Fp8/WeightedMeanSpec.hs` (8 tests) 追加, `stack test` 488 examples green, pandoc パース OK. **P3 (群の取消・差分 独立節) と新規演習 `### Exercise CH8-K` は繰延べ** — 必要になれば別提案として起こす. status: landed.
