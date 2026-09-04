---
title: exchangealgebra 0.5.0.0 と 0.4 系からの変更点
description: Haskell ライブラリ exchangealgebra の 0.5.0.0 を Hackage に公開しました. 0.4 系からの差分を, 値型の選択, 勘定科目の意味情報と転記の権限, 評価勘定の扱い, 試算表から財務諸表までの層, シミュレーション front-end (Lite) とスピル検証, モジュール境界の整理, 移行手順の順に紹介します.
tags:
    - haskell
    - hackage
    - exchangealgebra
    - accounting
    - simulation
    - release
featured: true
category: research
date: 2026-09-04
tableOfContents: true
open: true
---

# はじめに

Haskell ライブラリ [`exchangealgebra`](https://hackage.haskell.org/package/exchangealgebra) の 0.5.0.0 を 2026-09-04 に Hackage へ公開しました. [前回の記事](2026-05-26-exchangealgebra-on-hackage.html)で告知した 0.4.0.0 (2026-05-18) から約 3 か月ぶりの major 版で, その間に minor の 0.4.1.0 と patch の 0.4.1.1, 0.4.1.2 を出しています. 破壊的変更は 1 つの major に集約する方針を採っているので, 0.5.0.0 には 3 か月分の API 変更がまとめて入っています. 本記事はライブラリ API の差分を 0.4 系の利用者向けに整理します (同梱 examples の監査評価ツール群の変更は扱いません). 代数構造そのもののチュートリアルは前回の予告どおり関連論文の正式公表後に別記事とします.

- Hackage: <https://hackage.haskell.org/package/exchangealgebra-0.5.0.0>
- GitHub Release: <https://github.com/yakagika/ExchangeAlgebra/releases/tag/v0.5.0.0>
- ChangeLog (全項目): <https://github.com/yakagika/ExchangeAlgebra/blob/master/ChangeLog.md>

# 0.4 系の patch で直したこと

0.5.0.0 の差分を読む前提として, 0.4.1.x の 3 つの修正はすべて 0.5.0.0 に含まれています. 0.4.1.1 の修正は値の総量 (`norm`) が保たれたまま基底ごとの内訳が壊れる型の不具合で, 総量だけを見るテストでは検出できませんでした.

- **0.4.1.0**: `bases` が Not 側の列を無視して Hat 側を 2 回走査していました. 同時に, `bar` や `balance` の一致判定を固定の絶対許容誤差からスケール付きの許容誤差に変えました.
- **0.4.1.1**: 値 0 の単項と非 0 の単項を `.+` で結合すると, 非 0 の値が 0 側の基底に付け替えられていました. 明示的に `0 :@ base` を作る疎な入力係数を持つシミュレーションで, 累積順序によって隣の基底に転記が漏れ出し, 同梱の例では長期の run で企業のストックが無視できない幅でずれていました.
- **0.4.1.2**: 貸借が完全に一致した元帳に `incomeSummaryAccount` を適用すると, 純損益 0 のケースを場合分けが取りこぼしてクラッシュしていました.

# 値型を選べるようにした

0.4 系では `Alg v b` の値型 `v` は事実上 `Double` に固定されていました. `HatVal` クラスが `RealFloat` を上位クラスに持っていたためで, 正確な 10 進数型を値型にする道がありませんでした. 0.5.0.0 では `HatVal` から `RealFloat` を外し, 代わりに表示用の `showValue` メソッドを加えました. これが今回の major の出発点です.

その上で `ExchangeAlgebra.Value` に 2 つの newtype を用意しました. **`MoneyDouble`** は `Double` の zero-cost wrapper で, 係数や乱数と同じ型のまま金額を扱う事故を型で防ぎます. **`MoneyDecimal`** は 10 進固定小数 (`Data.Decimal` を包む) で, 加算が正確かつ結合的なので, 転記を足し合わせる順序が `norm` や `bar` の結果に影響しません. 非負性は `Double` と同じく型では保証せず, 負の転記を `.@` が拒否する形で保ちます. 数値リテラルはどちらでもそのまま書けます.

```haskell
import ExchangeAlgebra
import ExchangeAlgebra.Value (MoneyDecimal)
import ExchangeAlgebra.Algebra.Base

type Ledger = Alg MoneyDecimal (HatBase AccountTitles)

entry :: Ledger
entry = 10.5 .@ Hat :< Cash .+ 2 .@ Not :< Sales .+ 0.1 .@ Hat :< Cash

-- norm entry == 12.6
-- bar  entry == 10.6:@Hat:<Cash .+ 2:@Not:<Sales
```

`Double` では 10.5 + 0.1 のような加算が最下位ビットの丸めを含み, 同じ基底の転記を足す順序が変わると `norm` の末尾桁が動きます. 同梱のシミュレーション例では, 短い run では両型の結果が一致し, 期を重ねると最下位桁で分かれます. 正確さの代償として `MoneyDecimal` は `MoneyDouble` より遅く, メモリも多く使います. 用途で選ぶ前提であり, 既定は従来どおり `Double` です. 期数と規模に対する具体的な計測は別記事で扱います.

乗除算が要る場面 (税率, 按分) では `MoneyDecimal` の中間値は正確なまま保たれ, 金額を確定する時点で `bankersRound` (銀行丸め) か `ceilingRound` を明示的に呼びます. 丸め規則は法域や会社で異なるため, ライブラリ側で 1 つに固定していません.

シミュレーションでは, パラメータ・投入係数・乱数は `Double` のまま持ち, 元帳に入る時点で `realToFrac` で変換し, 集計値を可視化へ戻すときに `Double` に戻す境界パターンを推奨しています. 同梱の簿記例とシミュレーション例はこの形に揃えました. Leontief 逆行列や最適化のような数値計算主体の例は `Double` のままです.

# 勘定科目の意味情報と転記の権限

0.4 系の勘定科目 `AccountTitles` は列挙型と, 5 区分 (資産・負債・資本・費用・収益) やサイドを返す少数の関数だけを持っていました. 0.5.0.0 では科目ごとの正本となる **registry** を置き, 日商簿記 2 級・3 級の商業簿記の科目を取り込んで 235 の具体科目に拡張しました. 各科目には **`AccountSemantics`** として, 科目の役割, 転記できる文脈 (後述), 旧来の 5 区分が何を意味しているか, ホームサイド, 財務諸表への表示可否が付きます. `IncomeSummary` の区分が `Assets` になっているような, 方向の符号化を区分に流用していた箇所は, 分類ではなく符号化として説明されるようになりました.

外部データからの転記を安全にする層も追加しました. `ExchangeAlgebra.Convert` は借貸・科目名・金額の生データと代数の項を相互変換し, `Convert.Csv` は固定 schema の仕訳 CSV を読み, `Convert.Checked` は科目名の解決と転記の妥当性を検査してから `Journal` を作ります. 日商簿記の B 欄にある「未払金」「借入金」「仮払金」「仮受金」「有価証券」「投資有価証券」の 6 語は複数の科目に対応するので, 一意に解決せず候補付きの `AmbiguousAccount` を返します.

転記の権限は **処理文脈** (`ProcessingContext`) で決まります. 通常仕訳 (`OrdinaryJournal`) に加えて, 決算処理 (`ClosingProcess`), 連結精算表 (`ConsolidationWorksheet`), シミュレーションエンジン (`EngineComputation`) の 3 つがあり, それぞれが自分専用の転記能力を 1 つだけ追加します. 検査付き変換は文脈を引数に取り, 文脈に合わない科目は `PostingNotAllowed` で拒否します. 損益勘定 `IncomeSummary` を通常仕訳で転記しようとすると弾かれ, 決算処理では通ります.

```haskell
import ExchangeAlgebra.Accounting.PostingPolicy

postingCapabilityFor IncomeSummary                                   -- ClosingOnly
postingAllowedIn OrdinaryJournal (postingCapabilityFor IncomeSummary) -- False
postingAllowedIn ClosingProcess  (postingCapabilityFor IncomeSummary) -- True
```

代数の構築自体 (`.@` や `.+`) はこの検査を通りません. 検査は外部入力が元帳に入る境界にだけ置き, 代数の内部では従来どおり自由に項を作れます.

# 評価勘定の扱いと決算仕訳

0.4 系では減価償却累計額と貸倒引当金を `Liability` 区分に置いていました. 貸方残高になる資産の控除項目を, 区分の側だけで表現していたためです. 0.5.0.0 では `ExBaseClass` に **`isContra`** を加え, この 2 科目を `Assets` 区分の評価勘定として表します. ホームサイドと PIMO は `(区分, isContra)` の組から導出し, 評価勘定では区分の既定サイドを反転します. 観測できる不変条件として `whichSide`, `whatPIMO`, `fixedCurrent` は全科目で従来と同じ値を返し, 変わったのはこの 2 科目の `whatDiv` だけです.

```haskell
isContra  (Not :< AccumulatedDepreciation)  -- True
whatDiv   (Not :< AccumulatedDepreciation)  -- Assets   (0.4 系では Liability)
whichSide (Not :< AccumulatedDepreciation)  -- Credit   (変わらない)
```

この変更は 3 か所に波及します. 第一に, 区分ごとの 6 つの射影 (`projCurrentAssets`, `projFixedLiability` など) は評価勘定を含めなくなり, 評価勘定は `projContraAssets` か汎用の `projContra` で取り出します. 第二に, 貸借対照表と損益計算書の行を作る `bsRows` / `plRows` は, 評価勘定を負債側に置いたり損益の控除科目を落としたりせず, 総額行・控除行・純額行の 3 行で表示します. 格納される値は非負のまま保ち, 負号は行を描画する時点でのみ付きます. 第三に, 区分同士の交換可能性 `(<=>)` を PIMO から導出するようにした結果, `(Assets, Revenue)` と `(Cost, Revenue)` の組 (逆順も同様) が `False` から `True` に変わりました. 基底の交換判定には区分でなく `whatPIMO` を使うのが 0.5.0.0 の作法です.

決算仕訳の範囲も広げました. 0.4 系の `finalStockTransfer` は SNA 由来の 17 科目しか締めていませんでしたが, 0.5.0.0 では registry から締め対象を導出し, すべての費用・収益科目を締めます. `NetIncome` と `NetLoss` だけは区分が損益計算書の表示側を符号化しているため, 区分由来の規則を適用すると振替の符号が反転します. この 2 科目は明示的に `NoClose` とし, 専用の純損益振替が受け持ちます.

# 試算表から財務諸表までの層

0.4 系にも `Write` に貸借対照表・損益計算書・試算表の CSV 出力はありましたが, 試算表の検証, 表示への変換, 派生指標はライブラリの外にありました. 0.5.0.0 では次のモジュール群を加えました. `Bookkeeping` 以外は代数の項に新しい転記を挿入せず, 読み出し専用で動きます.

- **`ExchangeAlgebra.Bookkeeping`**: 決算整理仕訳 (減価償却, 引当金, 経過勘定, 持分法, 前期誤謬の訂正) を, 外部から与えた金額を明示引数として構築します.
- **`ExchangeAlgebra.TrialBalance.Balance`**: 借方・貸方の方向を構造として持つ `AccountBalance` と, (借方, 貸方) の順に固定した相殺の基本演算. 試算表・表示・連結が同じ 1 つの相殺規則を共有します.
- **`ExchangeAlgebra.TrialBalance.Validation`**: 相互科目・一時科目・異常残高などの所見を報告し, 明示した方針で報告への進行を止めます.
- **`ExchangeAlgebra.Reporting.Presentation`**: 検証済みの試算表から日本基準 (JGAAP) の表示へ変換します. 変換の出所を追える形で保ちます.
- **`ExchangeAlgebra.Reporting.Metric`**: 期間損益などの派生指標を型付きで返します. `periodResultOf` は元帳から期間の損益を導きます.
- **`ExchangeAlgebra.Consolidation.Worksheet`**: 連結修正仕訳を検証し, 精算表の出所を保持します.
- **`ExchangeAlgebra.Reporting.Group`**: 前節の表示グループと評価勘定の相殺方針.

`Write` の CSV 出力は残っていますが, 行を組み立てる純関数 (`bsRows` など) を切り出したので, CSV を経由せずに行データを取れます. 決算関連の書類 3 種の CSV writer もここに加えました.

# シミュレーション: Lite front-end, 方針の宣言, スピルの検証

## 新しいモデルは Lite で書く

0.4 系のシミュレーションは `StateSpace` と `Updatable` インスタンスを手で書く front-end でした. 0.5.0.0 の **`ExchangeAlgebra.Simulate.Lite`** はその engine の上に載る別の front-end で, 新しいモデルはこちらで書きます. モデルは 3 つの要素からなります. `Generic` から導出する product-only の world record (各 field に初期値・可変参照・スナップショットの役割を型族で付ける), 各 agent をスナップショットから `Journal` (メッセージ) へ写す純関数の stage の列, 期間・seed・元帳 field・stage・並列方針を束ねる `SimSpec` です.

意味論は bulk-synchronous parallel (BSP) に固定しました. 1 つの stage の中では全 agent が同じスナップショットを見て, 同じ stage 内の先行 agent が出したメッセージは見えません. メッセージは stage の終わりに一括で元帳へ commit され, 後続の stage と後続の期にだけ見えます. 従来の engine は命令的な step が同じ step 内の変更を読めたので, 移植するモデルは read-then-write の依存を 2 つの stage に分けます. 各 agent の乱数生成器は (seed, 期, stage, agent) の 4 つ組だけから導くので, 逐次実行と chunk 並列実行が同じ元帳を返します. これは同梱のテストで固定しています.

従来の front-end は公表済み結果の再現のために残し, 新機能は加えません. 併せて, 最上位モジュール `ExchangeAlgebra` は `Simulate` を再エクスポートしなくなりました. `copy`, `modify`, `update`, `normal` のような一般的な名前が簿記の入口を汚していたためで, 使う側は `import ExchangeAlgebra.Simulate` を 1 行足します.

## 長期シミュレーションの方針を宣言する

**`ExchangeAlgebra.Simulate.Policy`** は, 元帳の保持・スピル・圧縮を `LedgerPolicy` として宣言する語彙です. 既定は全転記を保持する `FullAudit` で, 監査証跡を保ちます. 長期の run では閉じた期の履歴が単調に増えて常駐メモリを支配するので, `CompressClosedTerms` を明示して閉じた期だけを圧縮できます. 進行中の期は圧縮せず, `norm` と残高は保たれます. 圧縮を関数の内部で暗黙に呼ぶことは設計上禁じているため, 方針の名前として書かせる形にしました.

```haskell
longRunPolicy :: LedgerPolicy
longRunPolicy = LedgerPolicy
  { retain     = RetainRecent 2
  , spillTo    = Just "ledger.spill"
  , compaction = CompressClosedTerms
  }
```

この設定 (直近 2 期を常駐, それ以前はディスクへ, 常駐分の閉じた期は圧縮) は, 非圧縮の全保持に比べて常駐メモリを大きく削ります. Lite からは `runLiteWithPolicy` で適用します.

## スピルファイルを検証してから復元する

スピル (ディスク退避) の codec と復元は **`ExchangeAlgebra.Simulate.Spill`** に集約しました. 0.4 系の `readBinarySpillFile` は復号に失敗した chunk 以降を黙って切り捨て, `restoreJournalFromBinarySpill` は部分的にしか読めないファイルや古いファイルを完全なものとして合流させていました. 最後に読めた chunk までの項が消えても気づけません. 0.5.0.0 では最初の復号失敗で例外を上げ, chunk の範囲が順序違い・重複・欠落・空のいずれかなら復元を拒否します. `Either` で受けたい場合は `Checked` 付きの変種を使います.

```haskell
readBinarySpillFileChecked
  :: (Binary t, Binary payload, Ord t, Enum t)
  => FilePath -> IO (Either (SpillReadError t) [((t, t), payload)])

data SpillReadError t
  = SpillDecodeFailure { spillErrorOffset :: Int64, spillErrorChunks :: Int, spillErrorMessage :: String }
  | SpillRangeError    { spillRangeIssue :: SpillRangeIssue, spillRangePrevious :: (t, t), spillRangeCurrent :: (t, t) }
  | SpillEmptyRange    { spillRangeCurrent :: (t, t) }
```

Lite の run はスピルファイルを開くときに切り詰める (`WriteMode`) ので, 同じパスで再実行しても古い chunk が積み上がりません.

## 取引ネットワークと産業連関の生成

**`ExchangeAlgebra.Simulate.Network`** は, 市場の取引関係 (`TradeNetwork`) と投入係数 (`InputCoefficients`) を分けて持ちます. 生成器は決定的で, 完全グラフ, circulant, k-正則, Erdős–Rényi, scale-free, sector block を用意しました. `industrialNetwork` / `industrialFlows` は順序付きの CL-SBM (部門で順序づけ, べき乗則で重み付けしたブロック三角のネットワーク) を整数 seed から決定的に生成し, 同梱の `industrialEx1` がその使い方を示します. 同じ seed から同じネットワークが出ることを前提に, 論文側の再現手順と対応づけています.

## その他の追加

`ExchangeAlgebra.Optimize` はソルバの共通インターフェースで, 焼きなまし (`Optimize.Annealing`) と実数値遺伝的アルゴリズム (`Optimize.GA`) の 2 実装を持ちます. 代数側では `mapBasePart` の関手則, `foldEntries` の普遍性, `postFromNetBy` の `bar` を経由する分解をそれぞれ文書化し, property test で固定しました. 基底の一部で商をとる `decBy` と, 純額から転記へ戻す `postFromNetBy` はその上に載ります.

# モジュール境界の整理

公開モジュールは 35 になり, 7 つの層 (入口, 科目語彙, 中核代数, 仕訳とシミュレーション, 簿記と報告, 変換と補助, 最適化) に並べました. 依存は層の並びに沿って一方向で, 0.4 系にあった `Write` から `Simulate` への依存は無くなりました. スピルの読み書きは `Simulate.Spill` に置き, `Simulate` と `Write` と `Policy` がそれぞれそこを見ます.

`Alg`, `Journal`, `TransTable` は抽象型にしました. `Alg` が公開するのは `Zero`, `(:@)`, `_val`, `_hatBase` だけで, 複数転記の内部表現とキャッシュ field は `ExchangeAlgebra.Algebra.Internal` に移しました. `Journal` は内部に軸ごとの索引を持ち, 手で組み立てた値は索引と本体がずれ, ワイルドカード射影が誤った答えを黙って返すことがあります. 抽象化はこの経路を塞ぐためで, 構築には `mkJournal`, `(.|)`, `fromList`, 転記の一対一の写像には型付きの `mapPosting` / `mapMaybePosting`, note の付け替えには `replaceNotes` を使います. `Internal` は PVP の互換保証の外にあります.

`Chart` / `Chart-cairo` による描画は Cabal flag `visualize` の背後に置きました. 既定は on なので挙動は変わりませんが, cairo や pango を入れたくない環境では flag を切れば `ExchangeAlgebra.Simulate.Visualize` ごと依存から外れます.

```yaml
# stack.yaml
flags:
  exchangealgebra:
    visualize: false
```

# 性能

`Journal.fromList` を遅延の右畳み込みから正格の左畳み込みに変えました. 中核ベンチマークでは転記数が増えるほど差が開き, 万単位の入力で桁違いに速くなります. 転記の多重集合は保たれ, 変わるのは同じ note と基底に衝突した転記の列の順序だけです. この順序は `Eq` / `Show` / `Binary` と, `Double` では `norm` の最下位桁に現れます. `MoneyDecimal` では現れません. ほかに, 完全一致の射影の fast path, 仕訳追加の高速化, 試算表の行生成での射影の共有を入れました. いずれも規模に対する具体的な計測は別記事で扱います.

`AccountTitles` の `Binary` タグは Word8 から big-endian の Word16 になりました. 256 科目の上限が外れ, 範囲外のタグは `Get` の失敗として報告されます. 0.4 系で書き出したスピルファイルや `Journal` のバイナリは 0.5.0.0 では読めません.

# 0.4 系からの移行

`Double` の元帳を `.@` と `.+` で組み立てて `norm` や `bar` で読むだけの利用なら, 変更は要りません. 次に該当するときだけ手を入れます.

影響が広いのは次の 4 つです.

- **自前の `HatVal` インスタンス**: `showValue` を定義します. `HatVal n => RealFloat n` を前提にした signature には `RealFloat` を明示します.
- **`Liner` / `Journal` / `TransTable` の構成子を直接使っていた**: `Alg` の内部表現だけは `ExchangeAlgebra.Algebra.Internal` から取れます. `Journal` と `TransTable` の構成子は公開されないので, `mkJournal` / `(.|)` / `fromList` と `table` / `(.->)` / `(|%)` に置き換えます.
- **`ExchangeAlgebra` 経由で `Simulate` の名前を使っていた**: `import ExchangeAlgebra.Simulate` を足します. `Base.Element` 経由で `Data.Hashable` / `GHC.Generics` 全体を使っていた場合も直接 import に変えます.
- **スピルファイルを読んでいた**: 壊れたファイルは例外になります. `Either` が欲しければ `Checked` 変種へ. 0.4 系のバイナリは Word16 化のため読めません.

残りは該当する箇所だけ直します.

- **`fromList` の出力を `Show` や `Binary` でバイト単位に比較していた**: 同じ (note, 基底) 内の順序が変わりえます. `norm` / `bar` / `balanceBy` で比較します. `MoneyDecimal` にすれば数値結果は順序に依存しなくなりますが, `Show` / `Binary` に現れる順序は残ります.
- **`Element` のワイルドカード method**: 綴りを `wiledcard` から `wildcard` に直しました (`haveWiledcard` / `isWiledcard` / `ignoreWiledcard` も同じ綴りの修正). 互換 alias はありません.
- **負のスカラーを `(.*)` に渡していた**: 例外になります. 値域は非負なので, 減少は Hat 側の転記で表します.
- **`whichSide` にワイルドカード基底を渡していた**: 黙って `Hat` 扱いだったものが例外になります.
- **日商簿記 B 欄の 6 語を `parseAccountTitle` で解決していた**: `AmbiguousAccount` を受けて候補から選ぶか, 正規の構成子名を使います.
- **`Simulate.Lite` の `Stage` 構成子や `GLite*` の primed method**: 名前だけの export になりました. `stageFor` / `stage` / `stageOf` で作り, `stageName` で読みます.
- **評価勘定を `Liability` 側の射影で取っていた**: `projContraAssets` へ. 区分の交換判定は `whatPIMO` へ.
- **`AccountInfo` の `aiDivision` / `aiHomeSide` を読んでいた**: `AccountSemantics` 由来の意味別の field に置き換わりました.
- **振替規則の生のタプルを `show` していた**: `HatVal n => Show (n -> n)` の汎用インスタンスを削除しました.
- **`Journal` の `_jVersion` を参照していた**: どの読み出し経路からも観測されない書き込み専用の counter だったので削除しました.
- **重複や包含のある複数パターンで `proj` / `projNorm` を呼んでいた**: 集合の意味論になり, 各転記は高々 1 回だけ選ばれます. 以前は重複分を二重に数えていました.
- **連結精算表の `BalancePosition`**: 共通の `AccountBalance` に統合しました.
- **非推奨 (0.6 で削除予定)**: `projNorm` / `projWithBaseNorm` / `projWithNoteNorm` は, `bar` による相殺を名前に出した `projNetNorm` / `projWithBaseNetNorm` / `projWithNoteBaseNetNorm` の別名になりました. `Journal.insert` は `replaceNotes` の別名になりました. `Number.NonNegative.Double` の `HatVal` インスタンスと `rounding` も非推奨で, `MoneyDouble` と `Value` の丸め関数へ移ります.

# インストール

```yaml
# stack.yaml
extra-deps:
  - exchangealgebra-0.5.0.0
```

GHC 9.10 (Stackage `lts-24.4` で検証) が対象で, 0.4 系と同じです. 既定では `Chart-cairo` を経由して cairo / pango が要るので, 不要なら前節の flag で外します. 同梱の例は GitHub の `examples/` にあり, 0.5.0.0 では `examples/stack.yaml` が Hackage 版を指すので, `examples/` だけを取り出して `stack build` できます.

# 今後

非推奨にした API (`projNorm` 系の旧名, `Journal.insert`, `Number.NonNegative.Double` の `HatVal` インスタンス, `rounding`) は 0.6 で削除します. 値型・保持方針・並列度を振ったときの計算時間とメモリの scaling は, 本記事では概要にとどめました. 計測条件を揃えた結果は別記事にまとめます. Exchange Algebra の代数構造の解説記事は, 関連論文の正式公表後に改めて書く予定です.
