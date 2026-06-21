# 暗黙の認知スキルを note で可視化する — fp 先行・効果検証つき

## メタ情報

- **状態**: proposed
- **作成日**: 2026-06-03
- **最終更新**: 2026-06-03
- **対象**: まず `lectures/fp/fp*.md`. 効果を見てから `lectures/slds/slds*.md` と全体へ.
- **発端**: fp5 再帰節が講義で伝わらず, 「再帰関数を実装するコツ」note を追加 (working tree 差分). 根本原因は学生が「記号を操作対象として扱い, 操作する」ことが出来ないこと, という担当者の診断.

## 概要

教師が暗黙に持つ認知スキル (例: 「まだ完成していない関数を, 既に機能しているとみなして使う」) と, 学生がまだ獲得していない認知段階のあいだのギャップが, プログラミング教育の主要な障壁になっている. 本計画は, このギャップを扱う教育研究をサーベイし, **「暗黙の認知スキルを名指し → スキルに分解 → 可視化 → 読む→直す→書く で足場を外す」** という再利用可能な記述方針として一般化する.

**運用方針 (本改訂の要点)**: 新しい box 型 (`::: skill` 等) は導入しない. 既存の `::: note` の中で skill を明示化する書き方を試す. まず **fp に限定** して, 担当者が無意識に使っているスキルを棚卸し → note 追加を個別に検討 → 講義で効果を検証 → その結果を見て **slds・全体への導入可否を判断** する, という段階を踏む.

横断テーマは **reification (物象化): 記号・過程をそれ自体ひとつの操作対象 (オブジェクト) として扱えるようになること**. 再帰の「飛躍」も高階関数の「関数を値として渡す」も同じ閾値の別の現れ.

## 動機

担当者の観察 (fp5 再帰): 学生は `total (x:xs) = x + total xs` を書き写せても, 「いま定義中の `total` を完成済みとみなして右辺で使う」操作が出来ない. 記号 (`total`, `xs`) を「操作の対象」として掴めていない. 教師には自明な認知操作が学生には不可視.

文献ではこれは **expert blind spot (専門家の盲点)**: 熟達で認知過程が自動化され, 教師は自分が踏んでいた中間ステップを思い出せなくなる. 経験豊富な指導者でも難易度判断は 43% しか一致しないという報告がある ([Nathan et al., 2001](https://website.education.wisc.edu/mnathan/Publications_files/2001_NathanEtAl_ICCS_EBS.pdf); [Unraveling novices' code composition difficulties, 2023](https://www.tandfonline.com/doi/full/10.1080/08993408.2023.2169067)). 暗黙スキルを明示記述に落とすことは, この盲点を構造的に塞ぐ. **だから出発点は「自分が無意識に何をしているか」の棚卸しになる.**

## 文献サーベイ

### A. 「なぜ詰まるか」を説明する理論 (診断)

1. **operational / structural 二重性と reification (Sfard 1991)** — 数学的概念は過程 (operational) としても対象 (structural) としても捉えられ, 過程が物象化されて初めて対象になる. reification は本質的に難しい. 「記号を操作対象にする」はこの理論そのもの.
   ([Sfard 1991](https://link.springer.com/article/10.1007/BF00302715); [Sfard & Linchevski 1994](https://link.springer.com/article/10.1007/BF01273663))
2. **APOS / procept (Dubinsky; Gray & Tall)** — Action→Process→Object→Schema. Process→Object の**カプセル化**が最難関. procept = 記号を過程と対象の両方として柔軟に見る力. 再帰の「呼び出す過程」と「関数という対象」の往復はこれ.
   ([Tall keynote 2007](http://sigmaa.maa.org/rume/crume2007/papers/tall.pdf))
3. **neo-Piagetian 段階 (Lister, Teague)** — sensorimotor (トレース不可) → preoperational (トレースは出来るが入出力の帰納でしか推論できない) → concrete operational (コードを読んで演繹できる) → formal. 「トレースは出来るのに説明できない」学生は preoperational. 再帰が伝わらない典型.
   ([Concrete and other neo-Piagetian forms of reasoning](https://www.semanticscholar.org/paper/Concrete-and-other-neo-Piagetian-forms-of-reasoning-Lister/100a326fb2ff7c348f30c54aed245855eadcb792); [Tracing and the Preoperational Programmer](http://users.sussex.ac.uk/~bend/ppig2014/6ppig2014_submission_8.pdf))
4. **notional machine (du Boulay; Sorva)** — 言語構文が含意する観念上の計算機. Haskell では**置換 (substitution / reduction) モデル**がそれ. fp5 の `total [1,2,3] = 1 + (total [2,3]) = ...` 展開列, fp6 の `foldl`/`zipWith` 展開列が実例.
   ([Sorva, Notional Machines](https://dl.acm.org/doi/pdf/10.1145/2483710.2483713))
5. **再帰の心内モデル (Götschi/Sanders; Close & Dicheva)** — 初学者は誤った **loop モデル**, 専門家は **copies モデル**. 指導テーマに **leap of faith** が挙がる.
   ([Götschi et al.](https://dl.acm.org/doi/10.1145/611892.612004); [Close & Dicheva](http://ddi.cs.uni-potsdam.de/HyFISCH/IterationRekursion/MentalModelsClose.htm))

### B. 「どう介入するか」の手法 (処方)

6. **認知的徒弟制 (Collins, Brown, Newman)** — 全体の傘. 熟達者の思考は不可視なので「意図的に表面に出して可視化する」. modeling / coaching / scaffolding / articulation / reflection / exploration.
   ([Making Thinking Visible](https://www.psy.lmu.de/isls-naples/intro/all-webinars/collins/cognitive-apprenticeship.pdf))
7. **ワークト例 + サブゴール・ラベリング (Margulieux, Catrambone, Morrison)** — 解答例の各ステップに構造ラベル. 学習・保持・転移が向上. fp5 の 4 ステップ手順は既にこれ.
   ([Subgoals Help Students Solve Parsons Problems](https://dl.acm.org/doi/10.1145/2839509.2844617); [Guzdial, subgoal labeling](https://computinged.wordpress.com/tag/subgoal-labeling/))
8. **PRIMM (Sentance, Waite)** — Predict→Run→Investigate→Modify→Make. 「書く」前に「読む・予測・トレース」.
   ([PRIMM](https://dl.acm.org/doi/10.1145/3287324.3287477); [Raspberry Pi pedagogy review](https://www.raspberrypi.org/app/uploads/2021/11/Teaching-programming-in-schools-pedagogy-review-Raspberry-Pi-Foundation.pdf))
9. **Parsons 問題 / Use-Modify-Create** — 行の並べ替えで構文負荷を切り離し構造に集中.
10. **認知負荷理論 / スキル分離 (Sweller; Caspersen & Bennedsen)** — 複合スキルは要素に分けて練習, 足場は徐々にフェード.

### サーベイの含意 (一文)

**詰まりの正体は reification の閾値であり, 処方は「暗黙の認知操作を名指して可視化し, サブゴールでラベルし, 読む→直す→書く の順で認知負荷を上げる」** に収束する.

## 一般化した方針: note の中でやること

box を増やさず, 既存 `::: note` の中で次の 4 つを意識して書く (全部を毎回やる必要はない).

1. **名指す (name)**: 暗黙スキルに名前を与える. 例「飛躍 (leap of faith)」「型から形を読む」「ループを map と見抜く」.
2. **分解する (decompose)**: スキルレベルの番号付き手順 (= サブゴール・ラベル) に落とす. fp5 の 4 ステップが既存モデル.
3. **可視化する (make visible)**: 教師が頭の中でやっている操作を「考え声」で書き下す. Haskell なら置換トレース.
4. **足場を外す (fade)**: 「読む (予測/トレース) → 直す (改変) → 書く (作成)」の順で課題を置く. 既存の `### Exercise CHN-K` 規約は維持し, 必要なら問題文の頭に `(予測)` / `(改変)` / `(作成)` と一言添える程度にとどめる.

記述様式は **「私が頭の中でやっていること」** という一人称の可視化を基本トーンにする (認知的徒弟制の modeling).

## 事例: fp5 / fp6 の暗黙スキル棚卸し

担当者が fp5・fp6 で (おそらく無意識に) 使っているスキルの候補一覧. **これは「note 追加候補のプール」であり, 全部を書くわけではない**. 各スキルに ❶名指し ❷出現箇所 ❸reification との関係 を付す. 優先度は「学生が詰まりやすい × 横断的に効く」で粗く付けた.

### 中核 (reification の幹) — 最優先候補

- **S1. 記号を「対象」として掴む (reification の素地)**
  名前や式を「あとで動かせる 1 個のモノ」として見る. **すべての土台**. fp5 演算子/変数, fp6 関数渡し, 全章に通底.
- **S2. 置換で評価を追う (notional machine / 簡約)**
  `total [1,2,3]` や `foldl (+) 0 [1,2,3]` を手で一段ずつ書き換える. fp5 再帰トレース・fp6 fold/zipWith 展開列に既出. **可視化の核**で, 他スキルの確認手段にもなる.
- **S3. 飛躍 (leap of faith): 未完成の関数を完成済みとみなして使う**
  `total (x:xs) = x + total xs` の右辺で `total` を信頼して呼ぶ. fp5 再帰. 既に note 化済 (今回の発端). copies モデルの明示反証 (fp の再帰 vs Python の `for`) とセットにすると効く.
- **S4. 型から関数の「形」を読む (type-directed reasoning)**
  `applyFToList :: (a->b)->[a]->[b]` を見ただけで「各要素に f を当てる」と分かる. 逆に型で本体を絞る. fp6 高階関数全般・fp5 「型を明確にする」. reification と並ぶ FP の中核スキル.

### 関数の対象化 (fp6 の幹) — 高優先候補

- **S5. 関数を「値」として渡す (first-class function)**
  `map show xs`, `applyFToList fib`. 関数をデータとして扱う = 関数の reification. fp6 高階関数の前提.
- **S6. 部分適用で「引数が一つ減った関数」を作る (partial application / section)**
  `(2*)`, `(10 <)`, `filter (10 <)`. 「不完全な式」ではなく「新しい関数という対象」と見る. fp5 カリー化, fp6 section. 学生が特に詰まりやすい.
- **S7. ループの形を高階関数に対応づける (pattern → map/filter/fold)**
  「各要素を変換 = map」「条件で残す = filter」「1 個に畳む = fold」. 構造パターンを名前のある combinator に翻訳する抽象化. fp6 全体. 手続き脳からの橋渡し.
- **S8. 関数合成でデータの流れを組む (pipeline)**
  `sum . map abs`, `(f . g)`. データが関数列を流れるイメージ. fp6 関数合成・`sumAbs` 解答に既出 (が暗黙).

### 構文・分岐の対象化 — 中優先候補

- **S9. 「すべては式」: 式の入れ子と合成**
  `if ... then ... else` も case も値を返す式なので, 引数や別の式の中に埋められる. fp5 「式と文」. ネスト/合成の自由度の源.
- **S10. 演算子と関数を相互変換する**
  `(+) 1 2` ↔ `1 + 2`, `` `mod` `` のバッククォート, section 化. 演算子記号を関数オブジェクトとして扱う. fp5 中置演算子.
- **S11. 名前付け vs 使い捨て (ラムダ) の判断**
  その場限りなら `\x -> ...`, 再利用するなら名前を付ける. fp6 ラムダ式の動機. 可読性の設計判断.

### 思考の進め方 (メタスキル) — 横断・常時

- **S12. 場合分けと基底からの構築 (case decomposition)**
  「いちばん単純な場合」から作り, 一般の場合へ広げる. fp5 再帰レシピ手順 2-3, パターンマッチ設計. 再帰以外にも効く.
- **S13. 具体例で確かめる (grounding by example)**
  `[1,2,3]` を入れて手で追う. S2 と対. 教師が無意識にやる検算を, 学生は飛ばす.
- **S14. 不変条件 / 変わるもの・変わらないものを見る (invariant)**
  fold のアキュムレータは変わる状態, 再帰の「残り」は縮む. 暗黙だが状態・モナド章への布石.

### この棚卸しから最初に着手する 1〜2 件 (PoC)

fp5 再帰節は既に S3 が note 化済なので, **そこに S2 (置換トレースの明示)** と **S4 (型から形を読む)** を一人称トーンで足すのが最小・最大効果. 次点で fp6 の **S6 (部分適用)** か **S7 (ループ→map/filter/fold)** を 1 つ note 化して比較する.

## ロードマップ (fp 先行・検証つき)

1. **棚卸し (本ドキュメント)**: fp5/fp6 のスキル候補を列挙 (済).
2. **PoC (1〜2 件)**: fp5 再帰節の note に S2・S4 を一人称で追記. コード例を増減した場合は `fp-examples/test/Fp5/*Spec.hs` も更新 (CLAUDE.md fp ルール) し `stack test`.
3. **講義で効果検証**: 実際の授業で伝わりやすさを定性確認. 詰まりどころの変化をメモ.
4. **fp 内で横展開可否を判断**: 効いた書き方だけ fp6 以降へ展開. 棚卸し残りのスキルを順次検討.
5. **slds・全体への導入可否を判断**: fp の結果を見てから slds (ベクトル化・分布・モデルの reification) への一般化を検討. ここで初めて全体方針を確定.
6. **landed 化**: 反映方針が固まったら `plans/landed/` へ移動.

## コスト見積もり

- 棚卸し: 済 (本ドキュメント).
- PoC (fp5 note 追記 + spec 確認): 数時間.
- 効果検証: 講義 1 回ぶん (定性).
- 横展開: note 1 件あたり 1〜2 時間, 段階的に.

## 未解決事項 / リスク

- **note の肥大化**: 1 節に長い skill note が積もると本筋が読みにくくなる. 1 note 1 スキルに絞り, 折りたたみ (`<details>`) の活用も検討.
- **「考え声」トーンの統一**: 一人称可視化は効くが冗長にもなりやすい. 短く要点だけ.
- **効果測定が定性的**: 学生反応依存. 比較のため「追加前/後」で詰まりどころをメモしておく.
- **演習タグの扱い**: `(予測)/(改変)/(作成)` は CLAUDE.md の Exercise 見出し規約と衝突させない (見出しでなく問題文側に添える).
- **fp-examples 同期**: コード例を触ったら spec 更新を忘れない (CLAUDE.md).

## 関連ファイル

- `lectures/fp/fp5.md` (再帰節 L818-): 起点. 「再帰関数を実装するコツ」note は working tree 差分.
- `lectures/fp/fp6.md`: 高階関数・ラムダ・合成. S4-S8 の宝庫.
- `fp-examples/test/Fp5/*Spec.hs`, `Fp6/*Spec.hs`: コード例変更時の検証対象 (CLAUDE.md).
- `lectures/slds/slds5.md`, `slds6.md`, `slds9.md`, `slds11.md`, `slds12.md`: slds 適用先候補 (fp 検証後).
- `CLAUDE.md`: Exercise 見出し規約 (fp / slds), fp-examples 検証ルール.

## 変更履歴

- 2026-06-03: 作成 (サーベイ + 一般化枠組み + fp/slds 適用案).
- 2026-06-03: 方針改訂. 新規 box を廃し `::: note` 内で skill 明示化に変更. fp 先行 + 効果検証 + 段階導入へ. fp5/fp6 の暗黙スキル棚卸し (S1-S14) を追加.
- 2026-06-03: PoC 第一弾を fp5 に着地. メタスキル **S12 (場合分けで考える)** をパターンマッチ節末尾に, **S13 (具体例で手を動かす)** を再帰節の `total [1,2,3]` 簡約列直後に, 既存 `::: note` 様式・一人称トーンで挿入. 新規コード例なしのため fp-examples 変更なし. `stack exec main rebuild` 通過. → 次は講義での効果検証, および fp6 以降への横展開候補の選定.
- 2026-06-03: PoC 第二弾を fp6 に着地. fp6 以降の最優先候補として **S7 (ループを map/filter/fold と見抜く)** を選定 (reification の本丸 + S3 loop 誤解の回収 + 手続き脳からの橋渡し + 転移大). map/filter/fold/zipWith 紹介の直後・`### Exercise CH6-1` 直前に, 章冒頭の `applyFToList` = `map` の回収を含む `::: note` を挿入. 新規コード例なし → fp-examples 変更なし. rebuild 通過.
