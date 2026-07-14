---
plan_id: fam-2sort-nat-trans-article
status: landed
created: 2026-07-14
updated: 2026-07-14
priority: high
next_actor: none
next_action: "完了 — ユーザ判断で post でなく講義 (fp9/fp10) へ反映 (2026-07-14)"
---

# 記事化ブリーフ: 関手の F はなぜ一文字か — 2ソート定義から Functor/Applicative/Monad を自然変換で貫く

## メタ情報

- **状態**: landed — **ユーザ判断 (2026-07-14) で記事 (post) ではなく講義資料へ反映**. fp10 を型紙 (関手+自然変換 2 本+法則, クラス=利用形) で再構成し, 手作り→正体→クラス化への並べ替え・ε/μ 二重矢印図・両クラスの利用上の変換統一表まで実装. 記事固有要素 (単ソート定義, カリー化随伴・閉圏, hom 同型の自然性, DerivingVia) は未収載 — 将来記事化する場合の素材として本ブリーフを保存
- **由来**: handoff `desktop_app-2026-07-14T10-02-33-591375` (desktop chat の 10 段の疑問連鎖を記事/講義素材化)
- **想定 posts/ ファイル名**: `posts/2026-07-XX-functor-applicative-monad-two-sorted.md`
- **class**: substantive → ユーザ承認待ち

## 現状分析 (2026-07-14 時点)

handoff の内容のうち **講義側への反映は同日のセッションで実装済み**:

- 疑問 1 (F₀/F₁ と同じ記号 F) → fp9 関手の 2 ソート定義 + 1 対 1 対応表 + 「添字を省いて同じ F で書く」注記
- 疑問 3 (applicative = 関手 + ε/μ + コヒーレンス) → fp10 「Applicative の正体」の (f, ε, μ) 再定式化
- 疑問 5 読み方A (pure=0引数, fmap=1, <*>=もう1引数) → fp10 クラス化の節
- 疑問 6 (タプル寄せ vs カリー化の二表示, 結合律が両者一致を保証) → fp10 「引数を増やす」節 (liftA2M/liftA3M のネスト) + 結合律の注記
- 疑問 10 (return=η, >>= = μ∘T(−)) → fp10 モナドの正体 + Monad instance 抜粋 (join∘fmap)
- Applicative(f,ε,μ) ↔ モナド(T,η,μ) の統一表 (単位+乗法, ⊗ = × vs ∘) → fp10 正体末尾に新設

**記事にしかない要素 (未実装)** — 記事化の主対象:

1. 単ソート定義 (arrows-only: 対象 = 恒等射, Ob/dom/cod/id を ∘ から復元) と 2 ソート定義の対比 — 疑問 1・2 の核心
2. F(id_X)=id_FX による「対象対応は射対応から一意復元」の議論 (記号の濫用が恣意的でない理由)
3. 疑問 5 読み方B: エフェクトの独立性 (⊗ か → か が applicative と monad の全差) の正面展開 + 御利益 (全エラー集約・静的解析・並列・traverse)
4. 疑問 7-9: カリー化随伴 (−×B) ⊣ (B⇒−), 閉圏・CCC, hom 集合同型の 2 層 (成分の全単射 + 自然性 2 本), η=半カリー化・ε=eval・三角等式=β/η則
5. μ (積版) を基本に取る 2 つの理由 (閉性不要 / 共変双関手間の自然変換として行儀がよい)
6. Haskell 型クラス機構と意味論のズレのコラム (Monoidal⇒Applicative は真だが要明示導出, DerivingVia 推奨, base は Applicative primary)
7. runnable GHC 断片 (pure/<*> ⇔ unit/⊗ 変換, >>= ⇔ join/fmap 変換)

## 構成案 (handoff 原文どおり)

(1) 導入 =「同じ記号 F」の違和感 → (2) 二ソート vs 単ソート → (3) 自然変換 + コヒーレンスの共通テンプレート →
(4) applicative = lax monoidal (⊗)・独立エフェクト・多引数の二表示 → (5) カリー化随伴で橋渡し, 随伴・閉圏 →
(6) hom 同型 = Set 値関手の自然同型 → (7) monad = η, μ, 三層統一表で締め. DerivingVia コラム挿入.

## 受け入れ基準 (handoff 原文)

次セッションで, 上記導線に沿った記事ドラフト (Markdown/Hakyll 用) または講義節の骨子 + 主要コード断片が起草され, ユーザのレビュー待ち状態になっていること.
