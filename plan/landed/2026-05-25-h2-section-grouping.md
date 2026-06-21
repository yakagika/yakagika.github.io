# h2 セクション全体の視覚的グルーピング

## メタ情報

- **状態**: landed
- **作成日**: 2026-05-25
- **最終更新**: 2026-05-25
- **着地日**: 2026-05-25
- **採用範囲**: Phase 1 + Phase 1.5 (Phase 2 は abandoned)
- **関連**: [css/base.css](../../css/base.css), [src/Main.hs:62-77](../../src/Main.hs) (`writerSectionDivs` の OFF 理由), [templates/default.html](../../templates/default.html) (TOC JS の id 解決ロジック)

## 概要

lecture 記事の `h2` 配下を「セクション」として視覚的にグループ化し、長文ページでも **「今どの h2 配下にいるか」が一目で分かる** デザインに改善する.

**実装結果**: Phase 1 (`article h2` を背景グラデ + 下罫線 + 56px 上余白で見出し帯化) と Phase 1.5 (2 つ目以降の h2 の前に dashed 区切り線を浮かせる) で目的を達成. Phase 2 (writerSectionDivs 有効化 + section card 化) は副作用が大きく ROI が低いため不採用.

## 動機

直前に実施した `plans/landed/<TBD>-lecture-callout-redesign` (note/warn/exercise の刷新, 2026-05-25) で callout 単位の区切りは明確化されたが, **マクロな章単位 (h2) のグルーピング** は未対応のまま残っていた. 章の終わりと始まりが視覚的に弱く, 長い章 (fp4 「エラー対応」など) で「これは前章のまとめか, 次章の前置きか」が判別しづらかった.

## 設計 (検討案と最終採用)

### 検討した案

| Option | 概要 | 工数 | 副作用 | 採用 |
|---|---|---|---|---|
| 1 | Pandoc `writerSectionDivs = True` を有効化 + CSS で `section.level2` をスタイル | 中 | `::: note` + 見出しの旧バグ再発リスク, 全 HTML 書換, 入れ子の視覚的重さ | ✕ 不採用 |
| 2 | JS で post-processing し h2 ごとに `<section class="h2-group">` で wrap | 小 | print/no-JS 環境で機能しない, 初回ペイント時にチラつく | ✕ 不採用 |
| 3 | Pandoc filter (Haskell の AST 変換) で wrap | 大 | 完全制御だが Hakyll の Pandoc 連携を深掘りする必要あり | ✕ overkill |
| **4 (Phase 1)** | CSS だけで h2 自身に背景帯 + 上余白拡大 + 下罫線 | 極小 | 「セクション本体を囲む箱」までは出せない | **✅ 採用** |
| **4.5 (Phase 1.5)** | 2 つ目以降の h2 の前に `::before` で dashed 区切り線を浮かせて章末を明示 | 極小 | なし | **✅ 採用** |

### Phase 2 を abandoned とした理由

ユーザとのレビューで「Phase 1 + 1.5 で十分」と判断. 主な根拠:

- **入れ子の視覚的重さ**: section card にすると note/warn/exercise のさらに外側に箱が増え, 学生視点で情報密度が上がりすぎる
- **回帰検証コストが見合わない**: `::: note` + 見出しの旧バグ (Main.hs:62-70 のコメント) の再発リスクが残る. 既存対応 (`closest('section[id]')`, `section.note` 両対応 CSS) で備えはあるが, 全 lecture の visual regression を自動化なしで確認するのは負担大
- **CSS の組合せ爆発**: `level2`/`level3`/`level4` × `note`/`warn`/`exercise` の override が増える
- **Phase 1.5 で「章末がどこか」が解消**: Phase 2 の主要 value (章末の視覚化) は dashed separator で代用できた

将来 long-form ページがさらに増えて再度ニーズが出てきたら Phase 2 を再検討する.

### 採用された CSS (`css/base.css`)

```css
/*
 * article 内の h2 は章境界として強調する.
 * - 上余白を拡大してマクロな章間距離を確保
 * - 左 4px ボーダー + accent の極淡 wash で見出し帯を形成
 * - 下罫線で配下コンテンツとの分離を明示
 */
article h2 {
    margin-top: 56px;
    margin-bottom: 24px;
    padding: 14px 14px 14px 16px;
    background: linear-gradient(
        to right,
        rgba(13, 148, 136, 0.07),
        transparent 70%
    );
    border-bottom: 1px solid var(--border-color);
}

article h2 + * {
    margin-top: 14px;
}

html.dark article h2 {
    background: linear-gradient(
        to right,
        rgba(45, 212, 191, 0.10),
        transparent 70%
    );
}

@media print {
    article h2 {
        background: none;
        border-bottom: 1px solid #000;
    }
}

/*
 * 2 つ目以降の h2 の前に dashed 区切り線を入れて章末を視覚化する
 * (Phase 1.5: writerSectionDivs を有効化せず構造変更ゼロで章境界を明示)
 */
article h2 ~ h2 {
    margin-top: 72px;
    position: relative;
}

article h2 ~ h2::before {
    content: "";
    display: block;
    border-top: 1px dashed var(--border-color);
    margin: -36px 0 28px 0;
}

@media print {
    article h2 ~ h2::before {
        border-top-color: #999;
    }
}
```

## ロードマップ (実施記録)

- [x] Phase 1: `base.css` の `article h2` ルールを追加 → `stack exec main -- rebuild` → fp4 で screenshot 確認 (2026-05-25)
- [x] ユーザ判断: Phase 2 へ進まず Phase 1.5 を採用 (2026-05-25)
- [x] Phase 1.5: `article h2 ~ h2::before` で dashed separator を追加 → screenshot で h2 境界の dashed 線を確認 (2026-05-25)
- [x] 計画文書を `plans/landed/2026-05-25-h2-section-grouping.md` に移動 (2026-05-25)

## 実施コスト (実績)

- Phase 1: 30 分 (計画通り)
- Phase 1.5: 15 分 (CSS 数行 + screenshot 確認)
- 計画書作成・レビュー含む: 約 1.5 時間

## 学び / 注意点 (将来の参考)

1. **Hakyll の stale binary 問題**: 本タスク中盤で `stack exec main -- clean && build` が `templates/content.html (snapshot _final) was not found in the cache` で再現的に失敗. 解決には `stack build --force-dirty` で executable を完全再構築する必要があった. CSS 変更だけで誘発される理由は不明 (Haskell の linker 状態か stack の cache 不整合)
2. **dashed separator の浮かせ方**: `::before` を `display: block` + `margin: -36px 0 28px 0` で h2 の padding zone から上方向に -36px シフト. negative margin で要素を「親より外」に配置するパターン
3. **Phase 2 の判断材料**: 本タスクではメリット 7 件 / デメリット 12 件を一覧化してユーザに提示し採否を判断. CSS のような視覚タスクは「実装する前にデメリットを言語化」が肝心

## 関連ファイル

- `css/base.css` (実装本体: `article h2`, `article h2 ~ h2`)
- `src/Main.hs` (`customWriterOptions` の `writerSectionDivs` — Phase 2 不採用のため変更なし)
- `templates/default.html` (TOC スクロールスパイ JS は無改修)
- `lectures/fp/fp4.md` (目視テスト用)
- `plans/landed/<2026-05-25 callout redesign>.md` (前段の note/warn/exercise 刷新, ※ 別タスクで `plans/landed/` に直接書き込んだ場合は補完)

## 変更履歴

- 2026-05-25: 作成 (Phase 1/2 の 2 段階アプローチで提案)
- 2026-05-25: Phase 1 実装完了, screenshot 確認
- 2026-05-25: Phase 2 のメリット/デメリット精査 → 不採用判断
- 2026-05-25: Phase 1.5 (dashed separator) 実装完了, screenshot 確認, landed へ移動
