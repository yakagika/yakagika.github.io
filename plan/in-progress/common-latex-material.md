---
plan_id: common-latex-material
status: in-progress
created: 2026-10-08
updated: 2026-10-08
priority: medium
next_actor: agent
next_action: "Collaborator 節と latex.md の VSCode の画面の図を撮って差し込む (本人の撮影か Chrome 拡張の導入後). 合わせて Windows の TeX Live で手順を通す"
---

# 共通資料 - LaTeX による原稿作成と, 共通資料の一覧の分割

## メタ情報

- **状態**: in-progress (2026-10-08 承認. エンジンは pLaTeX に変更, Python の資料も共通資料の枠に入れる)
- **作成日**: 2026-10-08
- **最終更新**: 2026-10-08
- **対象**: `lectures/common/latex.md` (新規), `lectures/common/git.md` (Collaborator 節の拡充),
  `lectures/common/setup.md` (次に読む資料の一覧), `lectures/common/agent.md` (章ナビ),
  `pages/lectures.markdown` (共通設定のカードを 4 枚に分割), `images/common/git/` と `images/common/latex/` (図)
- **関連計画**: [common-agent-literacy-material.md](common-agent-literacy-material.md) (Collaborator 招待の小節はこの計画で 2026-09-06 に追加した)

## 概要

学会発表の原稿を LaTeX で書き, GitHub の private リポジトリで教員と共有するまでの共通資料を新設する.
あわせて, Git の資料の Collaborator 招待を図つきの手順に拡充し, Lectures ページの「共通設定」を
環境構築, Git, AI Agent, LaTeX の 4 枚と Python の資料を, 講義のカードとは別の「共通資料」の枠にまとめる.

## 動機

- 特別講義の学生は年度末に学会発表をするが, これまでの原稿は Word で作られていた.
  原稿をテキストで書けば, git で差分を読めて, 教員が同じリポジトリで添削できる.
- 共通設定の LaTeX は VSCode の拡張機能を入れる 1 行しかなく, TeX 本体の導入もビルドも書かれていない.
- Git の資料の Collaborator 招待は本文 3 行で, 図も招待後の流れもない.

## 設計

### テンプレート (2026-10-08 確認)

- 本人の指定は SICE のテンプレート. 過去の学生原稿 (丹波 2026, 木村 2026) の体裁 (○登壇者, 概要, キーワード) は
  **システム・情報部門 学術講演会 (SSI)** のテンプレートと一致する.
- SSI のテンプレートは `SICE-SSI.sty` と `sample.tex` の 2 file で, UTF-8 / SJIS / EUC 版がある.
  2026 年版と 2025 年版は同一 (diff 無し). 配布 URL は `https://www.sice.or.jp/org/SSI2026/doc/template_utf8.zip`.
- 中身は pLaTeX 用 (`jarticle`, `graphicx` の `dvips` 指定, 図は `.ps`).
- **pLaTeX + dvipdfmx なら `\usepackage[dvips]{graphicx}` を `[dvipdfmx]` に変える 1 か所だけで, 見本の図 (`fig1.ps`) も含めて公式見本と同じ紙面になることを確認済み** (A4, 1 page, フォント埋め込み済み).
  - 参考: LuaLaTeX でも 3 か所 (`ltjarticle`, `graphicx` の指定外し, `1zw` → `1\zw`) の書き換えで動くことを確認したが, テンプレートが pLaTeX 用なので本人裁定で pLaTeX にした (2026-10-08).
  - `.ps` の図は dvipdfmx が Ghostscript で変換する. Windows の TeX Live で同じく通るかは実機確認の論点.
- テンプレートそのものは資料に再掲せず, 学生が SICE の配布元から取得して自分で書き換える.

### エンジンとビルド

- pLaTeX + dvipdfmx (2026-10-08 本人裁定: テンプレートが pLaTeX 用なのでそれに合わせる). 当初は LuaLaTeX を選んでいたが変更した.
- リポジトリに `.latexmkrc` を置き, `$latex` を platex, `$pdf_mode = 3` (dvipdfmx) にする. 端末からの `latexmk` でも同じ結果になる.
- TeX の導入: macOS は `brew install --cask mactex-no-gui`. Windows は winget に TeX Live が無い (MiKTeX のみ) ため,
  TeX Live 公式の `install-tl-windows.exe` を使う. どちらも数 GB で時間がかかる旨を書く.
- VSCode の LaTeX Workshop で, 保存したらビルドされるようにする. 設定は**リポジトリの `.vscode/settings.json`** に置き,
  教員が clone しても同じ設定でビルドできるようにする. LaTeX Workshop の既定の latexmk は `-pdf` を付けて pdfLaTeX を呼ぶため,
  `-pdf` を付けない latexmk の tool と recipe を定義し, `latex-workshop.latex.autoBuild.run` を `onSave` にする.

### 書き方 (本人指定: 導入 + 基本の書き方)

節と小節, 数式 (インラインと別行立て, 番号と参照), 図 (PNG の挿入, 2 段組での幅), 表 (`tabular`),
参考文献 (テンプレートの `thebibliography` と `1)` 形式の引用), 脚注. SSI の体裁の注意 (図説は英文, Fig. / Table) も扱う.

### Git での共有

- 原稿用に新しい private リポジトリを作り, 教員を Collaborator に招待する. 手順は git.md を参照させ重複して書かない.
- `.gitignore` に LaTeX の中間ファイル (`*.aux`, `*.log`, `*.synctex.gz`, `*.fdb_latexmk`, `*.fls`, `*.out`) を書く.
- **PDF は commit しない** (`.gitignore` に `/*.pdf` を足す. 教員と学生の両方が PDF を記録すると毎回コンフリクトになるため. 2026-10-09 本人指示で 10-08 の「commit する」を撤回). `/` を付けて `figures/` の PDF の図は残す.

### 演習

`### Exercise LATEX-k` (共通資料の `PY` / `GIT` に揃える).

- LATEX-1: テンプレートを書き換えて, 保存でビルドされることを確かめる
- LATEX-2: 自分の原稿の骨組み (題目, 著者, 概要, 節 3 つ, 図 1, 表 1, 文献 2) を作る
- LATEX-3: 原稿のリポジトリを push し, 教員を招待する

### Git の資料の Collaborator 節

- Settings → Collaborators → Add people → 招待の送信 → 招待中の表示までを, 他の節と同じ粒度で書く.
- 図は GitHub にサインイン済みの Chrome で撮る (本人の操作許可が要る). 撮影用のリポジトリは既存の図と同じ検証用のものを使う.
- 招待は 7 日で失効すること, 教員が受け取るまで閲覧できないこと, 招待した相手は private でも中身を読めることを書く.

### Lectures ページ

講義のカード (特別講義, データサイエンス実践, 関数型プログラミング) と分けて「共通資料」の枠を作り, 次の 5 枚を置く (2026-10-08 本人指示で Python も共通資料の枠へ). Colab の資料 (colab.md) は本人の指示に無いので今回は足さない.

| カード | リンク先 |
|---|---|
| 環境構築 | setup.html |
| バージョン管理と GitHub | git.html |
| コーディングエージェント | agent.html |
| LaTeX による原稿作成 | latex.html |
| プログラミング基礎 (Python) | python1.html |

### 章ナビ

`setup → git → agent → latex` とする (agent.md に `nextChapter: latex.html` を足し, latex.md には付けない).
setup.md の「この資料の後に読む資料」に LaTeX を足す. setup.md のデータサイエンス実践向けの LaTeX Workshop の 1 行は残し, latex.md へリンクする.

## ロードマップ

進み具合 (2026-10-08): 1 は本文のみ済 (図は未), 2〜5 は済 (commit dc00af5). macOS の TeX Live 2026 で, 空のリポジトリから手順どおりにビルドし, `git pull` の取り込みも手元の擬似リポジトリで確かめた.


1. git.md の Collaborator 節を拡充し, Chrome で図を撮る
2. latex.md を執筆する (図は VSCode と GitHub の画面, ビルド結果の PDF)
3. 手元の TeX Live で, 資料の手順どおりに空のリポジトリからビルドして確かめる
4. setup.md, agent.md, pages/lectures.markdown を直す
5. サイトをビルドしてリンクと章ナビを確かめる

## コスト見積もり

latex.md の執筆が本体 (1 session). 図の撮影に本人の Chrome の許可が要る.

## 未解決事項 / リスク

| 論点 | 状態 |
|---|---|
| 投稿先は SSI でよいか (他の SICE の研究会は様式が違う) | resolved: SSI (2026-10-08 本人) |
| PDF を commit するか | resolved: commit しない (2026-10-09 本人. 10-08 の「commit する」を撤回) |
| 図の撮影 (GitHub の Collaborators の画面, VSCode の LaTeX Workshop の画面) | 2026-10-08 は Chrome の拡張を使わない判断になり撮れなかった. 本人の撮影か, 拡張の導入後に撮る (理由 a: 本人の判断待ち) |
| サイトへの公開 (docs/ の Publish と push) | 2026-10-09 に公開済み (222d369). LaTeX Workshop の画面の図は TeX Live の導入が長引いたため, 図なしで先に公開する (本人指示). 図を差し込んだら再度 Publish する |
| 図の撮影の受け取り | collab-settings.png と collab-add.png は 2026-10-09 に git.md の手順 3, 4 へ差し込み済み. collab-pending.png は実際に招待しないと撮れないため図なし (2026-10-09 本人). 残りは latex-vscode.png (本人が撮って置いたら latex.md の「保存でビルドされることの確認」へ差し込む) |
| Windows の TeX Live 導入の実機確認 | 本人の Windows 機か VM で後日 (common-agent-literacy-material の実機確認と同じ機会) |

## 関連ファイル

- `lectures/common/setup.md`, `lectures/common/git.md`, `lectures/common/agent.md`
- `pages/lectures.markdown`
- `src/Main.hs:644` (basename の衝突: `latex.md` は他の講義に無いことを確認する)

## 変更履歴

- 2026-10-08: 作成 (SSI テンプレートの LuaLaTeX での動作を確認)
- 2026-10-08: 承認. エンジンを pLaTeX に変更 (1 か所の書き換えで動作確認), Python の資料を共通資料の枠へ, PDF は commit, 様式は SSI
- 2026-10-09: 本人指示で git.md に pull と merge の節 (コンフリクトの解決を含む), add/commit/push を荷物の発送にたとえた図, Exercise GIT-4 を追加. pull の設定を rebase から merge (pull.rebase false) に変え, latex.md の添削の取り込みと PDF のコンフリクトの解決 (原稿から作り直す) を git.md に合わせた. いずれも手元の擬似リポジトリで出力を取って確認
- 2026-10-09: 本人指示で PDF を記録しない方針へ変更. latex.md は clone 直後に `.gitignore` へ `/*.pdf` を足す手順を加え, 「PDF のコンフリクト」の節を削って, 取り込んだ後に PDF を作り直す案内に替えた. git.md のバイナリのコンフリクトの注記も合わせた. `/*.pdf` が `paper.pdf` と `sample.pdf` だけを外し `figures/*.pdf` を残すことを擬似リポジトリで確認
- 2026-10-09: Collaborator の画面の図 2 枚を git.md に入れた. 手順 5 のボタン名 `Add <アカウント名> to <リポジトリ名>` が HTML のタグと解釈されて消えていたので, コードの表記に直した
