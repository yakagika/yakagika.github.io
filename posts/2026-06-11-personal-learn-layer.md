---
title: AIと連携した個人の学習層 (Learn)
description: AI Agent との対話で得た知識が会話ログの中に流れて消える問題への自前の解. Agent が教えた / 薦めた知識を Obsidian vault に queue し, スマホで M/N 形式で少しずつ消化し, 非同期 Q&A と確認問題の添削を毎時巡回で回す「人間のための学習層」の設計を, iCloud 競合対策 (1 ファイル 1 書き手) や承認制の登録経路などの設計判断とともに解説する.
tags:
    - claude-code
    - obsidian
    - pkm
    - spaced-learning
    - orchestrator
    - automation
    - mcp
featured: true
date: 2026-06-11
tableOfContents: true
---

前回の記事 [Multi-Repo Agent Orchestrator](/posts/2026-06-03-multi-repo-agent-orchestrator.html) で, 複数リポの Claude/Codex セッションを束ねる上位レイヤを紹介した. あの構成にはタグ付きの知識層 (自前 RAG) があり, あるリポの Agent が踏んだ gotcha を別リポの Agent が二度踏まない, という横断再利用が回っている.

Agentの自律運用がある程度回ってくる中で, Agentの成果に対する私の理解と承認が障壁として顕在化してきた. 特に Opus 4.8 までは,提案の内容を理解して,課題等の指摘が可能であったが, 昨日公開された Fable 5 を使っていると, 思考の展開が早い,抽象度が高い,コードと数理の間の横断が自動的に展開していき追いつけない場面が出てきた.

対話の中で確認することは可能だが,人間の理解がボトルネックとなり,また理解してもそれらは会話ログの中に死蔵されることになる. そこで,人間のための学習キューをセッションから分離することにした(Orchestrator の知識層は *Agent のための* corpus であって, *人間のための* 学習キューではない).

本記事はその欠けた層 — **人間 (私) 自身の学習を, Agent 群と同じ枠組み (queue + 毎時巡回) で回す層** — の作成と運用を扱う.

まだ運用を始めたばかりなのであまり事例が積まれていないが,

- 全論文の参考文献(候補)とbibを管理する,papaer repo が次に読むべき論文をmd化→分割キュー化,スマホでちまちま読む,メモをする,質問をする,などを統合.

- 作業中に出てきた実装,数学,その他の知識に関して,教科書的に説明するmdを作成→分割キュー化

などを行っている. 特に1つ目に関しては,論文をスキマ時間に読む環境として,かなり良さそうに感じている.

# 要件

作る前に, 自分の学習がどこで破綻しているかを整理すると, 要件は次の 5 つになった.

1. **queue できる**: Agent が「これは学ぶ価値がある」と判断した知識 (承認制), または私が「これを学びたい」と指示した知識が, 1 箇所に積まれる.
2. **スマホで少しずつ消化できる**:
通勤や隙間時間に, 数分で読める単位 (Part) に割られていてほしい. 進捗が視覚化・管理可能なこと.

3. **集積される**: 学習済みの知識が markdown として個人の知識管理 (PKM) 網に残り, リンク・検索・関連付けに乗り,資産として運用できること.

4. **質問できる**: 読んでいて分からないところをその場で書いておけば, 短時間で Agent が答えてくれる.


# 手法比較 — なぜ Obsidian 正本か

当初考えていた候補は 以下の4 手法.

| | Todoist のみ | Anki 等 (間隔反復) | 専用アプリ自作 | **Obsidian 正本 + Todoist 薄ミラー** |
|---|---|---|---|---|
| 長文 + 数式の本文 | ✗  | △ (カード粒度に分解が要る) | ○ (自由) | ○ (markdown + KaTeX/MathJax) |
| 学習済みの集積 | ✗ (完了したら消える) | △ (カード DB に閉じる) | △ (export 次第) | ◎ (vault の知識網に直接残る) |
| スマホ閲覧 | ◎ | ○ | ○ | ○ (iCloud 同期, 追加実装ゼロ) |
| Agent との接続 | ○ (API) | △ | ○ | ◎ (plain markdown を読み書きするだけ) |
| 初期コスト | 小 | 中 | 大 | 小 (Phase 1 は plugin ゼロ) |

決め手は要件 3 (集積) である. 学習の成果物は「読了の記録」ではなく「自分の vault に増えた知識ノート」であってほしい. もともと個人の知識管理を Obsidian でやっているので, 学習 note の正本を Obsidian (iCloud 同期) に置けば, スマホ閲覧も集積も追加実装なしで手に入る.

Todoist は, 後ほど連携して**通知と Done 入口だけの薄い一方向ミラー**として足す予定. 間隔反復 (Anki 的な復習サイクル) も 後の検討項目で, 正本が plain markdown なのでどの方向にも発展できる可能. この「アプリ化や高機能化を後回しにしても障害にならない」点が, 正本を plain markdown に置く最大の利点である.

# 全体像

仕組みは小さい. Python スクリプト数本 (stdlib のみ) + headless Claude + launchd の毎時巡回で構成される.

<svg viewBox="0 0 720 470" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="Learn 層の全体像" font-family="sans-serif" font-size="13">
  <defs>
    <marker id="lah" markerWidth="9" markerHeight="9" refX="7" refY="3" orient="auto">
      <path d="M0,0 L7,3 L0,6 Z" fill="#555"/>
    </marker>
    <marker id="lahg" markerWidth="9" markerHeight="9" refX="7" refY="3" orient="auto">
      <path d="M0,0 L7,3 L0,6 Z" fill="#5a9a5a"/>
    </marker>
  </defs>
  <!-- enqueue row -->
  <rect x="20" y="20" width="170" height="56" rx="6" fill="#eef4fb" stroke="#4a78b5"/>
  <text x="105" y="44" text-anchor="middle" font-weight="bold">各リポの Agent</text>
  <text x="105" y="62" text-anchor="middle" fill="#555" font-size="11">📖 学習推奨を送る</text>
  <rect x="250" y="20" width="170" height="56" rx="6" fill="#fff" stroke="#999"/>
  <text x="335" y="44" text-anchor="middle" font-weight="bold">ユーザ承認</text>
  <text x="335" y="62" text-anchor="middle" fill="#555" font-size="11">briefing 経由 (承認制)</text>
  <rect x="480" y="20" width="170" height="56" rx="6" fill="#fff" stroke="#999"/>
  <text x="565" y="44" text-anchor="middle" font-weight="bold">enqueue CLI</text>
  <text x="565" y="62" text-anchor="middle" fill="#555" font-size="11">markdown を M/N に分割</text>
  <line x1="190" y1="48" x2="250" y2="48" stroke="#555" marker-end="url(#lah)"/>
  <line x1="420" y1="48" x2="480" y2="48" stroke="#555" marker-end="url(#lah)"/>
  <line x1="565" y1="76" x2="565" y2="120" stroke="#555" marker-end="url(#lah)"/>
  <!-- vault -->
  <rect x="40" y="122" width="640" height="130" rx="6" fill="#f6f1fb" stroke="#7a5aa5"/>
  <text x="360" y="144" text-anchor="middle" font-weight="bold">Obsidian vault (iCloud) の Learn/  —  正本</text>
  <rect x="60" y="158" width="280" height="74" rx="4" fill="#fff" stroke="#999"/>
  <text x="200" y="178" text-anchor="middle" font-weight="bold">queue/&lt;note&gt;.md</text>
  <text x="200" y="196" text-anchor="middle" fill="#555" font-size="11">ユーザ正本 (checkbox / ##Q / ##A / ##M)</text>
  <text x="200" y="214" text-anchor="middle" fill="#888" font-size="11">Agent はマーカー直下への冪等挿入のみ</text>
  <rect x="370" y="158" width="140" height="74" rx="4" fill="#fff" stroke="#999"/>
  <text x="440" y="182" text-anchor="middle" font-weight="bold">dashboard.md</text>
  <text x="440" y="200" text-anchor="middle" fill="#555" font-size="11">Agent 専有</text>
  <text x="440" y="216" text-anchor="middle" fill="#888" font-size="11">毎巡回で再生成</text>
  <rect x="540" y="158" width="120" height="74" rx="4" fill="#fff" stroke="#999"/>
  <text x="600" y="182" text-anchor="middle" font-weight="bold">assets/</text>
  <text x="600" y="200" text-anchor="middle" fill="#555" font-size="11">画像等の添付</text>
  <text x="600" y="216" text-anchor="middle" fill="#888" font-size="11">note 別フォルダ</text>
  <!-- user -->
  <rect x="20" y="300" width="220" height="70" rx="6" fill="#eef7ee" stroke="#5a9a5a"/>
  <text x="130" y="326" text-anchor="middle" font-weight="bold">ユーザ (スマホ Obsidian)</text>
  <text x="130" y="346" text-anchor="middle" fill="#555" font-size="11">Part を読む / checkbox ✓</text>
  <text x="130" y="362" text-anchor="middle" fill="#555" font-size="11">##Q 質問 / ##A 解答 / ##M メモ</text>
  <line x1="130" y1="300" x2="130" y2="252" stroke="#5a9a5a" marker-end="url(#lahg)"/>
  <text x="142" y="280" fill="#5a9a5a" font-size="11">iCloud 同期</text>
  <!-- patrol -->
  <rect x="300" y="300" width="400" height="150" rx="6" fill="#fbf7ee" stroke="#b58a4a"/>
  <text x="500" y="322" text-anchor="middle" font-weight="bold">毎時巡回 (launchd)</text>
  <rect x="320" y="338" width="110" height="50" rx="4" fill="#fff" stroke="#999"/>
  <text x="375" y="358" text-anchor="middle">patrol</text>
  <text x="375" y="376" text-anchor="middle" fill="#888" font-size="11">読むだけ</text>
  <rect x="450" y="338" width="110" height="50" rx="4" fill="#fff" stroke="#999"/>
  <text x="505" y="358" text-anchor="middle">headless</text>
  <text x="505" y="376" text-anchor="middle" fill="#888" font-size="11">claude -p</text>
  <rect x="580" y="338" width="100" height="50" rx="4" fill="#fff" stroke="#999"/>
  <text x="630" y="358" text-anchor="middle">冪等挿入</text>
  <text x="630" y="376" text-anchor="middle" fill="#888" font-size="11">挿入前 snapshot</text>
  <line x1="430" y1="363" x2="450" y2="363" stroke="#555" marker-end="url(#lah)"/>
  <line x1="560" y1="363" x2="580" y2="363" stroke="#555" marker-end="url(#lah)"/>
  <text x="500" y="416" text-anchor="middle" fill="#888" font-size="11">未回答の ##Q / 未添削の ##A を検出し, 回答をブロック直下へ書き戻す</text>
  <text x="500" y="434" text-anchor="middle" fill="#888" font-size="11">進捗 (M/N) は checkbox から導出して dashboard にのみ反映</text>
  <line x1="375" y1="338" x2="320" y2="252" stroke="#555" marker-end="url(#lah)"/>
  <text x="310" y="290" fill="#333" font-size="11">read</text>
  <line x1="630" y1="338" x2="630" y2="252" stroke="#555" marker-end="url(#lah)"/>
  <text x="642" y="290" fill="#333" font-size="11">write</text>
</svg>

学習 note は次のような形をしている. 入力 markdown を `## ` 見出し単位で Part に割り, 長すぎる節 (1500 字超) は段落単位で再分割, 小さい節は併合する. 各 Part の末尾に読了 checkbox が付く.

```markdown
---
title: <タイトル>
status: queued
source_repo: <どの repo 由来か>
recommended_by: agent | user
parts: 23
---
# <タイトル>

## Part 1/23 — <小見出し>
<本文>

- [ ] Part 1/23 読了

## Part 2/23 — ...
```

ユーザ (私) のやることは, スマホの Obsidian で Part を読んで checkbox を ✓ するだけ. 進捗は毎時の巡回が checkbox から数えて dashboard (一覧 note) に反映する.

# 設計の肝

機能としては「分割して積んで, 質問に答える」だけだが, 設計判断はいくつか紹介する価値があると思っている.

## 1 ファイル 1 書き手 — iCloud は無言で上書きする

最大の敵は iCloud 同期である. iCloud はオフライン編集が重なると, マージもエラーもせず**片方を無言で上書きする** (運が良ければ conflict copy が残る. 別リポでは conflict copy 58 件を踏んだ実績がある). スマホで checkbox を ✓ した直後に Agent が同じファイルを書くと, どちらかの編集が消える.

対策は同期アルゴリズムの工夫ではなく, **ファイルごとに書き手を固定する**という構造的なものにした.

| ファイル | 書き手 |
|---|---|
| `queue/<note>.md` (学習 note 本体) | **ユーザ正本**. Agent は後述の冪等挿入のみ |
| `dashboard.md` (一覧・進捗) | **Agent のみ**. 毎巡回で全再生成 |
| `assets/` (画像等の添付) | enqueue 時のみ書く |

当初は「queue note にはユーザしか書かない. Agent の回答は別ファイルに append して embed で読む」という完全分離だった. しかし使ってみると, 質問の回答は**質問を書いたその場所の直下**に出てほしい (スマホで embed の先まで飛ぶのは体験が悪い). そこで不変条件を一段緩和して, Agent の書込みを次の 4 条件つきで許した.

1. 挿入位置はユーザが書いたマーカーブロックの**直下のみ**. 既存行の変更・削除は絶対にしない (append-only な挿入).
2. **冪等**: ブロック内容の hash に対するマーカーをコメントで残し, 二重挿入を防ぐ. ユーザが質問文を書き換えると hash が変わり, 新しい質問として再回答される.
3. 挿入前に note 全文を repo 側へ **snapshot** する (iCloud に上書きされた場合の復元用の安全網).
4. 書込みは巡回時の数秒のみ (ユーザのオフライン編集と時間が重なる確率を最小化する).

「同期の衝突はアルゴリズムでなく所有権で避ける」「緩和するときは無条件でなく, 復元手段と確率の低減をセットにする」というのが, この層に限らず iCloud + Agent 構成全般で使っている指針である.

## ユーザの操作は行頭マーカー 3 種だけ

note への能動的な操作は, 任意の場所に行頭マーカーで書く 3 種類に統一した. スマホのキーボードで打てる記法であることが重要である.

- `##Q <質問>` — 質問. 次の巡回 (≤1h) で**直下に**回答 callout が入る.
- `##A <自分の解答>` — 確認問題への解答. 直下に添削 callout (正誤 + 指導) が入る.
- `##M <メモ>` — 学習メモ. Agent は回答せず, dashboard のメモ欄に集約だけされる (note を汚さず, 後から知識化する時の材料になる).

実際の画面はこうなる. 読んでいる論文 note の途中に `##Q` で質問を書いておくと, 次の巡回で直下に回答が挿入される.

![##Q で書いた質問の直下に, 巡回が回答 callout を冪等挿入する (スマホの Obsidian)](/images/posts/personal-learn-layer/inline-qa.png)

回答の生成は headless の `claude -p` で行う. 巡回スクリプトが未回答の `##Q` / 未添削の `##A` を検出して JSON に吐き, 回答 Agent が note 全文 + 質問 (解答添削の場合は直前の確認問題も) を文脈にして回答を生成し, 専用 CLI がブロック直下へ挿入する. **リアルタイムの対話なしで「Agent に質問できる」を成立させる**のがポイントで, スマホで読んでいる最中に PC の前にいる必要はない.

地味な gotcha として, マーカーブロックの終端は「空行まで」にしてある. note の任意箇所に書かれるため, 空行を越えて続きを読むと元の本文と機械的に区別できなくなるからである. この種の「どこまでがユーザの書いた質問か」問題は, 自由編集される markdown に機械処理を混ぜる時に必ず出てくる.

## 進捗は導出状態 — note に書き戻さない

M/N の進捗や queued / learning / done の状態を note の frontmatter に書き戻す設計も考えられるが, やめた. 巡回は checkbox を**数えるだけ**で, 導出した状態を dashboard にのみ表示する. 状態の二重管理 (checkbox と frontmatter が食い違う) を避けるためと, ユーザ正本への書込み経路を増やさないためである. 状態を 1 箇所 (checkbox) に置いて他は導出にする, というのは Orchestrator のタスク層でフィールド単位に真の側を分けたのと同じ思想である.

## launchd と iCloud TCC — /bin/bash では書けない

毎時巡回は launchd で回している. ここに macOS 固有の罠があって, launchd から `/bin/bash` 直起動したスクリプトは, TCC (プライバシー保護) により iCloud 配下 (`Mobile Documents`) への書込みが拒否される. 対策は, 独立にコンパイルした小さな helper binary を responsible process として立てる方式で, これは以前 backup 機構を作った時に確立した手法の再利用である. ついでに深夜 (0-7 時) は巡回を skip する quiet hours も入れてある (深夜に回答が届いても読まないし, 万一の暴走時の被害も減る).

# 他リポの Agent からの登録経路 — 承認制

この層の入口は「Agent が学習を薦める」である. 十数リポで走っている各 Agent が, セッション中に「これはユーザが学ぶ価値がある」と判断した知識を送ってくる — のだが, ここを自由化すると queue がすぐ Agent の善意で溢れる. そこで**承認制**にした.

1. 各リポの Agent は, 学習推奨を Orchestrator 経由のユーザ依頼 (📖 prefix 付き) として送る. **queue への直接書込みはさせない.**
2. 推奨は briefing (毎時の横断レポート) に surface され, 対話セッションが選択肢として提示する.
3. 承認されたものだけが enqueue CLI で queue に入る. 却下は却下として記録される.
4. 例外は 1 つ: ユーザがセッション中に「これを learn に入れて」と明示指示した場合のみ, その場で直接 enqueue してよい.

面白いのは, このために**新しい仕組みをほぼ作っていない**ことである. Orchestrator には元々「Agent からユーザへの判断依頼」を briefing に集約する承認フローがあり, 学習推奨はそれに 📖 prefix を付けて相乗りしただけ. 横断的なタスクは Orchestrator だけが扱い, 各リポの Agent は自リポのことだけを surface するという既存の役割分担とも整合する. レイヤを増やす時は, 専用機構より既存フローへの相乗りを先に検討する方がよい.

# 実例: 圏論解説 note — 研究知識の段階配信

この機構が一番活きたユースケースを紹介する. ライブラリの開発で, 並列実行の正しさを圏論で整理して定理証明→実装を進めていた際にその実装内容を, 私が確認する作業を一旦スキップして,証明自体は型推論とコンパイラに任せて取り敢えずgoを出した.そこで研究リポの Agent に, **できるだけ分解して理解への負担を最小限にした解説 note** を書かせ, learn に投入した.

結果は 23 Parts, 確認問題 6 問の note になった. 書かせる時の規約が効いていて:

- 証明は省略しない. 等式変形は 1 行 1 根拠で, どの定義・どの補題を使ったか明記する.
- 各節末に**確認問題**を置き, 解答は折りたたみ (callout) で同じ場所に入れる.

![圏論入門 note (23 Parts). 1 行 1 根拠の証明と折りたたみ解答, Part 単位の読了 checkbox (スマホの Obsidian)](/images/posts/personal-learn-layer/note-categorical.png)

↑この初歩的なところから,自分の研究まで分割して接続しているので,そのまま他人への説明になりうる(まだ全部読めていないけれど).

これをスマホで Part ごとに消化していく. モノイドの定義から始まって 1 Part 数分, 確認問題には `##A` で自分の解答を書くと巡回が添削を返してくる. 「研究に必要な長い数学知識を, 確認問題つきで自分に段階配信する」という体験は, 講義を受けるのとも教科書を読むのとも違っていて, 自分の研究の文脈だけに最適化された教材が, 自分の隙間時間の粒度かつ初学者レベルまで分解して届くために,認知不可が極限まで低下している感覚がある.

副産物として, 読了後の note はそのまま vault に残る. 論文の改稿で同じ数学に触れる時, Agent も私もこの note を参照できる. 学習の成果物が「読了の記録」でなく「知識ノート」になる, という要件 3 はここで効いている.

# 段階導入と今後

現在 Phase 1 (queue + M/N + 非同期 Q&A, Obsidian plugin ゼロ) が稼働中で, 以後は運用データを見ながら段階導入する.

1. **Phase 1 — Obsidian コア** (稼働中): 本記事の内容.
2. **Phase 2 — Todoist 一方向ミラー**: enqueue 時に「📖 タイトル (0/N)」をタスク起票し, 巡回が進捗を reconcile する. 通知と Done 入口だけの薄い層で, 正本は常に vault.
3. **Phase 3 — TTS / 間隔反復**: 読み上げと復習サイクル (FSRS 系) の検討.
4. **Phase 4 — アプリ化判断**: 消化率や Q&A 頻度のデータを見て決める.

最初から専用アプリを作らなかったのは意図的で, 「そもそも自分は queue された知識を消化するのか」という一番の不確実性を, 一番安い構成 (plain markdown + 既存の Obsidian) で検証したかったからである. そして正本が plain markdown である限り, どの Phase に進んでもデータの移行が問題にならない — これは Orchestrator の vault を全 markdown にしたのと同じ賭けであり, 今のところ良い賭けだったと思っている.

AI に何かを教わること自体は, もう誰でもやっているが 流れて消えるその知識を, 自分の知識基盤に向けて queue し直す層を一枚挟む — Agent 群の orchestration の隣に, 人間の learning を置いてみた, という話でした.
学生にAIの使い方,AIに教わる的な使い方を指導している資料などが多いが,個人的にも一番しっくり来ている. そのうち,学生向けの資料としてまとめるためのたたき台にしようかと思っている.
