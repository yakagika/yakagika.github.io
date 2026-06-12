---
title: repo 間の Agent タスク連鎖 — 中央 immutable queue による cross-repo handoff
description: Multi-Repo Agent Orchestrator の続編. 上流ライブラリの semantic change が下流の研究結果を動かす問題に対し, repo から repo への有向タスク連鎖を「中央 immutable queue + 1 ファイル 1 所有者 + 実行ゲート (preflight)」で運ぶ機構を作った. git の shared-write 衝突を所有権で原理的に消す設計, enqueue 自動 / 実行ゲートの分離, AI 同士の相互検証で初版設計が棄却された経緯を解説する.
tags:
    - claude-code
    - orchestrator
    - automation
    - multi-repo
    - architecture
    - git
date: 2026-06-12
tableOfContents: true
---

[Multi-Repo Agent Orchestrator](/posts/2026-06-03-multi-repo-agent-orchestrator.html) で, 複数リポの Agent を束ねる上位レイヤを紹介した. あの構成の通信モデルは「各リポの Agent ↔ Orchestrator」のハブであり, repo 同士は直接話すことができない. 状態の報告も知識もタスクも, いったん中央 (inbox/outbox) or 人間 を経由し, 毎時の巡回が配り直す形式になっている.

このハブがた運用だと **repo から repo への, 有向の, 低遅延なタスクの受け渡し**の度に人間が何かしらの処理(プロンプトで指示等)を挟む必要があり,非常に手間がかかるので,直接別repoの間のAgentが話せるようにした.

本記事はそのために追加した中央 immutable queue による cross-repo handoff の仕組みと設計判断の話です. 前回の続編だが, 「複数の git repo に複数の Agent が並行で作業している時, repo 間の依頼をどう運ぶか」という一般的な問題として読めるはず.

# ハブでは何が運べないか

既存の inbox/outbox ハブは repo ↔ Orchestrator の汎用レーンで, これはこれで機能している. しかし上の用途には 3 点で合わない.

1. **有向でない**: ハブの宛先は「Orchestrator」であって「特定の repo」ではない. 上流 Agent が「この変更は下流のあのリポに影響する」と分かっているのに, いったん中央の解釈を挟む必然がない.
2. **低遅延でない**: 配り直しは毎時巡回 (≤1h) 経由である. cmux でライブラリ側と論文側のセッションが**同時に生きている**作業日には, 1 時間の遅延は並行作業の意味を削ぐ.
3. **連鎖を表現できない**: 「version X の追従が終わってから version Y の追従をする」のような**依存つきのタスク列** (DAG) を, ハブの単発タスクでは表現できない.

実は当初は, もっと素朴な解決策 — 上流 Agent が下流リポの plan ディレクトリへ直接タスクファイルを書き込む「mailbox 方式」 — を設計していた. これがレビューで棄却された経緯は後述するとして, 先に最終形を見せる.

# 解決策 — 中央 immutable queue

機構の全体は, Orchestrator 領域 (git 追跡) に置かれた 2 種類のファイルだけでできている.

```
state/cross-repo-handoffs/
  <宛先リポ>/
    queue/   <chain_id>.md   ← 送信者 (上流 Agent) 所有. immutable, write-once = request
    receipts/<chain_id>.md   ← 受信者 (下流 Agent) 所有 = 処理結果 (disposition)
```

<svg viewBox="0 0 720 430" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="cross-repo handoff の全体像" font-family="sans-serif" font-size="13">
  <defs>
    <marker id="hah" markerWidth="9" markerHeight="9" refX="7" refY="3" orient="auto">
      <path d="M0,0 L7,3 L0,6 Z" fill="#555"/>
    </marker>
    <marker id="hahb" markerWidth="9" markerHeight="9" refX="7" refY="3" orient="auto">
      <path d="M0,0 L7,3 L0,6 Z" fill="#b58a4a"/>
    </marker>
  </defs>
  <!-- upstream -->
  <rect x="20" y="30" width="190" height="70" rx="6" fill="#eef4fb" stroke="#4a78b5"/>
  <text x="115" y="56" text-anchor="middle" font-weight="bold">上流 repo の Agent</text>
  <text x="115" y="76" text-anchor="middle" fill="#555" font-size="11">semantic change を land</text>
  <text x="115" y="92" text-anchor="middle" fill="#888" font-size="11">(ライブラリの正本)</text>
  <!-- queue -->
  <rect x="270" y="20" width="180" height="180" rx="6" fill="#fbf7ee" stroke="#b58a4a"/>
  <text x="360" y="44" text-anchor="middle" font-weight="bold">中央 queue (git 追跡)</text>
  <rect x="290" y="60" width="140" height="56" rx="4" fill="#fff" stroke="#999"/>
  <text x="360" y="82" text-anchor="middle">queue/&lt;id&gt;.md</text>
  <text x="360" y="100" text-anchor="middle" fill="#888" font-size="11">送信者所有・不変</text>
  <rect x="290" y="128" width="140" height="56" rx="4" fill="#fff" stroke="#999"/>
  <text x="360" y="150" text-anchor="middle">receipts/&lt;id&gt;.md</text>
  <text x="360" y="168" text-anchor="middle" fill="#888" font-size="11">受信者所有</text>
  <!-- downstream -->
  <rect x="510" y="30" width="190" height="70" rx="6" fill="#eef4fb" stroke="#4a78b5"/>
  <text x="605" y="56" text-anchor="middle" font-weight="bold">下流 repo の Agent</text>
  <text x="605" y="76" text-anchor="middle" fill="#555" font-size="11">session 冒頭に queue を走査</text>
  <text x="605" y="92" text-anchor="middle" fill="#888" font-size="11">(ライブラリを使う研究)</text>
  <!-- arrows -->
  <line x1="210" y1="65" x2="290" y2="80" stroke="#555" marker-end="url(#hah)"/>
  <text x="238" y="58" fill="#333" font-size="11">enqueue</text>
  <text x="238" y="72" fill="#888" font-size="10">write-once</text>
  <line x1="430" y1="80" x2="510" y2="65" stroke="#555" marker-end="url(#hah)"/>
  <text x="468" y="58" fill="#333" font-size="11">read</text>
  <line x1="510" y1="90" x2="430" y2="150" stroke="#555" marker-end="url(#hah)"/>
  <text x="487" y="128" fill="#333" font-size="11">receipt を書く</text>
  <!-- import to plan -->
  <rect x="510" y="140" width="190" height="60" rx="6" fill="#fff" stroke="#999"/>
  <text x="605" y="164" text-anchor="middle">自リポの plan へ import</text>
  <text x="605" y="184" text-anchor="middle" fill="#888" font-size="11">受信者所有のコピーで作業</text>
  <line x1="605" y1="100" x2="605" y2="140" stroke="#555" marker-end="url(#hah)"/>
  <!-- orchestrator surface -->
  <rect x="150" y="290" width="420" height="110" rx="6" fill="#f6f1fb" stroke="#7a5aa5"/>
  <text x="360" y="316" text-anchor="middle" font-weight="bold">Orchestrator (毎時巡回)</text>
  <text x="360" y="340" text-anchor="middle" fill="#555" font-size="12">receipt の無い request (未 drain) を数え,</text>
  <text x="360" y="358" text-anchor="middle" fill="#555" font-size="12">下流の context-pack / briefing に surface する</text>
  <text x="360" y="382" text-anchor="middle" fill="#888" font-size="11">永続パス: session を跨いだ取りこぼし防止 (≤1h)</text>
  <path d="M340,290 L350,200" stroke="#b58a4a" fill="none" stroke-dasharray="4 3" marker-end="url(#hahb)"/>
  <text x="290" y="250" fill="#7a5aa5" font-size="11">queue を監視</text>
  <path d="M570,330 L640,210" stroke="#b58a4a" fill="none" stroke-dasharray="4 3" marker-end="url(#hahb)"/>
  <text x="600" y="260" fill="#7a5aa5" font-size="11">通知</text>
</svg>

request (queue 側) はこういう形をしている. ポイントは `class` と `verify` と `depends_on_items` の 3 フィールドである.

```yaml
---
chain_id: <上流>-<commit|version>-<slug>   # 一意. 他 request の依存から参照される
from: <上流リポ>
to:   <下流リポ>
trigger: { kind: commit | release, ref: <hash|version> }
class: mechanical | substantive            # 迷ったら substantive
depends_on_items: []                       # 先行すべき他 chain_id (DAG)
on_dependency_rejected: ask                # 先行が却下された時: ask | abandon | continue
verify: ["stack build", "..."]             # 受信側が preflight で走らせる検証
created: 2026-06-11
---
## なぜ (上流で何が変わったか)
## 受信側でやること (action)
## 受け入れ基準
```

どの repo がどの repo の上流かは, Orchestrator の設定 (1 枚の YAML) に `depends_on` エッジとして宣言されている. このエッジが queue の張られるトポロジを決め, 巡回が監視する対象も決める.

# 設計判断 1 — 所有権の分離で git 衝突を原理的に消す

このシステムのファイルはすべて git 追跡された markdown であり, 複数の Agent (とユーザ) が並行で触る. すると当然, 同一ファイルへの並行書込み (shared-write) が衝突源になる. 解決策として採用したのは [Learn 層](/posts/2026-06-11-personal-learn-layer.html)の iCloud 競合対策と同じ指針 — **衝突はマージアルゴリズムでなく所有権で避ける** — である.

- **request は送信者所有で, 書いたら不変 (write-once)**. 受信者は request に一切触らない.
- **受信者の状態は別ファイル (receipt) に書く**. 1 chain_id = 1 receipt で, これは受信者だけが書く.
- 処理の作業コピーは受信側リポの plan ディレクトリへ import し, そこは受信者の領域.

「queue item に対応する receipt が無い」= 未処理, という判定もファイルの存在だけで決まるので, 状態フィールドの更新合戦が起きない. request を後から訂正したい時も, 元ファイルは編集せず**新しい chain_id を立てて旧を superseded 指定**する. event-sourcing を markdown と git でやっている, と言ってもよい.

この所有権モデルのおかげで, 同一ファイルへの shared-write が**設計上存在しない** (= git conflict が原理的に起きない). 並行 Agent の数が増えても通信路の不変条件は変わらない.

# 設計判断 2 — enqueue は自動, 実行はゲート

次に「いつ送ってよいか / いつ実行してよいか」の規律. ここは非対称にした.

**enqueue (送る側) は自動・即時・承認不要**である. 上流 Agent は semantic change (下流の挙動を動かしうる変更) を land した時点で, 影響先の queue へ request を積む. 迷ったら積む. これが許されるのは, **積むだけでは何も実行されない**からである. queue は不変の依頼書の束であって, 積むこと自体に副作用がない.

**drain (実行する側) はゲート付き**である. 下流 Agent は session 冒頭に自分宛て queue を走査し, class で分岐する.

<svg viewBox="0 0 720 300" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="drain の実行ゲート" font-family="sans-serif" font-size="13">
  <defs>
    <marker id="gah" markerWidth="9" markerHeight="9" refX="7" refY="3" orient="auto">
      <path d="M0,0 L7,3 L0,6 Z" fill="#555"/>
    </marker>
  </defs>
  <rect x="20" y="110" width="130" height="56" rx="6" fill="#fbf7ee" stroke="#b58a4a"/>
  <text x="85" y="134" text-anchor="middle" font-weight="bold">request</text>
  <text x="85" y="152" text-anchor="middle" fill="#555" font-size="11">class で分岐</text>
  <rect x="210" y="30" width="180" height="64" rx="6" fill="#fff" stroke="#999"/>
  <text x="300" y="54" text-anchor="middle" font-weight="bold">mechanical</text>
  <text x="300" y="74" text-anchor="middle" fill="#555" font-size="11">preflight: verify 実行 + diff 生成</text>
  <rect x="210" y="190" width="180" height="64" rx="6" fill="#fff" stroke="#999"/>
  <text x="300" y="214" text-anchor="middle" font-weight="bold">substantive</text>
  <text x="300" y="234" text-anchor="middle" fill="#555" font-size="11">承認ゲートへ (実行しない)</text>
  <line x1="150" y1="125" x2="210" y2="70" stroke="#555" marker-end="url(#gah)"/>
  <line x1="150" y1="150" x2="210" y2="210" stroke="#555" marker-end="url(#gah)"/>
  <rect x="480" y="20" width="220" height="56" rx="6" fill="#eef7ee" stroke="#5a9a5a"/>
  <text x="590" y="42" text-anchor="middle" font-weight="bold">自動完了</text>
  <text x="590" y="62" text-anchor="middle" fill="#555" font-size="11">diff 空 + 統計判定・図表値が不変</text>
  <rect x="480" y="190" width="220" height="80" rx="6" fill="#f7eeee" stroke="#a55a5a"/>
  <text x="590" y="214" text-anchor="middle" font-weight="bold">ユーザ承認待ち</text>
  <text x="590" y="234" text-anchor="middle" fill="#555" font-size="11">承認 → 実行 → done</text>
  <text x="590" y="252" text-anchor="middle" fill="#555" font-size="11">却下 → rejected で記録</text>
  <line x1="390" y1="48" x2="480" y2="48" stroke="#555" marker-end="url(#gah)"/>
  <line x1="390" y1="222" x2="480" y2="222" stroke="#555" marker-end="url(#gah)"/>
  <path d="M390,80 C440,110 440,160 410,190" stroke="#a55a5a" fill="none" marker-end="url(#gah)"/>
  <text x="455" y="140" fill="#a55a5a" font-size="11">非ゼロ diff / 統計判定変化</text>
  <text x="455" y="156" fill="#a55a5a" font-size="11">/ 図表値変化 → 自動昇格</text>
</svg>

- **mechanical** (新しい解釈を要さない反映 — 版番号の追従, pin の更新, 決定的再実行) でも, 自動で許されるのは **preflight = 検証コマンドの実行と diff の生成まで**. その結果 diff が空で統計判定も図表値も動かない時だけ, ユーザ指示なしで完了する. これが自動完了の**唯一の**経路である.
- preflight で**非ゼロ diff, 統計判定の変化, 図表値の変化**が出たら, その request は自動で substantive に昇格し, 承認ゲートに回る (receipt に昇格の事実が記録される).
- **substantive** (定義の改訂, 結論に関わる数値解釈, 本文主張の変更) は最初から承認ゲート行き. 承認まで実行されない.

この設計の核は, 動機の事故をそのまま裏返したものである. 「機械的な版更新」と分類されたタスクが, 実行してみたら結論を動かす — これが一番危ない取りこぼしだった. だから**分類を信用せず, preflight の結果で再分類する**. mechanical とは「安全だと宣言されたタスク」ではなく「安全かどうかを安く確認できるタスク」のことだ, と言い換えてもよい.

依存連鎖 (DAG) の側にも対応するゲートがある. 先行 chain_id が done になるまで後続は処理されない. では先行が**却下**されたら? — ここで黙って待ち続けるとデッドロックになるので, request 自身が `on_dependency_rejected` (ask / abandon / continue) で振る舞いを宣言しておく. 循環依存や参照先不明の request は blocked として隔離され, ユーザに報告される. 「待ちで固まる」状態を作らないことは, 人間が常時監視しない系では特に効く.

# 設計判断 3 — 二重トランスポート

通信路は 2 本あり, どちらも同じ queue を指す.

| パス | 経路 | 遅延 | 役割 |
|---|---|---|---|
| 即時 | 上流 Agent が中央 queue へ直接 write | ゼロ | 並行 session が同時に生きている時の低遅延 handoff |
| 永続 | Orchestrator 巡回が未 drain 件数を下流の context-pack / briefing に surface | ≤1h | session・日・版を跨いだ取りこぼし防止 |

即時パスだけだと, 下流のセッションが立っていない時に request が放置される. 永続パスだけだと冒頭の低遅延要件に戻ってしまう. 2 本を併設し, ただし**状態は queue という 1 箇所にだけ持つ** (パスはどちらも「気づかせる」だけ) ので, 二重計上や食い違いは起きない.

ハブ (inbox/outbox) との関係も同じ整理である. ハブは repo ↔ Orchestrator の汎用レーン, 本機構は repo ↔ repo の有向タスク連鎖で, **役割が違うので置き換えず併存させる**. 通信路を増やす時は「既存路の代替か, 別役割か」を先に決めておくと, 後で経路の選択に迷わない.

# 初期廃案 — AI 同士の相互検証

最後に設計の経緯を書いておきたい. 冒頭で触れたとおり, 初版の設計は「上流 Agent が**下流リポの plan ディレクトリへ直接** request を書き込む mailbox 方式」だった. 直感的だし, 中央を経由しない分シンプルに見える.

この案を, 普段から使っている相互検証の枠組み — Claude が設計し, 独立した別モデル (Codex) が懐疑的にレビューする — に掛けたところ, 4 点の指摘で棄却された.

1. 受信者が request の status を更新すると, 送信者と受信者の**同一ファイル shared-write** になる (git 衝突源).
2. mechanical の自動実行は, 「機械的な版更新が結論を覆す」事故 (まさに動機の実例) を**取りこぼす**.
3. 一部の下流リポは「plan ディレクトリは Orchestrator が取り込まない」という既存契約を持っており, 上流 Agent がそこへ書き込む設計は**契約と衝突**する.
4. リポによって plan を git 追跡していたりいなかったりして (gitignore), mailbox の**永続性が非対称**になる.

どれも言われてみればその通りで, 特に 2 は失敗例があり上手く働かないことが分かっていた. この指摘を受けて「中央 immutable queue + 所有権分離 + preflight 止まり」へ改設計したのが本記事の形である. 設計文書には, 採用した原則だけでなく**初版が何で棄却されたか**も why として記録してあり, 将来似た判断をする Agent がそこを読む.

一人で開発していると設計レビューの相手がいない, というのは個人開発の古典的な弱点だが, 独立した複数の AI に同じ設計を別角度から読ませる体制は, その代替として実用になりつつあると感じる.

