---
title: 関数型プログラミング Ch9 圏と多相データ型
description: 資料
tags:
    - algebra
    - lecture
    - statistics
    - haskell
featured: true
katex: true
date: 2026-07-13
tableOfContents: true
previousChapter: fp8.html
nextChapter: fp10.html
open: true
---

[前章 (第8章)](fp8.html)では, 型 (= 集合) の上に演算と法則を載せた **代数構造** と, その受け皿としての型クラスを見ました. 章の後半では **準同型 (構造を保つ写像)** が中心になり, 締めくくりで「台が値から関数 (射) へ持ち上がる」と予告しました. 本章はその回収です.

振り返ると, ここまでの 2 章は「**台に何を据えるか**」の物語として読めます.

- [第7章](fp7.html)では, 型を **集合** とみなしました. 台は「値の集まり」で, その上の関係・関数を直積の部分集合として定義しました.
- [第8章](fp8.html)では, 集合の上に **演算と法則** を載せて **代数** を作りました. 台は変わらず値の集まりのまま, 載せるものが増え, 最後に **構造を保つ写像 (準同型)** を手に入れました.

本章では, 台そのものを取り替えます. これまで「値の集まり」の上に構造を載せてきたのに対し, 今度は **関数 (写像) そのものの集まり** を台に据え, その上に **合成** という演算を載せます. こうしてできる構造が **圏 (category)** です.

[第7章](fp7.html)の演算の節で描いた図の系列が, この転換をそのまま表しています. 図0 は集合だけの世界. [第8章](fp8.html)までの主役は **図1 の世界** でした. 点 = 台 (集合) はただ 1 つ, 射 = 台の中で閉じた演算で, この世界に法則を課して代数を作ったのでした. 本章の主役は **図2 の世界** です. 点 = 型 (集合) は無数にあり, 射 = 関数が型から型へ渡ります. この図2 の世界を, 点 (型) の側からではなく **射 (関数) の側から** 眺め, 射の集まりを台に据えて合成の法則を課したものが圏です. 図1 から図2 へ向かう矢印こそ, [第7章](fp7.html)の演算の節で **俯瞰** (台を射の集まりに取り替える見方の転換) と名づけた操作であり, 本章はそれをついに実行するわけです.

<svg viewBox="0 0 772 246" width="100%" style="max-width: 780px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="図0 から図2 までの系列. 図0: 集合だけの世界 (点 Type のみ). 図1: 代数. 台 = Type, 演算 = ● と e がループの射. 第8章までは この世界に法則を課して代数を作った. 右の矢印は俯瞰 (台を射の集まりに取り替える見方の転換). 図2: 圏 Hask. Ob = {A, B, C}, Mor = {f, g, g∘f, id, …}. 本章はこの図2 の世界の射の集まりを台に据える. それが圏である.">
  <defs>
    <marker id="lad0-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="6.5" markerHeight="6.5" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <!-- 図0: 集合だけの世界 -->
  <rect x="12" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="26" y="46" fill="currentColor" font-size="10" opacity="0.8">図0  集合</text>
  <text x="110" y="106" fill="currentColor" font-family="monospace" font-size="12.5" font-weight="600" text-anchor="middle">Type</text>
  <g fill="currentColor" text-anchor="middle">
    <text x="110" y="193" font-size="12" font-weight="600">点 = 集合 (型)</text>
    <text x="110" y="208" font-size="9.5" opacity="0.85">台 = Type</text>
    <text x="110" y="222" font-size="9.5" opacity="0.85">演算 = (まだ無い)</text>
    <text x="110" y="237" font-size="10" opacity="0.7">第7章: 型 = 集合</text>
  </g>
  <line x1="214" y1="100" x2="282" y2="100" stroke="currentColor" stroke-width="1.2" marker-end="url(#lad0-arrow)" opacity="0.75"/>
  <g fill="currentColor" text-anchor="middle">
    <text x="248" y="78" font-size="10" font-weight="600">演算を</text>
    <text x="248" y="92" font-size="8.5" opacity="0.8">載せる</text>
  </g>
  <!-- 図1: 代数 -->
  <rect x="288" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="302" y="46" fill="currentColor" font-size="10" opacity="0.8">図1  代数</text>
  <text x="386" y="106" fill="currentColor" font-family="monospace" font-size="12" font-weight="600" text-anchor="middle">Type</text>
  <path d="M 400 94 C 452 52, 320 52, 372 94" fill="none" stroke="currentColor" stroke-width="1.3" marker-end="url(#lad0-arrow)"/>
  <text x="386" y="79" fill="currentColor"  font-size="11" text-anchor="middle">●</text>
  <path d="M 400 116 C 452 158, 320 158, 372 116" fill="none" stroke="currentColor" stroke-width="1.3" marker-end="url(#lad0-arrow)"/>
  <text x="386" y="142" fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="11" text-anchor="middle">e</text>
  <g fill="currentColor" text-anchor="middle">
    <text x="386" y="193" font-size="12" font-weight="600">点 = 台 (集合), 射 = 演算</text>
    <text x="386" y="208" font-size="9.5" opacity="0.85">台 = Type</text>
    <text x="386" y="222" font-size="9.5" opacity="0.85">演算 = ●, e</text>
    <text x="386" y="237" font-size="10" opacity="0.7">第8章まで: この世界 + 法則 = 代数</text>
  </g>
  <line x1="490" y1="100" x2="558" y2="100" stroke="currentColor" stroke-width="1.2" marker-end="url(#lad0-arrow)" opacity="0.75"/>
  <g fill="currentColor" text-anchor="middle">
    <text x="524" y="78" font-size="10" font-weight="600">俯瞰</text>
    <text x="524" y="92" font-size="8.5" opacity="0.8">台を射の集まりに</text>
  </g>
  <!-- 図2: 関数の世界 -->
  <rect x="564" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="578" y="46" fill="currentColor" font-size="10" opacity="0.8">図2  圏 Hask</text>
  <g fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="13" text-anchor="middle">
    <text x="604" y="71">A</text>
    <text x="714" y="71">B</text>
    <text x="660" y="151">C</text>
  </g>
  <g stroke="currentColor" stroke-width="1.3" fill="none">
    <line x1="620" y1="67" x2="698" y2="67" marker-end="url(#lad0-arrow)"/>
    <line x1="708" y1="80" x2="672" y2="136" marker-end="url(#lad0-arrow)"/>
    <line x1="608" y1="80" x2="646" y2="136" marker-end="url(#lad0-arrow)"/>
  </g>
  <g fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="11" text-anchor="middle">
    <text x="659" y="59">f</text>
    <text x="704" y="112">g</text>
    <text x="612" y="112">g∘f</text>
  </g>
  <g fill="currentColor" text-anchor="middle">
    <text x="662" y="193" font-size="12" font-weight="600">点 = 型 (集合), 射 = 関数</text>
    <text x="662" y="208" font-size="9.5" opacity="0.85">Ob = {A, B, C}</text>
    <text x="662" y="222" font-size="9.5" opacity="0.85">Mor = {f, g, g∘f, id, …}</text>
    <text x="662" y="237" font-size="10" opacity="0.7">本章: 射の集まりを台に据える = 圏</text>
  </g>
</svg>

そしてこの見方に立つと, 型引数を持つ **多相データ型** は **関手 (functor)**, 型に依らない一様な多相関数は **自然変換 (natural transformation)** という構造に対応します. すなわち, 本章では次の対応を順に確かめていきます.

| 圏論の概念 | Haskell での対応 |
| --- | --- |
| 対象 (object) | 型 |
| 射 (morphism) | 関数 `a -> b` |
| 関手 (functor) | 型引数を持つ多相データ型 (`fmap` を備える) |
| 自然変換 (natural transformation) | 型に依らない一様な多相関数 `forall a. f a -> g a` |

この対応の裏には, [第8章](fp8.html)から続く 1 本の縦糸があります. **レシピは同じまま, 台を持ち上げる** ことです. モノイドも圏も「組 + 法則 (結合律と単位律)」という同じレシピで定義され, 変わるのは **台に何を据えるか** だけです. 台に据えるものを一段ずつ持ち上げると, 次の階段 (本講義で **俯瞰の階段** と呼ぶ地図) になります (「図」の列は, 図0 (集合)・図1 (代数)・図2 (関数の世界) に続く本章の図の系列です). 本章はこの階段を下から順に上っていきます. 各段は対応する節で実物 (定義とコード) として確かめるので, いまは全体の地図として眺めるだけでかまいません.

| 段 | 図 | 台 (= 前段の「構造を保つ写像」) | 単位 | どこで確かめるか |
| --- | --- | --- | --- | --- |
| モノイド | 図1 | 値 | `mempty` | [第8章](fp8.html) (確認済み) |
| 圏 Hask | 図2 | 関数 (= 集合の写像) | `id` | 本章前半 |
| 圏 Cat | 図3 | 関手 (= 圏の準同型) | 恒等関手 | 「関手」の節 |
| 関手圏 | 図4 | 自然変換 (= 関手どうしの変換) | 恒等自然変換 | 「自然変換」「関手圏」の節・[第10章](fp10.html) |

::: note
[第7章](fp7.html)冒頭の警告で「Haskell の高度な機能は集合論的な理解よりも圏論的な理解のほうが適している. 一旦集合論的に概要を把握し, 後の章で圏論的な解釈を試みる」と述べました. 本章がその「後の章」にあたります. ただし圏論そのものを体系的に扱うのではなく, [第8章](fp8.html)までに作った「組 + 法則」の道具立てを, そのままもう一度使う程度に留めます.
:::

本章は 3 部構成です. 前半 **圏** では, 圏を定義し, Haskell の型と関数がそのまま圏をなすこと, そして圏の準同型 = **関手** までを扱います. 中盤 **多相データ型** (関手のモデルたち) では, 関手のモデル (実例) として多相データ型を一つずつ増やします. 失敗を表す `Maybe`, 二者択一の `Either` ([第7章](fp7.html)で「後の章で扱う」と先送りした型), 既習のリスト, 実務で頻出の辞書 `Map`, その内部構造を確かめるツリーです. それぞれが **なぜ実用で必要になるか** も, ここで一つずつ語ります. 後半では **自然変換** を導入し, 上の対応表を完成させます.

::: note
**講義で辿る道筋**

**今日の到達点:** 関数の合成を法則として読む圏の見方から, `fmap` を「関数を別の世界でも使える関数へ移す対応」として理解する. `Maybe` / `Either` を, 失敗を型で表す実用的な関手として使える.

板書と本文では, 次の順に進みます.

1. [圏の定義](#圏の定義)と[Hask](#圏-hask): 型, 関数, 合成, `id`.
2. [関手](#関手)と[リスト関手と `fmap`](#リスト関手と-fmap): 射の対応と Haskell の `Functor`.
3. [Maybe](#maybe), [Either](#either), [基本操作](#maybe-either-の基本操作): 失敗を明示した計算. [Exercise CH9-2](#exercise-ch9-2), [Exercise CH9-3](#exercise-ch9-3)
4. [自然変換](#自然変換): 型によらない一様な変換. [Exercise CH9-5](#exercise-ch9-5)

必要になったときに本文へ戻る節: Hask の厳密な但し書き, 一点圏, Cat と関手圏, 種・反変位置, `Map`・ツリー, 多項式関手・始代数.
:::

# 圏

## 圏の定義

[第8章](fp8.html)では, モノイドを「組 $(S, \bullet, e)$ であって, 結合律・単位元律という法則を満たすもの」として定義しました. 圏もまったく同じ流儀で定義します. 変わるのは **演算が乗る台** です. モノイドでは「値の集まり $S$」の上に演算 $\bullet$ を載せましたが, 圏では **「射 (写像) の集まり」の上に合成という演算を載せます**. [第7章](fp7.html)で型を集合とみなしたときの「集合のあいだの写像」たち, それ自体を今度は材料 (台) にするわけです.

**圏 (category)** とは, 組

$$(\mathrm{Ob},\ \mathrm{Mor};\ \mathrm{dom},\ \mathrm{cod},\ \circ)$$

であって, 後述の 2 つの法則を満たすものです. 組の成分は次のとおりです.

- **対象 (object) の集まり $\mathrm{Ob}$**: 射に付ける「ラベル」の世界です.
- **射 (morphism) の集まり $\mathrm{Mor}$**: **演算が乗る台**です.
- **1 項演算 $\mathrm{dom}, \mathrm{cod} : \mathrm{Mor} \to \mathrm{Ob}$**: 各射に **入口 (domain)** と **出口 (codomain)** の対象を割り当てます. $\mathrm{dom}\ f = A$ かつ $\mathrm{cod}\ f = B$ のとき, $f : A \to B$ と書きます.
- **2 項演算 $\circ$ (合成, composition)**: ただし, どの 2 射にも使えるわけではありません. 定義域は, [第7章](fp7.html)の内包表記で書ける部分

$$\{(g, f) \mid \mathrm{dom}\ g = \mathrm{cod}\ f\}$$

  すなわち「$f$ の出口と $g$ の入口が一致する対」に限られます. $f : A \to B$ と $g : B \to C$ を合成すると $g \circ f : A \to C$ です ($\mathrm{dom}(g \circ f) = \mathrm{dom}\ f$, $\mathrm{cod}(g \circ f) = \mathrm{cod}\ g$).

満たすべき **法則** は 2 つで, モノイドの結合律・単位元律に対応します.

- **結合律**: $\forall f, g, h.\ \ (h \circ g) \circ f = h \circ (g \circ f)$ (合成が定義できる限り)
- **恒等律 (恒等射の存在)**: $\forall A.\ \exists\, \mathrm{id}_A : A \to A.\ \forall f, g.\ \ \mathrm{id}_A \circ f = f,\ \ g \circ \mathrm{id}_A = g$ (型が合う限り. この $\mathrm{id}_A$ を対象 $A$ の **恒等射 (identity)** といいます)

組の成分を 1 枚の図にすると, こうなります. 点の集合 $\mathrm{Ob}$ の中で, 各射は $\mathrm{dom}$ の割り当てる入口から $\mathrm{cod}$ の割り当てる出口へ渡る 1 本の矢印です. $\mathrm{Mor}$ はその矢印たちを集めた集合で (中央の帯), 恒等射 $\mathrm{id}_A$ (入口と出口がどちらも $A$ の射) もその要素です.

<svg viewBox="0 0 560 348" width="100%" style="max-width: 570px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="圏の定義の組 (Ob, Mor; dom, cod, ∘) を 1 枚にした図. 外枠 = Ob (対象の集まり = 点の集合). その中に左の楕円 dom (射の入口) と右の楕円 cod (射の出口) があり, 左の点 A, B, Y から右の点 C, D, Z へ射 f, g, k, h の矢印が渡る. 中央の縦の帯が Mor (射の集まり) で, 矢印はみなこの帯を通る. A には自分に戻るループ id_A があり, 同じ対象が入口にも出口にもなれることを示す. キャプション: 圏 = 組 (Ob, Mor; dom, cod, ∘) + 結合律・恒等律. dom・cod : Mor → Ob が各射に入口と出口を割り当てる. 合成 ∘ は出口と入口が合う対にだけ定義される.">
  <defs>
    <marker id="def-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="6.5" markerHeight="6.5" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <!-- 外枠 = Ob -->
  <rect x="65" y="22" width="430" height="250" rx="12" fill="none" stroke="currentColor" stroke-width="1.3"/>
  <text x="79" y="42" fill="currentColor" font-size="10" font-weight="600">Ob: 対象の集まり (点)</text>
  <!-- dom / cod の楕円 -->
  <ellipse cx="163" cy="158" rx="72" ry="84" fill="rgba(13,148,136,0.10)" stroke="currentColor" stroke-width="1" opacity="0.85"/>
  <text x="163" y="258" fill="currentColor" font-size="9.5" text-anchor="middle" opacity="0.85">dom (入口)</text>
  <ellipse cx="403" cy="158" rx="68" ry="80" fill="rgba(13,148,136,0.10)" stroke="currentColor" stroke-width="1" opacity="0.85"/>
  <text x="403" y="256" fill="currentColor" font-size="9.5" text-anchor="middle" opacity="0.85">cod (出口)</text>
  <!-- Mor の帯 -->
  <rect x="261" y="58" width="58" height="196" rx="8" fill="none" stroke="currentColor" stroke-width="1" stroke-dasharray="4 3" opacity="0.7"/>
  <text x="290" y="52" fill="currentColor" font-size="9.5" text-anchor="middle" opacity="0.85">Mor: 射の集まり</text>
  <!-- 対象 (点) -->
  <g fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="12.5" text-anchor="middle">
    <text x="146" y="109">A</text><text x="136" y="174">B</text><text x="161" y="229">Y</text>
    <text x="416" y="112">C</text><text x="401" y="179">D</text><text x="421" y="232">Z</text>
  </g>
  <!-- 射 -->
  <g stroke="currentColor" stroke-width="1.2" fill="none">
    <line x1="161" y1="104" x2="399" y2="107" marker-end="url(#def-arrow)"/>
    <line x1="151" y1="166" x2="399" y2="113" marker-end="url(#def-arrow)"/>
    <line x1="151" y1="171" x2="383" y2="175" marker-end="url(#def-arrow)"/>
    <line x1="175" y1="222" x2="402" y2="227" marker-end="url(#def-arrow)"/>
  </g>
  <g fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="11" text-anchor="middle">
    <text x="290" y="98">f</text>
    <text x="290" y="130">k</text>
    <text x="290" y="165">g</text>
    <text x="290" y="216">h</text>
  </g>
  <text x="290" y="243" fill="currentColor" font-size="10" text-anchor="middle" opacity="0.7">…</text>
  <!-- 恒等射 id_A -->
  <path d="M 158 96 C 180 76, 112 76, 134 96" fill="none" stroke="currentColor" stroke-width="1.1" marker-end="url(#def-arrow)"/>
  <text x="146" y="72" fill="currentColor" font-size="10" text-anchor="middle"><tspan font-family="Georgia, 'Times New Roman', serif" font-style="italic">id</tspan><tspan dy="2.5" font-size="7.5" font-family="Georgia, 'Times New Roman', serif" font-style="italic">A</tspan></text>
  <!-- キャプション -->
  <g fill="currentColor" text-anchor="middle">
    <text x="280" y="294" font-size="12" font-weight="600">圏 = 組 (Ob, Mor; dom, cod, ∘) + 結合律・恒等律</text>
    <text x="280" y="311" font-size="9.5" opacity="0.85">Ob = 点の集合 {A, B, C, …} / Mor = 射の集合 {f, g, k, h, id_A, …}</text>
    <text x="280" y="326" font-size="9.5" opacity="0.85">dom・cod : Mor → Ob が各射に入口と出口を割り当てる (dom f = A, cod f = C)</text>
    <text x="280" y="341" font-size="10" opacity="0.7">合成 ∘ は出口と入口が合う対だけ. 同じ対象は入口にも出口にもなれる (id_A は両方が A)</text>
  </g>
</svg>

モノイドと並べると, 対応がはっきり見えます.

| | モノイド ([第8章](fp8.html)) | 圏 |
| --- | --- | --- |
| 台 (演算が乗る集まり) | 値の集合 $S$ | **射の集まり $\mathrm{Mor}$** |
| 演算 | $\bullet$ (2 項. どの 2 要素にも使える) | $\circ$ (2 項. 型の合う対のみ) と $\mathrm{dom}, \mathrm{cod}$ (1 項) |
| 単位 | 単位元 $e$ (全体で 1 つ) | 恒等射 $\mathrm{id}_A$ (**対象ごとに** 1 つ) |
| 法則 | 結合律・単位元律 | 結合律・恒等律 |

量化子の並びにも注目してください. モノイドの単位元は $\exists e.\ \forall x$ で, 全体でたった 1 つの $e$ がすべての $x$ に効くのでした. 圏の恒等射は $\forall A.\ \exists\, \mathrm{id}_A$ で, **対象ごとに 1 つずつ** 用意されます. [第8章](fp8.html)で半群・モノイド・群を量化子の形で区別したのと同じく, 量化子の順序と位置が構造の性格を決めています.

成分の役割も, [第8章](fp8.html)の「組 + 法則」の note と同じに分かれます. **台と演算はデータ** (何を選ぶかで圏そのものが変わります), **法則は性質** (選んだデータについて成り立つかを検査します), そして **恒等射は, あれば $\circ$ から一意に決まります** ($u$ と $u'$ がともに $A$ の恒等射なら $u = u \circ u' = u'$). `mempty` が $\bullet$ から一意に決まったのと同じ理屈で, 恒等射はデータとして独立に選ぶものではなく, 演算の付属品です.

## 圏 Hask

Haskell の型と関数は, この定義をそのまま満たします. 対象を **型**, 射を **関数 `a -> b`** とみなすと, $\mathrm{dom}$ / $\mathrm{cod}$ は「引数の型と返り値の型」, 合成は関数合成 `(.)`, 恒等射は恒等関数 `id` です. 合成 `g . f` が「`f` の返り値の型と `g` の引数の型が一致するときだけ」型検査を通ることは, $\circ$ の定義域の制限そのものです. この圏を慣習的に **Hask** と呼びます.

~~~ haskell
-- 射 = 関数, 合成 = (.), 恒等射 = id
f :: Int -> Int
f = (+ 1)

g :: Int -> Int
g = (* 2)

main :: IO ()
main = do
  print ((g . f) 3)   -- 8   (g (f 3) = g 4 = 8)
  print (id 3)        -- 3   (恒等射)
  print ((f . id) 3)  -- 4   (id . f = f . id = f)
~~~

`(.)` と `id` は [第6章](fp6.html) で関数合成として導入しましたが, 圏論的にはこれが「射の合成」と「恒等射」にあたります. 圏の法則も成り立ちます. `(f . g) . h` と `f . (g . h)` はどちらも `\x -> f (g (h x))` で等しく (結合律), `id . f` と `f . id` はどちらも `f` です (恒等律).

もう一つ, 対象が相異なる具体例で **合成** を図にしてみましょう. 3 つの型 `Bool`, `Int`, `String` を **対象**, それらをつなぐ 2 つの関数を **射** とします.

~~~ haskell
-- 対象 = 型, 射 = 関数.  2 つの射:
--   fromEnum :: Bool -> Int      False → 0, True → 1
--   show     :: Int  -> String   数を文字列へ
showBit :: Bool -> String
showBit = show . fromEnum        -- 合成した射 g . f

main :: IO ()
main = do
  print (fromEnum True)    -- 1     (Bool -> Int)
  print (show (1 :: Int))  -- "1"   (Int -> String)
  print (showBit True)     -- "1"   (show . fromEnum, Bool -> String)
~~~

`show . fromEnum` は, `Bool` から `String` への **1 本の射** です. `True` を渡すと `fromEnum` で `Int` の `1` を経由し, `show` で `"1"` になります. この「対象・射・合成」の関係を描いたのが次の **可換図式 (commutative diagram)** です. `Bool` から `String` へ至る 2 つの経路 (上を回って `fromEnum` してから `show`, あるいは対角の `show . fromEnum` を直接たどる) が **同じ射** になる (これを「図式が **可換** である」といいます) ことを表しています.

<svg viewBox="0 0 460 250" width="100%" style="max-width: 520px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="圏の合成を表す可換図式. 対象 Bool から fromEnum で Int へ, Int から show で String へ矢印が伸び, さらに Bool から String へ直接 show . fromEnum の対角の矢印が伸びる. fromEnum してから show をたどる経路と, 対角の show . fromEnum をたどる経路は, どちらも同じ射 Bool から String を表し, 図式は可換である.">
  <defs>
    <marker id="cd-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <g stroke="currentColor" stroke-width="1.5" fill="none">
    <line x1="104" y1="52" x2="358" y2="52" marker-end="url(#cd-arrow)"/>
    <line x1="388" y1="72" x2="388" y2="181" marker-end="url(#cd-arrow)"/>
    <line x1="80" y1="74" x2="350" y2="184" marker-end="url(#cd-arrow)"/>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="17" font-weight="600" text-anchor="middle">
    <text x="72" y="58">Bool</text>
    <text x="388" y="58">Int</text>
    <text x="388" y="205">String</text>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="13">
    <text x="231" y="42" text-anchor="middle">fromEnum</text>
    <text x="400" y="132" text-anchor="start">show</text>
    <text x="180" y="158" text-anchor="middle">show . fromEnum</text>
  </g>
  <text x="230" y="238" fill="currentColor" font-size="12" text-anchor="middle">2 経路はどちらも同じ射 Bool → String. 図式は可換</text>
</svg>

矢印が **射**, 頂点が **対象**, 対角線が **合成した射** です. 合成 $g \circ f$ とは, まさに「この図で `f` の矢印と `g` の矢印を継いで得られる対角の矢印」にほかなりません.

::: note
**発展: Hask はどこまで含むか, そして厳密には圏でない**

- **Hask は「全部入り」の 1 つの圏です.** 対象は種 `*` を持つ **すべての型**, 射 $\mathrm{Mor}(A, B)$ は **型 `A -> B` の値すべて** で, 名前を付けて定義した関数だけでなく, `\x -> x + 1` のようにその場で書けるものも全部, 1 本ずつ射です. 「どの型とどの関数を入れるか」を選ぶ余地はなく, 言語が決まれば Hask は 1 つに決まります.
- 本文の `Bool`/`Int`/`String` の三角形のような小さな圏は, Hask から対象と射を選んで切り出した **部分圏 (subcategory)** です (恒等射をすべて含み, 合成で閉じるように選びます). 図に描けるのは部分圏ですが, のちの `Functor` クラスが相手にするのは **Hask 全体から Hask 全体への** 関手です.
- **厳密な但し書き**: 本物の Haskell には $\bot$ (`undefined`・無限ループ) が値として存在するため, Hask は圏の公理を厳密には満たしません (たとえば `seq` を使うと恒等律が人工的な例で破れます). 本講義では **$\bot$ を無視した, 全域関数だけからなる理想化された Hask** で考えます. この立場は「厳密には不正確だが, 導かれる結論は実用上正しい」ことが知られており (*fast and loose reasoning is morally correct*, Danielsson ら 2006), [第7章](fp7.html)で部分関数 (`head`) を全域性の欠けとして扱った議論が, ここでも理想化の境目になっています.
:::

## 圏はモノイドの一般化

ここまでの定義を, [第8章](fp8.html)のモノイドとの関係として整理します. モノイドは「結合律 + 単位元律」を満たす演算 `(<>)` と単位元 `mempty` の組, 圏は「結合律 + 恒等律」を満たす合成 `(.)` と恒等射 `id` の組でした.

$$
\underbrace{(h \circ g) \circ f = h \circ (g \circ f)}_{\text{圏の結合律}}, \quad
\underbrace{\mathrm{id} \circ f = f \circ \mathrm{id} = f}_{\text{圏の恒等律}}
$$

実際, 圏は **モノイドを一般化した構造** です. [第8章](fp8.html)のモノイドは, **対象が 1 つだけの圏** (射 = その型の要素, 合成 = `<>`, 恒等射 = `mempty`) とみなせます (なお[第7章](fp7.html)の図1 では射 = **演算** でした. 同じ「一点のまわりのループ」の絵でも, 何を射に据えるかで読み方が変わります. この一点圏の読みでは **要素そのもの** が射です). 対象が 1 つしかなければ「型の合う対だけ」という合成の制限は常に満たされ, 「対象ごとに 1 つ」の恒等射も全体で 1 つに戻ります. 圏の定義が, そのままモノイドの定義に退化するのです. 逆に言えば, 圏とは **モノイドのレシピ (組 + 法則) はそのままに, 台を「値」から「射」へ持ち上げ, 合成の定義域を型で絞ったもの** です. 冒頭の階段の 1 段目 (モノイド) から 2 段目 (圏 Hask) への持ち上げが, まさにこれでした.

言い換えだけでは心もとないので, [第8章](fp8.html)で扱った具体的なモノイドを 1 つ, 実際に圏として読み直してみます. リストのモノイド $([\mathrm{Int}],\ \mathbin{+\!\!\!+},\ [])$ です. 圏の組 $(\mathrm{Ob},\ \mathrm{Mor};\ \mathrm{dom}, \mathrm{cod}, \circ)$ の各成分に, モノイドの部品を次のように割り当てます.

| 圏の成分 | 一点圏として読んだ $([\mathrm{Int}],\ \mathbin{+\!\!\!+},\ [])$ |
| --- | --- |
| 対象 $\mathrm{Ob}$ | $\star$ ただ 1 つ (名前はなんでもよい) |
| 射 $\mathrm{Mor}$ | 各リスト (`[1,2]` も `[3]` も `[]` も) がそれぞれ射 $\star \to \star$ |
| $\mathrm{dom},\ \mathrm{cod}$ | すべての射で $\star$ (対象が 1 つしかないので) |
| 合成 $\circ$ | 連結 `++`. 「型の合う対のみ」の制限は常に満たされる |
| 恒等射 $\mathrm{id}_\star$ | 空リスト `[]` |
| 結合律 | $(xs \mathbin{+\!\!\!+} ys) \mathbin{+\!\!\!+} zs = xs \mathbin{+\!\!\!+} (ys \mathbin{+\!\!\!+} zs)$ |
| 恒等律 | $[] \mathbin{+\!\!\!+} xs = xs \mathbin{+\!\!\!+} [] = xs$ |

同じ内容を図にすると, 左が[第8章](fp8.html)の読み方 (図1 の実例), 右が一点圏としての読み方です.

<svg viewBox="0 0 496 246" width="100%" style="max-width: 506px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="リストのモノイドの 2 つの読み方. 左は第8章の読み方 (図1 の実例) で, 台 = [Int], 演算 = ++ と [] がループの射として載る. 右は一点圏としての読み方で, Ob = {★}, Mor = {[1,2], [3], [], …}: 対象 ★ が 1 つだけあり, 各リストが ★ から ★ への射になる. 合成が ++, 恒等射が []. 中央の矢印は値を射へ読み替える対応.">
  <defs>
    <marker id="opc-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="6.5" markerHeight="6.5" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <!-- 図1 の実例: リストの代数 -->
  <rect x="12" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="26" y="46" fill="currentColor" font-size="10" opacity="0.8">図1 の実例  代数</text>
  <text x="110" y="106" fill="currentColor" font-family="monospace" font-size="12" font-weight="600" text-anchor="middle">[Int]</text>
  <path d="M 124 94 C 176 52, 44 52, 96 94" fill="none" stroke="currentColor" stroke-width="1.3" marker-end="url(#opc-arrow)"/>
  <text x="110" y="79" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">++</text>
  <path d="M 124 116 C 176 158, 44 158, 96 116" fill="none" stroke="currentColor" stroke-width="1.3" marker-end="url(#opc-arrow)"/>
  <text x="110" y="142" fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">[]</text>
  <g fill="currentColor" text-anchor="middle">
    <text x="110" y="193" font-size="12" font-weight="600">点 = 台, 射 = 演算</text>
    <text x="110" y="208" font-size="9.5" opacity="0.85">台 = [Int]</text>
    <text x="110" y="222" font-size="9.5" opacity="0.85">演算 = ++, []</text>
    <text x="110" y="237" font-size="10" opacity="0.7">第8章の読み方: 値の上の演算</text>
  </g>
  <line x1="214" y1="100" x2="282" y2="100" stroke="currentColor" stroke-width="1.2" marker-end="url(#opc-arrow)" opacity="0.75"/>
  <g fill="currentColor" text-anchor="middle">
    <text x="248" y="78" font-size="10" font-weight="600">読み替え</text>
    <text x="248" y="92" font-size="8.5" opacity="0.8">値を射に</text>
  </g>
  <!-- 一点圏として -->
  <rect x="288" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="302" y="46" fill="currentColor" font-size="10" opacity="0.8">一点圏</text>
  <text x="386" y="107" fill="currentColor" font-size="14" text-anchor="middle">★</text>
  <path d="M 400 94 C 452 52, 320 52, 372 94" fill="none" stroke="currentColor" stroke-width="1.3" marker-end="url(#opc-arrow)"/>
  <text x="386" y="79" fill="currentColor" font-family="monospace" font-size="9.5" text-anchor="middle">[1,2]</text>
  <path d="M 400 116 C 452 158, 320 158, 372 116" fill="none" stroke="currentColor" stroke-width="1.3" marker-end="url(#opc-arrow)"/>
  <text x="386" y="142" fill="currentColor" font-family="monospace" font-size="9.5" text-anchor="middle">[3]</text>
  <path d="M 370 100 C 310 84, 310 128, 370 112" fill="none" stroke="currentColor" stroke-width="1.3" marker-end="url(#opc-arrow)"/>
  <text x="334" y="94" fill="currentColor" font-family="monospace" font-size="8.5" text-anchor="middle">[] = id</text>
  <g fill="currentColor" text-anchor="middle">
    <text x="386" y="193" font-size="12" font-weight="600">点 = ★ ただ 1 つ, 射 = 各リスト</text>
    <text x="386" y="208" font-size="9.5" opacity="0.85">Ob = {★}</text>
    <text x="386" y="222" font-size="9.5" opacity="0.85">Mor = {[1,2], [3], [], …}</text>
    <text x="386" y="237" font-size="10" opacity="0.7">合成 = ++, 恒等射 = []</text>
  </g>
</svg>

[第8章](fp8.html)ではモノイドの **台の要素 (値)** だった `[1,2]` や `[3]` が, この読み方では 1 本ずつの **射** になります. 「リストを 2 つ選んで連結する」ことは「射を 2 本選んで合成する」ことにほかならず, モノイドの結合律・単位元律はそのまま圏の結合律・恒等律として読めます. この「モノイド = 一点圏」の読み方は, 次節で **最初の関手を作る材料** としてさっそく使います.

::: note
**発展: アリティは「域の形」に溶ける (演算・値・関係の圏論的整理)**

圏の射は, 定義上すべて「1 入力」($f : A \to B$) です. では [第7章](fp7.html)で整理した $n$ 項演算 ($A^n \to A$) はどこへ行ったのでしょうか. 答えは: **アリティの違いは, 射の種類の違いではなく域 (domain) の形の違いに溶けます**.

- **$n$ 項演算** = 域が直積 $A^n$ の射 $A^n \to A$. 直積は [第7章](fp7.html)の直積そのものです.
- **0 項演算** = 域が $A^0$ の射. $A^0$ は「0 個の直積」= 1 点集合で, Haskell では `()` (Unit 型) です. つまり定数は $() \to A$ という射になります.
- さらに圏論では, **「$A$ の要素」自体を射 $() \to A$ で定義します** (**大域的元** global element といいます). [第7章](fp7.html)で「0 項演算 = 定数 = 要素の指定」と読み替えたものが, ここでは読み替えですらなく **定義の一致** になります. 値・定数・0 項演算は, 圏論ではすべて「Unit からの射」という同じものです.
- **カリー化** ([第5章](fp5.html)) はこの積形のもう 1 つの顔で, `(a, a) -> a` と `a -> a -> a` を行き来させます. 数学の一般論では積形 $A^n \to A$ を使うのが標準です.
- **関係** ($R \subseteq A \times A$, [第7章](fp7.html)) は「`Bool` への射」$A \times A \to \mathrm{Bool}$ として表せます. [第8章](fp8.html)の「関係 = `Bool` への 2 項演算」の読み替えは, `Bool` という対象が圏の中に実在することを使った表現でした.

仕上げに 1 つ予告を. [第7章](fp7.html)の再帰的データ型では, 構成子の束 (たとえば `Nat` の 0 項演算 `Zero` と 1 項演算 `Succ`) を生成作用素 $\Phi$ に畳み込みました. 圏論ではこれをさらに進め, アリティのばらばらな演算たちを直和で束ねて **たった 1 本の射** $\alpha : F(A) \to A$ にします (`Nat` なら $\alpha = [\texttt{Zero}, \texttt{Succ}] : () + \mathrm{Nat} \to \mathrm{Nat}$). この $F$ は本節の次に学ぶ **関手** であり, この形の組 $(A, \alpha)$ は **$F$-代数**, その中で最初に出来上がるものは **始代数 (initial algebra)** と呼ばれます. 「再帰的データ型 = 生成作用素の最小不動点」の圏論版です (名前だけ挙げておきます. [第10章](fp10.html)のモナドの単位 $\eta : \mathrm{Id} \Rightarrow T$ も, 実は「単位対象からの射としての 0 項演算」パターンの再演です).
:::

## 関手

[第8章](fp8.html)の準同型の章では, 構造を保つ写像を「集合の対応であって, **構造の成分を対応先の成分に写すもの**」として一般化し, モノイド準同型 (演算と単位元を保つ)・単調写像 (順序を保つ) などを 1 つの表に並べました. 圏も「組 + 法則」で定義された構造ですから, 同じ発想で「圏の構造 (**合成と恒等射**) を保つ写像」が考えられるはずです. それが **関手 (functor)** で, 冒頭の階段の 3 段目にあたります. ただし, いきなり一般の定義や Haskell の型クラスから入るのはやめて, まず **関手そのものを具体的に 2 つ, 手で構成して** みます. あとで見るとおり, Haskell に型クラスとして用意されているのは関手のうち特殊なケースだけ ([第8章](fp8.html)でマグマや群に標準クラスが無かったのと同じ事情) だからです.

### 最初の関手

1 つ目の材料は, 前節で作ったばかりの **リストの一点圏** です. 行き先として, もう 1 つモノイドを一点圏に読み替えておきます. [第8章](fp8.html)の加算モノイド $(\mathrm{Int}, +, 0)$ です ($\mathrm{Ob} = \{\star\}$, $\mathrm{Mor} =$ 各整数, 合成 $= +$, 恒等射 $= 0$).

この 2 つの圏の間の対応 $F$ を定めます.

- **対象の対応**: $\star \mapsto \star$. どちらの圏も対象は 1 つしかないので, 選びようがありません (自明な対応です).
- **射の対応**: 出発点の圏の射 = リスト $xs$ に, 行き先の圏の射 = その **長さ** `length xs` を割り当てます.

この `length` は, [第7章](fp7.html)の演算の節で「台の外へ出ていく関数」として, [第8章](fp8.html)では **モノイド準同型** として登場したものです. 関手が「保つべきもの」は圏の構造 (合成と恒等射) でした. 出発点の圏の合成は `++`, 恒等射は `[]`. 行き先の圏の合成は `+`, 恒等射は `0`. つまり検査すべきは次の 2 つの等式で, これは第8章で確かめた準同型の 2 法則そのものです.

$$\mathrm{length}\ (xs \mathbin{+\!\!\!+} ys) = \mathrm{length}\ xs + \mathrm{length}\ ys, \qquad \mathrm{length}\ [] = 0$$

~~~ haskell
main :: IO ()
main = do
  -- 射の対応: リスト xs ↦ 整数 length xs
  print (length [10, 20, 30])            -- 3
  -- 合成を保つか: F (xs ++ ys) = F xs + F ys
  print (length ([1,2] ++ [3,4,5]))      -- 5
  print (length [1,2] + length [3,4,5])  -- 5   (一致)
  -- 恒等射を保つか: F [] = 0
  print (length ([] :: [Int]))           -- 0
~~~

これで **関手が 1 つ, 手の中にできました**. 注目してほしいのは, ここに型クラスが 1 つも出てこないことです. 関手とは「対応 + 法則」であって, [第8章](fp8.html)のマグマや群に標準クラスが無かったのと同じように, **この関手のための型クラスは Haskell にありません**. あるのは, ただの関数 `length` と, 検査できる 2 つの等式だけです.

同じ形は, 第8章の準同型ぜんぶに言えます. `stats = mconcat . map singleton` も, リストの一点圏から `Stats` の一点圏への関手です. 第8章で見た「2 つの経路が同じ要約にたどり着く」四角形を, 圏の言葉で読み直してみましょう.

<svg viewBox="0 0 520 282" width="100%" style="max-width: 560px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="モノイド準同型 stats の四角形を圏論的に読み直した図. 左上 (xs, ys), 右上 xs ++ ys, 左下 (stats xs, stats ys), 右下 stats xs &lt;&gt; stats ys の四頂点をもつ. 上下の横矢印 ++ と &lt;&gt; は, 各モノイドを対象が 1 つの圏とみたときの射の合成にあたる. 左右の縦矢印 stats が準同型で, 一方の圏の射を他方の圏の射へ送る. 四角形が閉じること stats (xs ++ ys) = stats xs &lt;&gt; stats ys は stats が合成を保つことを, stats [] = mempty は恒等射を保つことを表す.">
  <defs>
    <marker id="hom-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <g stroke="currentColor" stroke-width="1.5" fill="none">
    <line x1="170" y1="52" x2="352" y2="52" marker-end="url(#hom-arrow)"/>
    <line x1="212" y1="196" x2="300" y2="196" marker-end="url(#hom-arrow)"/>
    <line x1="120" y1="70" x2="120" y2="176" marker-end="url(#hom-arrow)"/>
    <line x1="404" y1="70" x2="404" y2="176" marker-end="url(#hom-arrow)"/>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="15" font-weight="600" text-anchor="middle">
    <text x="120" y="46">(xs, ys)</text>
    <text x="404" y="46">xs ++ ys</text>
    <text x="120" y="202">(stats xs, stats ys)</text>
    <text x="404" y="202">stats xs &lt;&gt; stats ys</text>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="13">
    <text x="261" y="41" text-anchor="middle">++</text>
    <text x="256" y="188" text-anchor="middle">&lt;&gt;</text>
    <text x="112" y="127" text-anchor="end">stats</text>
    <text x="412" y="127" text-anchor="start">stats</text>
  </g>
  <g fill="currentColor" font-size="10.5" opacity="0.72">
    <text x="261" y="63" text-anchor="middle">(合成)</text>
    <text x="256" y="208" text-anchor="middle">(合成)</text>
    <text x="112" y="145" text-anchor="end">(準同型)</text>
    <text x="412" y="145" text-anchor="start">(準同型)</text>
  </g>
  <text x="260" y="264" fill="currentColor" font-size="12" text-anchor="middle">四角形が閉じる = stats が「合成 (++, &lt;&gt;) を保つ」／ stats [] = mempty = 「恒等射を保つ」</text>
</svg>

前節で, モノイドは「対象が 1 つだけの圏」(射がその型の要素, 射の合成がモノイド演算, 恒等射が単位元) とみなせることを見たばかりです. その目でこの四角形を読むと:

- 横の 2 本の矢印 `++` と `<>` は, それぞれの一点圏の **射の合成**.
- 縦の `stats` が, 一方の圏の射 (= リスト) を他方の圏の射 (= 要約) へ送る **射の対応**.
- 四角形が閉じること (`stats (xs ++ ys) = stats xs <> stats ys`) は, `stats` が **合成を保つ** こと.
- もう 1 つの法則 (`stats [] = mempty`) は, `stats` が **恒等射を保つ** こと.

つまり **モノイド準同型とは, 一点圏どうしの間の関手** にほかなりません. [第8章](fp8.html)で「構造を保つ写像」と呼んでいたものの正体が, これです. `mkZ7` も, そして `fromEnum` も, モノイド $(\mathrm{Bool}, \wedge, \mathrm{True})$ から $(\mathrm{Int}, \times, 1)$ への準同型と読めば ($\mathrm{fromEnum}\,(x \wedge y) = \mathrm{fromEnum}\,x \times \mathrm{fromEnum}\,y$ と $\mathrm{fromEnum}\,\mathrm{True} = 1$ が成り立ちます. `Bool` は有限なので全数検査できます), みなこの意味の関手です.

### リストの世界への複写

一点圏どうしでは, 対象の対応が自明でした. 今度は **対象が複数ある圏** を使って, 対象の対応が本気で働く例を作ります. 出発点は, 「圏 Hask」の節で使った小さな圏 $\mathcal{C}$ (対象は `Bool`, `Int`, `String` の 3 つ, 射は `fromEnum`, `show`, その合成 `show . fromEnum`, そして各対象の恒等射 `id`) です (図2 の世界の一部です). 行き先は, それぞれの型の **リスト版** の世界 $\mathcal{D}$ (対象が `[Bool]`, `[Int]`, `[String]` で, 射がその間の関数) とします. どちらも Hask の一部を切り出した圏です.

$\mathcal{C}$ から $\mathcal{D}$ への対応 $F$ を, 2 本立てで定めます. まず **対象の対応** です. 各型に, そのリスト型を割り当てます.

| $\mathcal{C}$ の対象 $A$ | $\mathcal{D}$ の対象 $F\,A$ |
| --- | --- |
| `Bool` | `[Bool]` |
| `Int` | `[Int]` |
| `String` | `[String]` |

次に **射の対応** です. 各関数 $f : A \to B$ に, 「リストの各要素に $f$ を適用する関数」`map f` を割り当てます ([第4章](fp4.html)の `map` です). 行き先の型が $F\,A \to F\,B$ の形にそろっていることを確認してください.

| $\mathcal{C}$ の射 $f : A \to B$ | $\mathcal{D}$ の射 $F\,f : F\,A \to F\,B$ |
| --- | --- |
| `fromEnum :: Bool -> Int` | `map fromEnum :: [Bool] -> [Int]` |
| `show :: Int -> String` | `map show :: [Int] -> [String]` |
| `show . fromEnum :: Bool -> String` | `map (show . fromEnum) :: [Bool] -> [String]` |
| `id :: A -> A` (各対象) | `map id :: [A] -> [A]` |

<svg viewBox="0 0 496 246" width="100%" style="max-width: 500px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="手作りの関手 F の図. 左は出発点の圏 C で, 対象 Bool, Int, String と, 射 fromEnum, show, その合成. 右は行き先の圏 D で, 対象 [Bool], [Int], [String] と, 射 map fromEnum, map show, その合成. 中央の矢印 F が対象と射をまるごと写す: 対象 A は [A] へ, 射 f は map f へ.">
  <defs>
    <marker id="cf-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="6.5" markerHeight="6.5" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <!-- 圏 C: Bool / Int / String -->
  <rect x="12" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="26" y="46" fill="currentColor" font-size="10" opacity="0.7">圏 <tspan font-family="Georgia, 'Times New Roman', serif" font-style="italic">C</tspan></text>
  <g fill="currentColor" font-family="monospace" font-size="12" text-anchor="middle">
    <text x="52" y="71">Bool</text>
    <text x="162" y="71">Int</text>
    <text x="108" y="151">String</text>
  </g>
  <g stroke="currentColor" stroke-width="1.3" fill="none">
    <line x1="74" y1="67" x2="138" y2="67" marker-end="url(#cf-arrow)"/>
    <line x1="154" y1="80" x2="122" y2="136" marker-end="url(#cf-arrow)"/>
    <line x1="60" y1="80" x2="92" y2="136" marker-end="url(#cf-arrow)"/>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="8.5" text-anchor="middle">
    <text x="106" y="59">fromEnum</text>
    <text x="155" y="112">show</text>
  </g>
  <g fill="currentColor" text-anchor="middle">
    <text x="110" y="193" font-size="12" font-weight="600">対象 = 型, 射 = 関数</text>
    <text x="110" y="208" font-size="9.5" opacity="0.85">Ob = {Bool, Int, String}</text>
    <text x="110" y="222" font-size="9.5" opacity="0.85">Mor = {fromEnum, show, …}</text>
    <text x="110" y="237" font-size="10" opacity="0.7">出発点の圏 (図2 の世界)</text>
  </g>
  <!-- F: 対象と射をまるごと写す -->
  <line x1="214" y1="100" x2="282" y2="100" stroke="currentColor" stroke-width="1.4" marker-end="url(#cf-arrow)"/>
  <text x="248" y="78" fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="16" text-anchor="middle">F</text>
  <text x="248" y="92" fill="currentColor" font-size="8" text-anchor="middle" opacity="0.8">対象と射をまるごと写す</text>
  <!-- 圏 D: [Bool] / [Int] / [String] -->
  <rect x="288" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="302" y="46" fill="currentColor" font-size="10" opacity="0.7">圏 <tspan font-family="Georgia, 'Times New Roman', serif" font-style="italic">D</tspan></text>
  <g fill="currentColor" font-family="monospace" font-size="11" text-anchor="middle">
    <text x="330" y="71">[Bool]</text>
    <text x="442" y="71">[Int]</text>
    <text x="384" y="151">[String]</text>
  </g>
  <g stroke="currentColor" stroke-width="1.3" fill="none">
    <line x1="356" y1="67" x2="418" y2="67" marker-end="url(#cf-arrow)"/>
    <line x1="434" y1="80" x2="402" y2="136" marker-end="url(#cf-arrow)"/>
    <line x1="328" y1="80" x2="360" y2="136" marker-end="url(#cf-arrow)"/>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="7.5" text-anchor="middle">
    <text x="386" y="58">map fromEnum</text>
    <text x="446" y="112">map show</text>
  </g>
  <g fill="currentColor" text-anchor="middle">
    <text x="386" y="193" font-size="12" font-weight="600">対象 = リスト型, 射 = map f</text>
    <text x="386" y="208" font-size="9.5" opacity="0.85">Ob = {[Bool], [Int], [String]}</text>
    <text x="386" y="222" font-size="9.5" opacity="0.85">Mor = {map fromEnum, map show, …}</text>
    <text x="386" y="237" font-size="10" opacity="0.7">行き先の圏 (リストの世界)</text>
  </g>
</svg>

これで対応そのものは定まりましたが, まだ「構造を保つ」ことを検査していません. 圏の構造の成分は合成と恒等射でしたから, 確かめるべきは 2 つです. $F$ は恒等射を恒等射に写すか ($F\,\mathrm{id} = \mathrm{id}$), 合成してから写しても写してから合成しても同じか ($F\,(\mathtt{show} \circ \mathtt{fromEnum}) = F\,\mathtt{show} \circ F\,\mathtt{fromEnum}$). どちらも実行して確かめられます.

~~~ haskell
main :: IO ()
main = do
  -- 射の対応: fromEnum ↦ map fromEnum  ([Bool] から [Int] への射になる)
  print (map fromEnum [False, True])               -- [0,1]
  -- 恒等射を保つか: F id = id
  print (map id [False, True])                     -- [False,True]  (id と同じ)
  -- 合成を保つか: F (show . fromEnum) = F show ∘ F fromEnum
  print (map (show . fromEnum) [False, True])      -- ["0","1"]
  print ((map show . map fromEnum) [False, True])  -- ["0","1"]  (一致)
~~~

一致しました. 各対象に「そのリスト版」を, 各射に「各要素への適用」を割り当てる. これで, 圏 $\mathcal{C}$ から圏 $\mathcal{D}$ への **関手が 1 つ, 手の中にできました**.

### 一般の定義

いまの 2 つの構成から個別の事情を取り除くと, そのまま関手の一般定義になります.

圏は $\mathrm{Ob}$ と $\mathrm{Mor}$ という **2 つのソート** (2 種類の台) を持つ構造でした. だから, 構造を保つ写像も **ソートごとに 1 本ずつ** 用意するのが筋です. これは[第8章](fp8.html)の準同型 (台の対応 1 本 + 保存則) の, 2 ソート版にあたります.

**関手 (functor)** とは, 圏 $\mathcal{C}$ から圏 $\mathcal{D}$ への **写像の組**

$$F = (F_{\mathrm{Ob}},\ F_{\mathrm{Mor}}), \qquad F_{\mathrm{Ob}} : \mathrm{Ob}_{\mathcal{C}} \to \mathrm{Ob}_{\mathcal{D}}, \quad F_{\mathrm{Mor}} : \mathrm{Mor}_{\mathcal{C}} \to \mathrm{Mor}_{\mathcal{D}}$$

であって, 圏の **演算ごとに保存則を 1 本ずつ** 満たすものです (**関手則**).

- **入口・出口を保つ**: $f : A \to B$ ならば $F_{\mathrm{Mor}}\,f : F_{\mathrm{Ob}}\,A \to F_{\mathrm{Ob}}\,B$ ($\mathrm{dom}$・$\mathrm{cod}$ の保存)
- **合成を保つ**: $F_{\mathrm{Mor}}\,(g \circ f) = F_{\mathrm{Mor}}\,g \circ F_{\mathrm{Mor}}\,f$
- **恒等射を保つ**: $F_{\mathrm{Mor}}\,(\mathrm{id}_A) = \mathrm{id}_{F_{\mathrm{Ob}}\,A}$

圏の成分と関手の成分は, こうして **1 対 1 に対応** します. ソートには写像を, 演算には保存則を, 1 本ずつです.

| 圏の成分 | 関手が持つもの |
| --- | --- |
| $\mathrm{Ob}$ (ソート 1) | 写像 $F_{\mathrm{Ob}}$ (対象の対応) |
| $\mathrm{Mor}$ (ソート 2) | 写像 $F_{\mathrm{Mor}}$ (射の対応) |
| $\mathrm{dom},\ \mathrm{cod}$ (1 項演算) | 入口・出口の保存則 |
| $\circ$ (2 項演算) | 合成の保存則 |
| $\mathrm{id}$ (単位) | 恒等射の保存則 |

ふだんは添字を省き, どちらの写像も同じ文字 $F$ で書きます ($F\,A$, $F\,f$). 本章でも以降はそう書きますが, **中身は 2 本の写像の組** であることを覚えておいてください.

手作りした 2 つの関手がこの形をしていることを見比べてみてください. `length` は対象の対応が自明 ($\star \mapsto \star$) で射の対応が `length`, 複写の $F$ は対象の対応がリスト版 (1 つ目の表)・射の対応が `map` (2 つ目の表). 法則はどちらもコードで検査したとおりです. [第8章](fp8.html)の一般化表に, もう 1 行加わったと言ってもかまいません.

| 構造 | 構造を保つ写像 | 保つもの |
| --- | --- | --- |
| モノイド $(S, \bullet, e)$ | モノイド準同型 | 演算と単位元 |
| 順序集合 $(A, \le)$ | 単調写像 | 順序 |
| 圏 $(\mathrm{Ob}, \mathrm{Mor};\ \mathrm{dom}, \mathrm{cod}, \circ)$ | **関手** $(F_{\mathrm{Ob}}, F_{\mathrm{Mor}})$ | 入口・出口, 合成, 恒等射 |

ズームアウトの図で言えば, 冒頭の階段の 3 段目に来たことになります. 図2 (点 = 型, 射 = 関数の世界) から一歩引くと, 図2 全体が 1 つの点 (= 圏) に潰れ, 点と点を結ぶ新しい射が現れます. いま手作りした $F$ は, この「圏どうしを結ぶ射」の実物です (図3).

<svg viewBox="0 0 466 246" width="100%" style="max-width: 476px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="関手の導入図. 図2: 圏 Hask. Ob = {A, B, C}, Mor = {f, g, g∘f, id, …} の世界. 一歩引くと図3: 圏 Cat. Ob = {C, D, …}, Mor = {F, G∘F, Id, …} (合成 = 関手の合成, 恒等射 = 恒等関手). 図2 全体が 1 つの点 (圏) に潰れ, 圏と圏を結ぶ新しい射 F が現れる. 点 = 圏, 射 = 関手.">
  <defs>
    <marker id="lad1-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="6.5" markerHeight="6.5" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <!-- 図2: 関数の世界 -->
  <rect x="12" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="26" y="46" fill="currentColor" font-size="10" opacity="0.8">図2  圏 Hask</text>
  <g fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="13" text-anchor="middle">
    <text x="52" y="71">A</text>
    <text x="162" y="71">B</text>
    <text x="108" y="151">C</text>
  </g>
  <g stroke="currentColor" stroke-width="1.3" fill="none">
    <line x1="68" y1="67" x2="146" y2="67" marker-end="url(#lad1-arrow)"/>
    <line x1="156" y1="80" x2="120" y2="136" marker-end="url(#lad1-arrow)"/>
    <line x1="56" y1="80" x2="94" y2="136" marker-end="url(#lad1-arrow)"/>
  </g>
  <g fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="11" text-anchor="middle">
    <text x="107" y="59">f</text>
    <text x="152" y="112">g</text>
    <text x="60" y="112">g∘f</text>
  </g>
  <g fill="currentColor" text-anchor="middle">
    <text x="110" y="193" font-size="12" font-weight="600">点 = 型 (集合), 射 = 関数</text>
    <text x="110" y="208" font-size="9.5" opacity="0.85">Ob = {A, B, C}</text>
    <text x="110" y="222" font-size="9.5" opacity="0.85">Mor = {f, g, g∘f, id, …}</text>
    <text x="110" y="237" font-size="10" opacity="0.7">圏 Hask (これまでの世界)</text>
  </g>
  <line x1="214" y1="100" x2="282" y2="100" stroke="currentColor" stroke-width="1.2" marker-end="url(#lad1-arrow)" opacity="0.75"/>
  <g fill="currentColor" text-anchor="middle">
    <text x="248" y="78" font-size="10" font-weight="600">一歩引く</text>
  </g>
  <!-- 図3: 点 = 圏, 射 = 関手 -->
  <rect x="258" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="272" y="46" fill="currentColor" font-size="10" opacity="0.8">図3  圏 Cat</text>
  <g fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.4">
    <circle cx="308" cy="97" r="17"/>
    <circle cx="404" cy="97" r="17"/>
  </g>
  <g fill="currentColor" opacity="0.8">
    <circle cx="302" cy="92" r="1.5"/><circle cx="314" cy="92" r="1.5"/><circle cx="308" cy="104" r="1.5"/>
    <circle cx="398" cy="92" r="1.5"/><circle cx="410" cy="92" r="1.5"/><circle cx="404" cy="104" r="1.5"/>
  </g>
  <g stroke="currentColor" stroke-width="0.7" opacity="0.7">
    <line x1="303" y1="92" x2="312" y2="92"/><line x1="306" y1="94" x2="308" y2="102"/>
    <line x1="399" y1="92" x2="408" y2="92"/><line x1="402" y1="94" x2="404" y2="102"/>
  </g>
  <line x1="330" y1="97" x2="382" y2="97" stroke="currentColor" stroke-width="1.4" marker-end="url(#lad1-arrow)"/>
  <text x="356" y="84" fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="15" text-anchor="middle">F</text>
  <g fill="currentColor" font-size="10.5" text-anchor="middle" opacity="0.8">
    <text x="308" y="132">圏 <tspan font-family="Georgia, 'Times New Roman', serif" font-style="italic">C</tspan></text>
    <text x="404" y="132">圏 <tspan font-family="Georgia, 'Times New Roman', serif" font-style="italic">D</tspan></text>
  </g>
  <g fill="currentColor" text-anchor="middle">
    <text x="356" y="193" font-size="12" font-weight="600">点 = 圏, 射 = 関手</text>
    <text x="356" y="208" font-size="9.5" opacity="0.85">Ob = {C, D, …}</text>
    <text x="356" y="222" font-size="9.5" opacity="0.85">Mor = {F, G∘F, Id, …}</text>
    <text x="356" y="237" font-size="10" opacity="0.7">図2 全体が 1 つの点に潰れる</text>
  </g>
</svg>

### 圏の圏 Cat

図3 のラベルには **圏 Cat** と書きました. 「圏」と名乗るからには, この図の世界自身が圏の定義 (組 + 法則) を満たしていなければなりません. 点 = 圏, 射 = 関手, までは決まっています. 足りない成分は **射の合成** と **恒等射** の 2 つで, どちらもいま手元にある材料から作れます.

**関手は合成できます.** 関手 $F : \mathcal{C} \to \mathcal{D}$ と $G : \mathcal{D} \to \mathcal{E}$ があれば, 対象の対応どうし・射の対応どうしをそのまま継いだ対応

$$G \circ F : \mathcal{C} \to \mathcal{E}, \qquad A \mapsto G\,(F\,A), \qquad f \mapsto G\,(F\,f)$$

もまた関手です. 関手則は「まず $F$ が保ち, つづけて $G$ が保つ」ので, そのまま引き継がれます. 実物も作れます. 複写の関手 $F$ (対象は `Bool ↦ [Bool]`, 射は `f ↦ map f`) の行き先の世界 $\mathcal{D}$ に, もう一度複写の関手 $F'$ (対象は `[Bool] ↦ [[Bool]]`) を重ねると, 合成 $F' \circ F$ は対象を 2 重リストへ (`Bool ↦ [[Bool]]`), 射を 2 重の `map` へ (`f ↦ map (map f)`) 写す関手になります.

~~~ haskell
ghci> map (map fromEnum) [[False, True], [True]]
[[0,1],[1]]
~~~

この「同じ形の関手を重ねた合成」は, [第10章](fp10.html)で `m (m a)` (モナドの演算の材料 $T \circ T$) として主役になります.

**恒等関手 (identity functor).** 各対象を自分自身に ($A \mapsto A$), 各射を自分自身に ($f \mapsto f$) 対応させる対応 $\mathrm{Id}_{\mathcal{C}} : \mathcal{C} \to \mathcal{C}$ は, 何もしないがゆえに関手則を自明に満たす関手です. そして関手の合成に対して $G \circ \mathrm{Id}_{\mathcal{C}} = G$, $\mathrm{Id}_{\mathcal{D}} \circ F = F$. つまり **合成の単位** として振る舞います. 恒等関数 `id` が合成 `(.)` の単位だったことの, 1 段上での再演です.

これで成分が揃ったので, 圏の定義と照合できます. $\mathrm{Ob} = \{\mathcal{C}, \mathcal{D}, \dots\}$ (圏たち), $\mathrm{Mor} = \{F,\ G,\ G \circ F,\ \mathrm{Id}_{\mathcal{C}}, \dots\}$ (関手たち), 合成 = 関手の合成, 恒等射 = 恒等関手. 結合律は「対応を順に継ぐだけ」なので関数合成と同じ理屈で成り立ち, 恒等律はいま確かめたとおりです. つまり **図3 の世界は, それ自身が圏の定義を満たす 1 つの圏** です. 対象が圏, 射が関手の **圏の圏 (category of categories) Cat** です (集合の大きさに関わる厳密な但し書きは, Hask の発展 note と同じく理想化して省きます). 冒頭の階段の 3 段目「圏 Cat: 台 = 関手, 単位 = 恒等関手」は, これで回収されました. 圏を調べるために作った道具 (関手) が, そのまま一段上の圏の射になる. この入れ子こそ, 同じレシピで台を持ち上げ続けられる理由です.

### リスト関手と fmap

2 つ目に手作りした $F$ は, 対象 3 つ・射 4 本に制限した, いわば **ミニチュアのリスト関手** でした. この制限を外し, **すべての型** `a` に `[a]` を, **すべての関数** `f :: a -> b` に `map f` を割り当てると, Hask から Hask 自身への関手 (**リスト関手**) になります.

- **対象の対応**: 型 `a` を型 `[a]` へ. これを担うのが型構築子 `[]` です.
- **射の対応**: 関数 `f :: a -> b` を `map f :: [a] -> [b]` へ.

関手則も, 手作り版で検査した等式の一般形で, `map` について経験的に知っている事実の言い換えです. `map id` は「各要素に何もしない」ので `id` と同じ (恒等射を保つ). `map (g . f)` は「各要素に `f` してから `g` する」を一度に行うもので, `map f` してから `map g` するのと同じ (合成を保つ).

$$\mathrm{map}\ \mathrm{id} = \mathrm{id}, \qquad \mathrm{map}\ (g \circ f) = \mathrm{map}\ g \circ \mathrm{map}\ f$$

Haskell では, この「射の対応」の部分を型クラスにしています. それが `Functor` クラスで, 射の対応の名前が `fmap` です. 注意してほしいのは抽象度の段差です. **`fmap` は個々の関手そのものではなく, あらゆる関手の「射の対応」を同じ名前で呼べるようにした一般化** であり, 関手より 1 段抽象的な道具です. 先に関手そのもの (手作りの $F$) を持っているからこそ, この一般化を「何の一般化か」を見失わずに読めます.

~~~ haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
~~~

`fmap` は「`a` から `b` への関数」を「`f a` から `f b` への関数」に持ち上げます. これがまさに「射 $f : a \to b$ を $F\,f : f\,a \to f\,b$ に対応させる」操作です. 関手が満たすべき **関手則** は, 圏の恒等射と合成を保つことを Haskell の言葉に直したものです.

$$\mathrm{fmap}\ \mathrm{id} = \mathrm{id}, \qquad \mathrm{fmap}\ (g \circ h) = \mathrm{fmap}\ g \circ \mathrm{fmap}\ h$$

つまり `fmap` は, リスト専用だった `map` を **任意の関手に一般化** したものです. `map` の型 `(a -> b) -> [a] -> [b]` の `[]` を一般の型構築子 `f` で置き換えると, ちょうど `fmap :: (a -> b) -> f a -> f b` になります.

$$\underbrace{(a \to b) \to [a] \to [b]}_{\text{map (リスト専用)}} \;\;\Longrightarrow\;\; \underbrace{(a \to b) \to f\,a \to f\,b}_{\text{fmap (任意の関手)}}$$

読み方はここまでと同じで, **`fmap` は「`f` を適用する前の世界で使う関数 `a -> b` を, 適用した後の世界で使える関数 `f a -> f b` に変換する」道具** です. 最初の例と見比べてください. `length` が「リストの世界の射」を「整数の世界の射」に写したように, `fmap` は「`a` の世界の射」を「`f a` の世界の射」に写します. 射の対応 = 関数の変換です. 変換のしかたは関手ごとに決まります. リストなら「各要素に適用する関数」へ, 後で見る `Maybe` なら「`Just` の中身には適用し `Nothing` は素通しする関数」へ, 木なら「各ノードに適用する関数」へ.

リストの `fmap` の中身は, [第4章](fp4.html) の `map` の再帰定義をそのまま書いたものです. 組込みのインスタンスと同じ定義を `fmapList` として書き下し, `fmap`・`map` と一致することを確かめます.

~~~ haskell
-- リストの fmap の中身 = 第4章の map の再帰定義そのもの
fmapList :: (a -> b) -> [a] -> [b]
fmapList _ []       = []
fmapList f (x : xs) = f x : fmapList f xs

main :: IO ()
main = do
  print (fmapList (* 2) [1, 2, 3])  -- [2,4,6]
  print (fmap     (* 2) [1, 2, 3])  -- [2,4,6]   (組込みの fmap = map)
  print (map      (* 2) [1, 2, 3])  -- [2,4,6]
~~~

3 つの結果はすべて一致します. `fmapList f` は「要素に使う関数 `f`」を「リスト全体に使える関数」へ変換したもので, 変換後の関数は空リストには何もせず, `(x : xs)` の各要素だけを `f` で置き換えます. リストの長さと並びが変わらないこと. これが, リスト関手が構造 (合成と恒等射) を保つことの目に見える現れです.

ただし, ここで押さえておいてほしいのは, **`Functor` クラスに載る関手は, 関手全体のごく一部** だということです. クラスの形 (`f` が型構築子, `fmap` が 1 本の多相関数) から, 載るための条件が 3 つ読み取れます.

1. **域も余域も Hask**: 表せるのは **Hask から Hask 自身への関手 (自己関手)** だけです. 最初に作った `length` (一点圏 → 一点圏) は, 立派な関手ですが `Functor` インスタンスにはなれません. 両端の圏が Hask ではないからです.
2. **対象の対応が型構築子で書ける**: $F(A) = f\,A$ という一様な形 (種 `f :: * -> *`) に限られます. 「`Int` は `Bool` へ, `String` は `Int` へ」のような場当たりな対象の対応は書けません.
3. **射の対応がパラメトリック多相 1 本**: `fmap` は, 型 `a`, `b` を覗いて型ごとに違う変換をすることができません.

[第8章](fp8.html)のマグマ・群のときと同じ「標準クラスの有無」で整理すると, こうなります.

| 関手 | Haskell での受け皿 | 例 |
| --- | --- | --- |
| 一点圏 → 一点圏 (= モノイド準同型) | クラスなし: 関数 + 法則 (マグマ・群と同じ立場) | `length`, `stats`, `mkZ7` |
| 小さな圏どうしの関手 | クラスなし | 複写の $F$ |
| **Hask → Hask, 対象対応 = 型構築子, 射対応 = パラメトリック** | **`Functor` クラス (`fmap`)** | リスト, `Maybe`, `Either a`, `Tree` |
| 反変版 (射の向きを逆に写す関手) | 別クラス `Contravariant` (発展) | 述語 `Pred` の仲間 |

同じ「関手」という言葉が, どの圏を選ぶかで `length` も `fmap` も指します. この区別がつくと, [第8章](fp8.html)の代数 (モノイドと準同型) と本章の圏論 (圏と関手) が, ひと続きの話として見えてきます.

::: note
**よく見る「`f` は箱」という説明について.** 入門記事ではしばしば, `Functor` を「箱 (容器) の中身に関数を適用するもの」という比喩で説明します. 本講義ではこの比喩を **採用しません**. 理由は 2 つあります. (1) 比喩が成り立たないインスタンスがあるからです. たとえば `f a = r -> a` (「`r` を読んで `a` を返す関数」を包んだ型. `fmap` は関数合成) は立派な `Functor` ですが箱ではありませんし, 「任意の値を箱に入れる操作」`a -> f a` は `Functor` の要件ですらありません (それが要求されるのは[第10章](fp10.html)の `pure` (モナドの単位 $\eta$) からです). (2) より一貫した読み方がすでに手元にあるからです. 関手とは **射の対応** であり, `fmap` は「適用前の世界の関数を, 適用後の世界で使える関数へ変換するもの」. この読み方なら, `length` (一点圏どうし) にも `Maybe` にも `r -> a` にも, 同じ言葉がそのまま通ります.
:::

::: warn
`Functor` クラスも, `Semigroup` などと同様に **関手則をコンパイラは検査しません**. 関手則を破る `fmap` (たとえば木の形を変えてしまうもの) を書いてもコンパイルは通ります. 法則を守るのはプログラマの責任です ([第8章](fp8.html)のコラムで触れた QuickCheck で, `fmap id == id` などを性質として確認できます).
:::

::: note
`fmap` には中置演算子 `(<$>)` という別名があり, `(<$>) = fmap` です. `(+1) <$> Just 3` は `fmap (+1) (Just 3)` と同じで `Just 4` を返します. 関数適用 `$` の関手版という見立てで, 実務ではこちらもよく使われます.
:::

# 多相データ型

圏・関手という骨組みは手に入りました. ここからは主役を **モデル (実例)** に移します. [第8章](fp8.html)で「モノイド」という 1 つの構造に対して数の加算・リスト連結・`Stats` という複数のモデルを並べたように, 本部では **「Hask から Hask への関手」のモデル** を一つずつ増やしていきます. 素材は **多相データ型** です. まず型引数を持つデータ型の文法を整え, そのうえで `Maybe` / `Either` / リスト / `Map` / ツリーを順に見ます.

## 型構築子と種

関手のデータ部は, 圏の 2 ソートに対応する 2 本の写像 (対象の対応 $F_{\mathrm{Ob}}$ と射の対応 $F_{\mathrm{Mor}}$) の組でした. Haskell ではこの 2 本が **言語の別の層** に住みます: 射の対応は値の層の関数 `fmap`, そして **対象 (型) の対応** を担う文法が, 型の層の **型引数を持つデータ型** (型構築子) です.

これまで定義してきたデータ型は, `MyDogs` や `Color` のように中身の型が固定されていました. これに対し, **中身の型を後から決められる** データ型を作れます. 型の定義に **型変数** (型引数) を持たせるのです.

~~~ haskell
data Box a  = Box a          -- 任意の型 a を 1 つ包む箱
data Pair a b = Pair a b     -- 型 a と型 b を 1 つずつ持つ組
~~~

`data Box a = Box a` の左辺 `Box a` の `a` が **型引数** です. これは「`a` をどの型にするかは, 使うときに決める」ことを表します. `Box Int` なら `Int` を包む箱, `Box String` なら文字列を包む箱になります. このように型引数を持つデータ型を **多相データ型 (polymorphic data type)** といいます.

ここで, 左辺の `Box` と右辺の `Box` は役割が違う点に注意してください.

- 左辺の `Box` は **型構築子 (type constructor)** です. それ自体は型ではなく, 型 `a` を 1 つ受け取って `Box a` という型を作ります. これを **種 (kind)** という記法で `Box :: * -> *` と書きます (`*` が「具体的な型」を表し, 「型を 1 つ受け取って型を返す」ことを `* -> *` と表現します).
- 右辺の `Box` は **データ構築子 (data constructor)** です. こちらは値を 1 つ受け取って `Box a` 型の値を作る関数で, 型は `Box :: a -> Box a` です.

`Pair a b` は型引数を 2 つ取るので, 型構築子の種は `Pair :: * -> * -> *` です. このように, 型引数の個数が種の矢印の本数に対応します.

実は, すでに使ってきた **リスト `[a]`** も多相データ型です. `[]` が型構築子 (`[] :: * -> *`), 要素の型 `a` を受け取って `[a]` 型を作ります. `[Int]`, `[Char]`, `[Bool]` がすべて同じ `[]` から作られるのは, リストが「中身の型を後から決められる」多相データ型だからです.

種の記法で言い直すと, 関手 (Hask → Hask) のデータ部との対応はこうなります. **種 `* -> *` の型構築子が「対象の対応」** (型 `a` を型 `f a` へ) を与え, そこに **「射の対応」`fmap` を関手則を満たすように与えられれば**, `Functor` のインスタンス (関手のモデル) になります. 前節のリストがまさにこの形でした (`[]` が対象の対応, `map` が射の対応).

::: warn
**種が `* -> *` なら必ず関手になれる, わけではありません.** 多相データ型は関手の **候補** です. たとえば

~~~ haskell
newtype Pred a = Pred (a -> Bool)   -- a の述語 (a を受け取って判定する)
~~~

は種 `Pred :: * -> *` を持ちますが, `fmap :: (a -> b) -> Pred a -> Pred b` を書くことはできません. 中身が「`a` を **受け取る** 関数」であり, 型引数 `a` が矢印の **左側 (反変の位置)** に現れているからです. `a -> Bool` から `b -> Bool` を作るには, 手持ちの `f :: a -> b` では向きが合わず, 逆向きの `b -> a` が必要になります. [第7章](fp7.html)の再帰的データ型の発展 note で「矢印の左に自分が現れると生成の道具立てが壊れる」ことに触れましたが, ここでも矢印の左 = 反変の位置が, (共変の) 関手の資格を奪います. 以降で見るモデルたちは, 型引数がすべて「値として持つ」位置 (共変の位置) に現れる型です.
:::

::: note

### Exercise CH9-1

**自作多相型 `Box` を Functor にする**

値を 1 つ包む多相データ型 `Box a` を定義し, `Functor` インスタンスを実装してください. `fmap f` は「`a` に使う関数 `f`」を「`Box a` に使える関数」へ変換します. また, 中身を取り出す関数 `unBox :: Box a -> a` も定義してください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (unBox (fmap (+ 1) (Box 10)))      -- 11
  print (unBox (fmap show (Box (42 :: Int))))  -- "42"
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data Box a = Box a deriving Show

instance Functor Box where
  fmap f (Box x) = Box (f x)

unBox :: Box a -> a
unBox (Box x) = x

main :: IO ()
main = do
  print (unBox (fmap (+ 1) (Box 10)))          -- 11
  print (unBox (fmap show (Box (42 :: Int))))  -- "42"
~~~

`fmap f (Box x) = Box (f x)` は, 「`a` に使う関数 `f`」を「`Box a` に使える関数」へ変換しています. 変換後の関数は `Box` の構造を保ち, 中身 `x` だけを `f` で置き換えます. `fmap id (Box x) = Box x` で恒等射が保たれていることも確認できます.

</details>

:::

## Maybe

[第4章](fp4.html)・[第7章](fp7.html)で何度か「`Maybe` は後の章で扱う」と先送りしてきました. ここで回収します.

[第7章](fp7.html)で関数を特別な関係として扱った節では, `head :: [a] -> a` のような **部分関数** の問題を見ました. 空リストという「出力が定義されていない入力」があるのに, 型の上では「必ず答えを返す」かのように見えてしまい, 適用すると実行時エラーで停止するのでした. 直和型のレコードセレクタ (同じく[第7章](fp7.html)「レコード構文」節の `dogAge`) も同様です. そして同じ節で, 対処の方向は 2 つあることを整理しました. (1) **定義域を型で絞る** (スマートコンストラクタ) と, (2) **出力を広げる** です. `Maybe` は, この **「出力を広げる」道の主役** です.

`Maybe` は, 「答えを返せないかもしれない」ことを **型で表す** ための多相データ型です. 定義は次の通りで, [第7章](fp7.html)の **直和型** に型引数 `a` を付けたものです.

~~~ haskell
data Maybe a = Nothing | Just a
~~~

- `Nothing` は「値が無い (失敗した)」ことを表すコンストラクタです. 引数を取りません.
- `Just a` は「値 `a` がある (成功した)」ことを表すコンストラクタで, 中身 `a` を 1 つ包みます.

型 `Maybe a` は「`a` の値が 1 つあるか, 何も無いかのいずれか」を表します. たとえば `Maybe Int` の値は `Just 3` や `Just (-1)` や `Nothing` です. 失敗しうる計算の結果を `Maybe` で包むと, **失敗の可能性が型に現れる** ため, 呼び出す側はパターンマッチで両方の場合に対処せざるをえなくなります.

集合論的には, `Just` でくるんだ `a` の値全体に, 値を持たない `Nothing` を 1 つ足したものなので,

$$\mathrm{Maybe}\ a = \{\mathrm{Nothing}\} \cup \{\mathrm{Just}\ x \mid x \in a\}$$

となります. これは $a$ の要素数に 1 を足した集合, すなわち [第7章](fp7.html)の直和の記法で $a + 1$ (要素 1 つの集合 `{Nothing}` との直和) です.

`error` で停止していた `head` を, `Maybe` を返す **全域関数** に書き直してみます. 同様に, 失敗しうる割り算も `Maybe` で安全にできます.

~~~ haskell
-- 空リストでも停止しない安全な head
safeHead :: [a] -> Maybe a
safeHead []      = Nothing
safeHead (x : _) = Just x

-- 0 で割るときは Nothing を返す安全な割り算
safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

main :: IO ()
main = do
  print (safeHead [1, 2, 3 :: Int])  -- Just 1
  print (safeHead ([] :: [Int]))     -- Nothing
  print (safeDiv 10 2)               -- Just 5
  print (safeDiv 10 0)               -- Nothing
~~~

`safeHead []` が `Nothing` を返すので, 空リストでも停止しません. 型 `[a] -> Maybe a` を見るだけで「失敗しうる」ことが分かり, 呼び出し側は `Just` と `Nothing` の両方を処理する必要があります. 結果を使う側はパターンマッチで取り出します.

~~~ haskell
describeMaybe :: Maybe Int -> String
describeMaybe Nothing  = "値なし"
describeMaybe (Just n) = "値は " ++ show n

main :: IO ()
main = do
  putStrLn (describeMaybe (safeDiv 10 2))  -- 値は 5
  putStrLn (describeMaybe (safeDiv 10 0))  -- 値なし
~~~

`Nothing` のケースを書き忘れると GHC が網羅性の警告を出すため, 「失敗の処理を忘れる」ミスをコンパイル時に気づけます. これが, 部分関数を `Maybe` で書き換える最大の利点です.

::: warn
`Maybe` は「失敗するかもしれない 1 つの値」を表しますが, **なぜ失敗したか** (理由) は持てません. `Nothing` はただの「値なし」です. 失敗の理由を伴わせたい場合は, 次節の `Either` を使います.
:::

### Maybe は関手

`Maybe` は種 `Maybe :: * -> *` の型構築子で, 組込みで `Functor` のインスタンスです. `fmap` は「`Just` の中身に関数を適用し, `Nothing` はそのまま返す」操作になります.

~~~ haskell
main :: IO ()
main = do
  print (fmap (+ 1) (Just 3))            -- Just 4
  print (fmap (+ 1) (Nothing :: Maybe Int))  -- Nothing
~~~

`fmap (+1) (Just 3)` が `Just 4` になり, `Nothing` には何も起きません. `fmap (+1)` は「`Int` に使う関数 `(+1)`」を「`Maybe Int` に使える関数」へ変換したもので, 変換後の関数は `Just` の中身には元の関数を適用し, `Nothing` は素通しします. リストの `fmap` が長さと並びを保ったのと同じく, 「`Nothing` か `Just` か」という **構造は変わりません**.

::: note

### Exercise CH9-2

**安全な探索関数 `safeLast` / `lookupKey` (Maybe)**

1. リストの **末尾** の要素を返す安全な関数 `safeLast :: [a] -> Maybe a` を実装してください. 空リストには `Nothing`, それ以外は最後の要素を `Just` で返します.

2. キーと値のペアのリストから, 指定したキーに対応する値を探す関数 `lookupKey :: Eq k => k -> [(k, v)] -> Maybe v` を実装してください. 見つかれば `Just 値`, 無ければ `Nothing` を返します (標準の `lookup` と同じ動作を自分で書きます).

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (safeLast [1, 2, 3 :: Int])   -- Just 3
  print (safeLast ([] :: [Int]))      -- Nothing
  print (lookupKey "b" [("a", 1), ("b", 2)])  -- Just 2
  print (lookupKey "z" [("a", 1), ("b", 2)])  -- Nothing
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
safeLast :: [a] -> Maybe a
safeLast []       = Nothing
safeLast [x]      = Just x
safeLast (_ : xs) = safeLast xs

lookupKey :: Eq k => k -> [(k, v)] -> Maybe v
lookupKey _ [] = Nothing
lookupKey key ((k, v) : rest)
  | key == k  = Just v
  | otherwise = lookupKey key rest

main :: IO ()
main = do
  print (safeLast [1, 2, 3 :: Int])           -- Just 3
  print (safeLast ([] :: [Int]))              -- Nothing
  print (lookupKey "b" [("a", 1), ("b", 2)])  -- Just 2
  print (lookupKey "z" [("a", 1), ("b", 2)])  -- Nothing
~~~

どちらも「失敗しうる」ことを返り値の `Maybe` で表しているため, 空リストやキー不在でも実行時エラーになりません. `safeLast` は要素 1 つの場合 `[x]` を基底として末尾まで再帰し, `lookupKey` はキーが一致したところで `Just` を返します.

</details>

:::

## Either

`Maybe` の `Nothing` は「失敗した」ことしか伝えられませんでした. 失敗の **理由** も一緒に運びたいときに使うのが `Either` です. `Either` は「2 つの型のどちらか一方の値を持つ」多相データ型で, 型引数を 2 つ取ります.

~~~ haskell
data Either a b = Left a | Right b
~~~

- `Left a` は型 `a` の値を包むコンストラクタです.
- `Right b` は型 `b` の値を包むコンストラクタです.

`Either a b` の値は `Left <a の値>` か `Right <b の値>` のいずれかです. これは [第7章](fp7.html)の直和型そのもので, 集合論的には $a$ と $b$ の **タグ付き直和**

$$\mathrm{Either}\ a\ b = a + b$$

になります (`Left` / `Right` というタグで両者を区別するので, 共通要素があっても素な和になります).

エラー処理では, **`Left` を失敗 (理由つき) / `Right` を成功** に使うのが Haskell の慣習です. 「正しい (right)」と「右 (right)」を掛けた語呂で, 成功を `Right` に割り当てると覚えるとよいでしょう.

では, 失敗の理由 `a` はどんな型で持たせるべきでしょうか. いちばん手軽なのは `Either String b` のように **文字列** で持たせることですが, 純粋関数型らしく設計するなら, **起こりうるエラーを列挙した専用の直和型** を作るほうが適切です. 「ある計算で起こりうる失敗」はふつう有限個しかありません. その候補を [第7章](fp7.html) の **直和型 (列挙型)** としてあらかじめ書き出しておくのです. 割り算の失敗が「0 除算」1 種類なら, 候補全体を直和で書けば $\mathrm{DivError} = \mathrm{DivByZero}$ (エラーの種類が増えれば $e_1 + e_2 + \cdots$ と構築子を足していく) となります. 前節の `safeDiv` を, この専用エラー型で書き直します.

~~~ haskell
-- 起こりうる失敗を列挙した専用のエラー型 (いまは 1 種類)
data DivError = DivByZero
  deriving (Show, Eq)

safeDivE :: Int -> Int -> Either DivError Int
safeDivE _ 0 = Left DivByZero
safeDivE x y = Right (x `div` y)

main :: IO ()
main = do
  print (safeDivE 10 2)  -- Right 5
  print (safeDivE 10 0)  -- Left DivByZero
~~~

`safeDiv` が `Nothing` を返していた箇所で, `safeDivE` は `Left DivByZero` と **理由つきの失敗** を返します. 理由が「文字列という値」ではなく **専用の型** になったことで, 3 つの利点が生まれます.

- **失敗が閉じた集合になる**: 起こりうるエラーが型の構築子として列挙されるので, 「どんな失敗がありうるか」を型を見るだけで把握できます. 文字列だと理由は無限に書けてしまい, 呼び出す側は一覧を追えません.
- **網羅性を検査できる**: `Left` を受けてパターンマッチするとき, 構築子を書き漏らすと GHC が警告します ([第7章](fp7.html) の直和型と同じ利点). 文字列にはこの検査が効きません.
- **表示 (プレゼンテーション) を値から分離できる**: 「何が起きたか」は `DivByZero` という値で持ち, 「どう見せるか」は **別の関数** で与えます.

最後の点を担うのが, エラー値を人間向けの文字列へ変換する関数です. 受け取る側は, これと `Right` の整形を組み合わせて表示します.

~~~ haskell
-- エラー値を人間向けメッセージへ (表示を値から分離する)
renderDivError :: DivError -> String
renderDivError DivByZero = "0 では割れません"

report :: Either DivError Int -> String
report (Left e)  = "エラー: " ++ renderDivError e
report (Right n) = "結果: " ++ show n

main :: IO ()
main = do
  putStrLn (report (safeDivE 10 2))  -- 結果: 5
  putStrLn (report (safeDivE 10 0))  -- エラー: 0 では割れません
~~~

`renderDivError` を差し替えれば, 同じ `DivByZero` を英語で出すことも, ログ用に整形することもできます. **「何が起きたか (値)」と「どう見せるか (文字列)」を分けられる** のが, 専用エラー型の実務上の効きどころです.

::: note

**文字列でもエラーは書ける.** 手早く試すだけなら, 理由をそのまま文字列で持たせて `Either String Int` としてもかまいません.

~~~ haskell
-- 文字列版: 手軽だが「閉じた集合」でも「網羅検査」でもない
safeDivS :: Int -> Int -> Either String Int
safeDivS _ 0 = Left "0 では割れません"
safeDivS x y = Right (x `div` y)
~~~

この書き方は簡単ですが, 失敗の集合が閉じず (どんな文字列でも `Left` にできてしまう), 網羅性検査も効かず, さらに次の warn で述べる **`print` の日本語エスケープ** にも直面します. プロトタイプや使い捨ての計算では文字列で十分ですが, エラーを設計の一部として扱うなら専用の直和型を選ぶ, と使い分けるとよいでしょう.

:::

::: warn

**日本語のエラー文字列を `print` すると化ける.** [第8章](fp8.html) で見たとおり `print x = putStrLn (show x)` であり, `show` は ASCII の範囲外の文字を `\12391` (「で」) のような十進エスケープに置き換えます (`show` の結果をそのまま Haskell コードに貼り戻せるようにするため). 第8章の例は「`String` を返す関数」だったので `putStrLn` に替えれば済みましたが, `Either String Int` のように **`Show` 表現の内側に日本語 `String` が入れ子** になっている場合はやっかいです.

~~~ haskell
ghci> print (Left "0 では割れません" :: Either String Int)
Left "0 \12391\12399\21106\12428\12414\12379\12435"   -- ASCII の "0 " は残り, 日本語だけ化ける
~~~

値全体は `String` ではなく `Either` なので, `putStrLn` に単純に替えることはできません (`putStrLn :: String -> IO ()` は `String` しか受け取れない). 対処は 2 通りあります.

1. **専用エラー型 + `render` 関数で組む (本節の方針)**: `Left DivByZero` は `Show` しても `Left DivByZero` という ASCII だけの表現になり, そもそもエスケープが起きません. 日本語は `renderDivError` が返す `String` にだけ現れ, それを `putStrLn` で出します. 純粋関数型として設計する動機に, この表示上の利点も加わるわけです.
2. **`unicode-show` パッケージの `uprint` / `ushow` を使う**: どうしても日本語を含む `Show` 表現をそのまま出したいときは, エスケープせず表示する `uprint` (= `putStrLn . ushow`) が使えます.

~~~ haskell
ghci> import Text.Show.Unicode (uprint)
ghci> uprint (Left "0 では割れません" :: Either String Int)
Left "0 では割れません"   -- エスケープされない
~~~

`unicode-show` は標準ライブラリ外なので, 使うにはプロジェクトの依存に加える必要があります (`stack` なら `package.yaml` の `dependencies` に `unicode-show` を足す). GHCi で常用したいなら `:set -interactive-print Text.Show.Unicode.uprint` で既定の表示器を差し替える手もあります.

:::

`Maybe` と `Either` の使い分けは「失敗の理由が要るか」です. 理由が不要なら `Maybe`, 理由を運びたいなら `Either` を使い, その理由の型は **専用の直和型を第一候補** に (手軽さを優先するなら `String` でも) 選びます.

### Either a は関手

**`Either a` も関手** です. ただし `Either` は型引数を 2 つ取る (`Either :: * -> * -> *`) ため, 関手にするには片方を固定して `Either a` (種が `* -> *`) の形にします. `fmap` は `Right` の中身にだけ作用し, `Left` (慣習上は失敗) はそのまま素通しします.

~~~ haskell
main :: IO ()
main = do
  print (fmap (+ 1) (Right 3 :: Either String Int))  -- Right 4
  print (fmap (+ 1) (Left "err" :: Either String Int))  -- Left "err"
~~~

`Maybe` と `Either a` は, どちらも「失敗するかもしれない値」を表す関手です. `fmap` で成功側 (`Just` / `Right`) の中身だけを加工しつつ, 失敗 (`Nothing` / `Left`) が出たらそれ以降の加工を素通しできます. エラー処理の文脈では, この性質が便利です.

::: note

### Exercise CH9-3

**理由つきの検証 `checkAge` (専用エラー型)**

年齢の検証で起こりうる失敗を, まず **専用の直和型** `AgeError` として列挙してください. 「負の年齢」と「大きすぎる年齢」の 2 種類なので, 直和で書けば $\mathrm{AgeError} = \mathrm{Negative} + \mathrm{TooLarge}$ です. これを使い, `Int` を受け取って妥当なら `Right 年齢`, 不正なら対応する `Left` を返す関数 `checkAge :: Int -> Either AgeError Int` を実装してください (0 未満なら `Negative`, 150 より大きいなら `TooLarge`). さらに, エラー値を日本語メッセージへ変換する `renderAgeError :: AgeError -> String` も書いてください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (checkAge 30)                                    -- Right 30
  print (checkAge (-1))                                  -- Left Negative
  print (checkAge 200)                                   -- Left TooLarge
  putStrLn (either renderAgeError show (checkAge (-1)))  -- 年齢が負です
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
-- ① 起こりうる失敗を直和型で列挙する
data AgeError = Negative | TooLarge
  deriving (Show, Eq)

-- ② その型を Left に載せて検証する
checkAge :: Int -> Either AgeError Int
checkAge n
  | n < 0     = Left Negative
  | n > 150   = Left TooLarge
  | otherwise = Right n

-- ③ 表示は値と分離し, render 関数で与える
renderAgeError :: AgeError -> String
renderAgeError Negative = "年齢が負です"
renderAgeError TooLarge = "年齢が大きすぎます"

main :: IO ()
main = do
  print (checkAge 30)                                    -- Right 30
  print (checkAge (-1))                                  -- Left Negative
  print (checkAge 200)                                   -- Left TooLarge
  putStrLn (either renderAgeError show (checkAge (-1)))  -- 年齢が負です
~~~

`Maybe` では「不正だった」ことしか伝えられませんが, 専用エラー型 `AgeError` なら **どう不正か** を型の構築子で伝えられます. `Negative` / `TooLarge` と名前が付くので, 受け取る側は文字列を照合せず構築子でパターンマッチでき, 構築子を書き漏らせば網羅性の警告が出ます. 日本語メッセージは `renderAgeError` に切り出し, 値と表示を分けています. (文字列で `Left "年齢が負です"` と持たせることもできますが, その場合は本文の warn で述べた `print` のエスケープに注意が要ります.)

</details>

:::

## Maybe / Either の基本操作

`Maybe` と `Either` はよく使われるため, 標準ライブラリにパターンマッチを定型化した関数が用意されています. 毎回 `case` やパターンマッチを書かずに済みます.

`maybe` は「`Nothing` のときの既定値」と「`Just x` のときに `x` に適用する関数」を渡すと, `Maybe` を 1 つの値に畳み込みます.

~~~ haskell
maybe :: b -> (a -> b) -> Maybe a -> b
~~~

`fromMaybe` は `Maybe` から値を取り出し, `Nothing` のときは既定値を返します (`Data.Maybe` にあります).

~~~ haskell
fromMaybe :: a -> Maybe a -> a
~~~

`either` は `Either` 版で, `Left` のときと `Right` のときの 2 つの関数を渡します.

~~~ haskell
either :: (a -> c) -> (b -> c) -> Either a b -> c
~~~

これらを使うと, 前節までのパターンマッチを短く書けます.

~~~ haskell
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  -- maybe 既定値 関数 Maybe値
  print (maybe 0 (+ 100) (Just 5))         -- 105
  print (maybe 0 (+ 100) Nothing)          -- 0
  -- fromMaybe 既定値 Maybe値
  print (fromMaybe (-1) (safeDiv 10 2))    -- 5
  print (fromMaybe (-1) (safeDiv 10 0))    -- -1
  -- either 左用関数 右用関数 Either値 (Left は renderDivError で文字列化)
  putStrLn (either (\e -> "エラー: " ++ renderDivError e) (\n -> "結果: " ++ show n) (safeDivE 10 2))  -- 結果: 5
  putStrLn (either (\e -> "エラー: " ++ renderDivError e) (\n -> "結果: " ++ show n) (safeDivE 10 0))  -- エラー: 0 では割れません
~~~

`Data.Maybe` には, `Maybe` の集まりを扱う関数もあります. `isJust` / `isNothing` は中身の有無を判定し, `catMaybes` は `Maybe` のリストから `Just` の中身だけを集め, `mapMaybe` は「各要素に `Maybe` を返す関数を適用し, `Just` のものだけ残す」操作です.

~~~ haskell
import Data.Maybe (isJust, catMaybes, mapMaybe)

-- 文字列を Int に変換しようとし, 数字でなければ Nothing
parseInt :: String -> Maybe Int
parseInt s = case reads s of
  [(n, "")] -> Just n
  _         -> Nothing

main :: IO ()
main = do
  print (isJust (Just 3))                          -- True
  print (isJust (Nothing :: Maybe Int))            -- False
  print (catMaybes [Just 1, Nothing, Just 3])      -- [1,3]
  print (mapMaybe parseInt ["1", "x", "3", "y"])   -- [1,3]
~~~

`catMaybes` は失敗 (`Nothing`) を捨てて成功だけを集め, `mapMaybe` は「変換と絞り込みを同時に行う」関数です. 「数字に変換できる文字列だけを集めて整数にする」といった処理を 1 行で書けます.

::: note

### Exercise CH9-4

**`Maybe` を `Either` へ変換 (エラー型を選べる `toEither`)**

`safeDiv :: Int -> Int -> Maybe Int` (本文の定義) の結果を, 理由つきの `Either e a` に変換する関数 `toEither :: e -> Maybe a -> Either e a` を実装してください. `Nothing` のときは渡した理由 (エラー値) を `Left` に, `Just x` のときは `Right x` にします. 理由の型を **`e` のまま多相** にしておくと, 本文の `DivError` のような専用エラー型でも, 手軽な `String` でも, **同じ関数** で変換できます.

~~~ haskell
-- 実行例 (理由には本文の DivError を使う)
main :: IO ()
main = do
  print (toEither DivByZero (safeDiv 10 2))          -- Right 5
  print (toEither DivByZero (safeDiv 10 0))          -- Left DivByZero
  print (toEither "0 では割れません" (safeDiv 10 2))  -- Right 5   (同じ toEither が String でも動く)
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
data DivError = DivByZero
  deriving (Show, Eq)

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv x y = Just (x `div` y)

-- 理由の型 e を固定しない (専用エラー型でも String でも動く)
toEither :: e -> Maybe a -> Either e a
toEither reason Nothing  = Left reason
toEither _      (Just x) = Right x

main :: IO ()
main = do
  print (toEither DivByZero (safeDiv 10 2))          -- Right 5
  print (toEither DivByZero (safeDiv 10 0))          -- Left DivByZero
  print (toEither "0 では割れません" (safeDiv 10 2))  -- Right 5
~~~

`toEither` は「理由を持たない失敗 (`Maybe`)」を「理由つきの失敗 (`Either`)」へ橋渡しします. 理由の型 `e` を固定しないことで, 専用エラー型と文字列のどちらにも同じコードで対応できます. 型を多相にできるのは, `toEither` が中身 `a` にも理由 `e` にも触れず ただ運ぶだけだからです. 実はこの「中身に触れず外側の構造だけ移し替える」形には名前が付いています. 本章後半で見る **自然変換** (`Maybe` から `Either e` への自然変換) です.

</details>

:::

## リスト

[第7章](fp7.html)では「リストが代数的にどのように定義されるかは後の章で扱う」と先送りしていました. その回収です.

### リストは再帰的な代数的データ型

組込のリスト `[a]` は, [第7章](fp7.html)で作った再帰型 `Nat` (`Zero | Succ Nat`) と同じ仲間で, **自分自身を参照する代数的データ型** です. 違いは, `Succ` が「1 つ前の自然数」を抱えるだけだったのに対し, リストは各段に **要素の値も一緒に** 抱える点です. リストの構築子は次の 2 つで,

- `[]` : **空リスト**. 要素を 1 つも持たない基底.
- `(:)` : **cons**. 先頭の要素 `x` と残りのリスト `xs` から `x : xs` を作る再帰.

`[1,2,3]` は糖衣にすぎず, 実体は `1 : (2 : (3 : []))` です. この構造をそのまま `data` で書くと, 組込リストの正体が見えます. `[a]` と同型な自作版 `List a` を書いてみましょう.

~~~ haskell
-- 組込リストの正体:  data [a] = [] | a : [a]
-- それを自作の名前で書いたもの (Nil ↔ [], Cons ↔ (:))
data List a = Nil | Cons a (List a)
  deriving (Show, Eq)

-- [1,2,3] に対応するのは  Cons 1 (Cons 2 (Cons 3 Nil))

-- 長さ: Nil で 0, Cons で「1 + 残りの長さ」
len :: List a -> Int
len Nil         = 0
len (Cons _ xs) = 1 + len xs

-- 連結: 左が空なら右をそのまま, Cons なら先頭を残して残りを再帰的に連結
append :: List a -> List a -> List a
append Nil         ys = ys
append (Cons x xs) ys = Cons x (append xs ys)

main :: IO ()
main = do
  let xs = Cons 1 (Cons 2 (Cons 3 Nil))
      ys = Cons 4 (Cons 5 Nil)
  print (len xs)        -- 3
  print (append xs ys)  -- Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))
~~~

`len` も `append` も, **型の再帰構造に沿って** 定義されています. 構築子が `Nil` (基底) と `Cons` (再帰) の 2 通りなので, 関数もその 2 通りに場合分けし, `Cons` の枝で「残りのリスト `xs`」へ再帰します. これが[第6章](fp6.html)・[第7章](fp7.html)で繰り返した **構造的再帰** で, データの形と関数の形がそのまま対応します. [第7章](fp7.html)の生成作用素の言葉で言えば, `Nat` の $\Phi(X) = \{\texttt{Zero}\} \cup \{\texttt{Succ}\ n \mid n \in X\}$ に対応して, リストは $\Phi(X) = \{\texttt{Nil}\} \cup \{\texttt{Cons}\ x\ xs \mid x \in a,\ xs \in X\}$ が生成する最小の集合です.

そして, この `append` こそが組込の `(++)` にほかなりません (`Nil ↔ []`, `Cons ↔ (:)` と読み替えれば `(++)` の定義そのものです). つまりリストの連結は「型の再帰構造をたどって末尾までつなぐ」再帰関数として定義され, その `(++)` と `[]` が, 次に見るリストのモノイド構造を与えます.

### リストはモノイド

リストは, 連結演算 `(++)` と空リスト `[]` を単位元として, **モノイド** になります. 数式で書けば, 連結 $xs \mathbin{+\!\!\!+} ys$ が演算, $[]$ が単位元で, 結合律 $(xs \mathbin{+\!\!\!+} ys) \mathbin{+\!\!\!+} zs = xs \mathbin{+\!\!\!+} (ys \mathbin{+\!\!\!+} zs)$ と単位元律 $[] \mathbin{+\!\!\!+} xs = xs \mathbin{+\!\!\!+} [] = xs$ を満たします. リストでは新しく演算子を定義する必要はなく, **既存の `(++)` がその役割を果たし**, `(<>) = (++)`, `mempty = []` となっています.

~~~ haskell
main :: IO ()
main = do
  print (([1,2] <> [3,4]) :: [Int])          -- [1,2,3,4]    (<> は ++ と同じ)
  print (([] <> [1,2,3]) :: [Int])           -- [1,2,3]      (左単位元)
  print (([1,2,3] <> []) :: [Int])           -- [1,2,3]      (右単位元)
  print (mconcat [[1],[2],[3]] :: [Int])     -- [1,2,3]      (畳み込み)
~~~

リストの `<>` は `(++)`, `mempty` は `[]` です. `mconcat` は `concat` と同じはたらきをします.

リストは, モノイドの中でも特別な位置を占めます. 要素の型 `a` を決めると, `[a]` は **`a` の値から作れる「最も自由な」モノイド** になります. これを **自由モノイド (free monoid)** といいます. 「自由」とは, 結合律と単位元律以外に余計な等式が成り立たない, という意味です. たとえば `[1,2]` と `[2,1]` は (集合ではないので) 別物のままで, 順序や重複が潰れません. [第7章](fp7.html)で「リストには順序があり重複も許され, 集合とは別物」と注意したことが, ここでは「自由モノイド = 余計な等式を課さないモノイド」として代数的に説明できます.

こうしてリストには, 3 つの見方が重なりました. [第7章](fp7.html)の目には **再帰的な代数的データ型**, [第8章](fp8.html)の目には **(自由) モノイド**, そして本章の目には **関手** です. `fmap = map` は「最初のモデル」の節で見たとおりです. 同じ 1 つの型が, 台の取り方 (値の集まり / 演算を載せた代数 / 射の対応) に応じて別の顔を見せるわけです.

## Map

次に, 連想的なデータを扱うとき実務で最もよく使う, 標準ライブラリ `containers` の **`Map`** を見ます. `Map k v` は **キー `k` から値 `v` を引ける辞書 (連想配列)** を表す多相データ型で, 型引数を **2 つ** 取ります (`Map :: * -> * -> *`). リストと同じ多相型ですが, 自作せずライブラリの型をそのまま使う点が違います (次節では逆に, この種の構造を自分で作って中身を理解します).

~~~ haskell
import qualified Data.Map as Map
import Data.Map (Map)

-- キー (果物名) から 値 (個数) への対応
stock :: Map String Int
stock = Map.fromList [("apple", 3), ("banana", 2)]
~~~

キーで値を引くには `Map.lookup` を使います. キーが無いこともあるので, 結果は **`Maybe v`** で返ります (`Maybe` は前の節で扱いました).

~~~ haskell
main :: IO ()
main = do
  print (Map.lookup "apple" stock)   -- Just 3
  print (Map.lookup "grape" stock)   -- Nothing  (キーが無い)
~~~

**`Map` はモノイド** です. ただし `(<>)` は **左偏 (left-biased) の和** で, 同じキーがあれば **左の値を優先** し, 値そのものは結合しません. 単位元 `mempty` は空の `Map` です.

~~~ haskell
main :: IO ()
main = do
  let a = Map.fromList [("a", 1), ("b", 2)]
      c = Map.fromList [("a", 9), ("d", 4)]
  print ((a <> c) :: Map String Int)
    -- fromList [("a",1),("b",2),("d",4)]   (キー "a" は左の 1 が残る)
~~~

同じキーの値を **結合** したいときは, [第8章](fp8.html)「代数のインスタンスにする利点」で予告したとおり, **値が `Semigroup` であれば** `Map.unionWith (<>)` を使います. 衝突したキーの値を `(<>)` でまとめられます. 典型例が **単語の出現回数の集計** です.

~~~ haskell
import Data.Monoid (Sum (..))

-- 2 つの集計を, 同じキーの値を <> (= 加算) でまとめる
merged :: Map String (Sum Int)
merged = Map.unionWith (<>)
           (Map.fromList [("a", Sum 1), ("b", Sum 1)])
           (Map.fromList [("a", Sum 1), ("c", Sum 1)])
  -- fromList [("a",Sum 2),("b",Sum 1),("c",Sum 1)]

-- 単語リストから出現回数を数える
wordCount :: [String] -> Map String (Sum Int)
wordCount ws = Map.fromListWith (<>) [(w, Sum 1) | w <- ws]
  -- wordCount ["a","b","a"] = fromList [("a",Sum 2),("b",Sum 1)]
~~~

**`Map` は関手 (Functor) でもあります.** `Either` と同じく型引数が 2 つあるので, キーの側を固定した `Map k` (種が `* -> *`) が関手です. したがって `fmap` が作用するのは **値だけ** で, キーは変わりません.

~~~ haskell
main :: IO ()
main = do
  print (fmap (* 10) stock)
    -- fromList [("apple",30),("banana",20)]
~~~

このように `Map` は, **多相型であり, モノイドであり, 関手でもある**, 実務で頻出の型です. リストで見た「3 つの顔」が, ライブラリの型にもそのまま現れています.

## ツリー

前節の `Map` (やその仲間の `Set`) は, 実は内部的に **平衡二分探索木 (balanced binary search tree)** で実装されています. 普段はライブラリの `Map` / `Set` を使えば十分ですが, ここでは ① その **内部構造** がどうなっているか, ② 自作のデータ型に **`Monoid` をどう定義するか**, ③ 自作のデータ型を **`Functor` にする** とはどういうことか, という 3 点を理解するために, 簡単な二分探索木 (binary search tree) を自分で作ってみます.

二分探索木は, 各 **節点 (node)** が 1 つの値と左右 2 つの部分木を持つ木で, どの節点でも

**左部分木のすべての値 $<$ 節点の値 $<$ 右部分木のすべての値**

という **不変条件** を保ちます. この順序のおかげで, 中間順 (左 → 節点 → 右) に走査すると値が **昇順** に並びます. 値 $\{1, 3, 5, 8, 9\}$ を持つ二分探索木の例を図に示します (同じ要素集合でも, 挿入順によって木の形は変わりえます. 詳しくは下の注意を参照).

<svg viewBox="0 0 460 340" width="100%" style="max-width: 520px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="二分探索木の例: 根 5, 左部分木の根 3 (左の子 1), 右部分木の根 8 (右の子 9). 各節点で 左 < 節点 < 右 を満たし, 中間順走査で 1 3 5 8 9 と昇順になる">
  <line x1="210" y1="44" x2="120" y2="120" stroke="currentColor" stroke-width="1.5"/>
  <line x1="210" y1="44" x2="300" y2="120" stroke="currentColor" stroke-width="1.5"/>
  <line x1="120" y1="120" x2="72" y2="196" stroke="currentColor" stroke-width="1.5"/>
  <line x1="300" y1="120" x2="348" y2="196" stroke="currentColor" stroke-width="1.5"/>
  <g stroke="currentColor" stroke-width="1" opacity="0.4">
    <line x1="120" y1="120" x2="168" y2="190"/>
    <line x1="300" y1="120" x2="252" y2="190"/>
    <line x1="72" y1="196" x2="44" y2="264"/>
    <line x1="72" y1="196" x2="100" y2="264"/>
    <line x1="348" y1="196" x2="320" y2="264"/>
    <line x1="348" y1="196" x2="376" y2="264"/>
  </g>
  <g fill="none" stroke="currentColor" stroke-width="1" stroke-dasharray="3 2" opacity="0.55">
    <rect x="162" y="184" width="12" height="12" rx="2"/>
    <rect x="246" y="184" width="12" height="12" rx="2"/>
    <rect x="38" y="258" width="12" height="12" rx="2"/>
    <rect x="94" y="258" width="12" height="12" rx="2"/>
    <rect x="314" y="258" width="12" height="12" rx="2"/>
    <rect x="370" y="258" width="12" height="12" rx="2"/>
  </g>
  <g fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.5">
    <circle cx="210" cy="44" r="19"/>
    <circle cx="120" cy="120" r="19"/>
    <circle cx="300" cy="120" r="19"/>
    <circle cx="72" cy="196" r="19"/>
    <circle cx="348" cy="196" r="19"/>
  </g>
  <g fill="currentColor" font-size="15" font-weight="600" text-anchor="middle">
    <text x="210" y="49">5</text>
    <text x="120" y="125">3</text>
    <text x="300" y="125">8</text>
    <text x="72" y="201">1</text>
    <text x="348" y="201">9</text>
  </g>
  <g font-size="11">
    <circle cx="22" cy="24" r="7" fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.2"/>
    <text x="34" y="28" fill="currentColor">= Node (左部分木・値・右部分木)</text>
    <rect x="16" y="40" width="12" height="12" rx="2" fill="none" stroke="currentColor" stroke-width="1" stroke-dasharray="3 2"/>
    <text x="34" y="50" fill="currentColor">= Leaf (空の木)</text>
  </g>
  <g fill="currentColor" font-size="12" text-anchor="middle">
    <text x="230" y="300">不変条件: 左部分木の値 &lt; 節点の値 &lt; 右部分木の値</text>
    <text x="230" y="320" font-weight="600">中間順走査 toList' → 1  3  5  8  9 (昇順)</text>
  </g>
</svg>

Haskell では, この構造をそのまま **再帰的な代数的データ型** で表せます ([第7章](fp7.html)で作った再帰型 `Nat` と同じく型が自分自身を参照しますが, ここではさらに **型引数** `a` を載せて, 任意の要素型を持つ木に多相化します). 空の木を `Leaf` (図の破線の四角), 値と左右の部分木を持つ節点を `Node (Tree a) a (Tree a)` (図の丸) とします. 続いて, 不変条件を保つ挿入 `insert'`, 中間順走査 `toList'` を定義し, **「要素の集まりを表す木」** としてモノイドを与えます.

~~~ haskell
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- 二分探索木への挿入 (すでにあれば何もしない)
insert' :: Ord a => a -> Tree a -> Tree a
insert' x Leaf = Node Leaf x Leaf
insert' x t@(Node l y r)
  | x < y     = Node (insert' x l) y r
  | x > y     = Node l y (insert' x r)
  | otherwise = t

-- 中間順走査で要素を昇順に取り出す
toList' :: Tree a -> [a]
toList' Leaf         = []
toList' (Node l x r) = toList' l ++ [x] ++ toList' r

fromList' :: Ord a => [a] -> Tree a
fromList' = foldr insert' Leaf

-- 合併演算 ⊔:  t1 ⊔ t2 = 「t1 の全要素を t2 に挿入した木」
(|+|) :: Ord a => Tree a -> Tree a -> Tree a
t1 |+| t2 = foldr insert' t2 (toList' t1)

-- <> は (|+|), 単位元は空の木
instance Ord a => Semigroup (Tree a) where (<>)   = (|+|)
instance Ord a => Monoid    (Tree a) where mempty = Leaf

main :: IO ()
main = do
  let t = fromList' [5,3,8] <> fromList' [3,9,1]
  print (toList' t)   -- [1,3,5,8,9]
~~~

ここでは木を「要素の集合の入れ物」とみなし, `<>` を「一方の全要素をもう一方に挿入する合併」として定義しています. 単位元は空の木 `Leaf` です. `instance Ord a => Semigroup (Tree a)` のように, インスタンス宣言にも `Ord a =>` という **制約を付けられる** 点に注目してください (挿入のために要素の比較 `Ord` が必要なため).

::: warn
この `<>` が結合律を満たすのは, あくまで **「木が表す要素の集合」のレベル** です. `t1 <> t2` と `t2 <> t1`, あるいは括弧の付け方を変えたものは, 取り出される要素の集合 (`toList'` の結果) は等しくなりますが, **木の内部構造 (枝分かれの形) は異なりうる** 点に注意してください. これは[第7章](fp7.html)で見た「リストと集合は別物」という注意の, 木における対応物です. 法則を「どの同値性のもとで成り立つと見るか」を意識することが大切です ([第8章](fp8.html)の商集合の言葉で言えば, 結合律が成り立つのは「同じ要素集合を表す木を同一視した商」の上です).
:::

### ツリーは関手

最後に, `Tree a` を **`Functor` のインスタンス** にしてみます. 自作の多相データ型を関手のモデルに仕立てる, 本部の締めくくりの実例です. `fmap` は「木の構造はそのままに, 各ノードの値だけを関数で変換する」操作として定義します.

~~~ haskell
data Tree a = Leaf | Node (Tree a) a (Tree a) deriving Show

-- 木の形は保ったまま, 各ノードの値に関数を適用する
instance Functor Tree where
  fmap _ Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

-- 中間順走査で値を取り出す (確認用)
toList' :: Tree a -> [a]
toList' Leaf         = []
toList' (Node l x r) = toList' l ++ [x] ++ toList' r

sample :: Tree Int
sample = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)

main :: IO ()
main = do
  print (toList' sample)             -- [1,2,3]
  print (toList' (fmap (* 10) sample))  -- [10,20,30]
~~~

`fmap (*10) sample` は, 木の枝分かれの形 (どこが `Node` でどこが `Leaf` か) を一切変えずに, 各ノードの値だけを 10 倍します. ここで定義の `fmap _ Leaf = Leaf` が「`Leaf` の構造を保つ」, `Node (fmap f l) (f x) (fmap f r)` が「`Node` の構造を保ちつつ値だけ変換し, 部分木にも再帰的に `fmap` する」ことを表しています. リストや木のように, **要素を一様に持つデータ構造はたいてい関手になる** わけです.

::: note
**発展: `data` 宣言は「多項式」 (多項式関手・始代数・Lambek の補題)**

本部で見たモデルたちの `data` 宣言は, [第7章](fp7.html)の数え上げ (直和 = $+$, 直積 = $\times$) の記法で **式** に翻訳できます. 規則は 3 つだけです. 構築子の並び (`|`) は $+$, フィールドの並びは $\times$, 引数のない構築子は $1$ (1 点集合), そして **再帰の位置は変数 $X$** と置きます.

- `Maybe a = Nothing | Just a` → $F(X) = 1 + a$ (再帰なし)
- `List a = Nil | Cons a (List a)` → $F(X) = 1 + a \times X$
- `Tree a = Leaf | Node (Tree a) a (Tree a)` → $F(X) = 1 + X \times a \times X$

こうして得られる $F$ は, $1$ (定数) と $a$ (定数) と $X$ を $+$ と $\times$ で組み合わせた, いわば **多項式** の形をしています. 実際この $F$ は (対象の対応 $X \mapsto F(X)$ に自然な射の対応を添えると) 本章の意味での **関手** になり, **多項式関手 (polynomial functor)** と呼ばれます.

この目で再帰的データ型を見直すと, 前半の発展 note の予告がつながります. 構成子の束を 1 本にした射 $\alpha : F(A) \to A$ を備えた組 $(A, \alpha)$ が **$F$-代数** で, 再帰的データ型はその中で「最初に出来上がる」**始代数** ([第7章](fp7.html)の生成作用素 $\Phi$ の最小不動点の圏論版) でした. さらに **Lambek の補題** という定理が, 始代数の $\alpha$ は必ず **同型** (可逆) になることを保証します. つまり

$$\mathrm{List}\ a \;\cong\; 1 + a \times \mathrm{List}\ a$$

つまりリストという型は, 方程式 $X \cong 1 + a \times X$ の (最小の) **解** なのです. `data` 宣言とは再帰方程式であり, データ型はその解である. この見方はここで名前を挙げるだけに留めますが, 「データの形 ($F$)」と「その上の再帰 (構造的再帰)」を分けて扱う発想は, [第10章](fp10.html)以降の抽象化にも通じています.
:::

# 自然変換

## 型に依らない一様な変換

関手が「圏どうしの変換」だとすれば, **自然変換 (natural transformation)** は「関手どうしの変換」です. 2 つの関手 `f` と `g` があるとき, **型 `a` に依らず一様に** `f a` を `g a` に変換する操作が自然変換です. Haskell では, 次のような **多相関数** がこれにあたります.

$$\alpha : \forall a.\ f\,a \to g\,a$$

ズームアウトの図では, 関手のとき (図2→図3) からさらに一歩引いた, 冒頭の階段の最終段にあたります. 図3 の射 (関手) が今度は点に潰れ, 関手と関手を結ぶ **二重の射** が現れます. これが自然変換です (図4).

<svg viewBox="0 0 466 246" width="100%" style="max-width: 476px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="自然変換の導入図. 図3: 圏 Cat. 圏どうしを関手 F が結ぶ世界. さらに一歩引くと図4: 関手圏. Ob = {F, G, …}, Mor = {α, β∘α, id_F, …} (合成 = 関数合成, 恒等射 = 恒等自然変換). 関手 F と G がそれぞれ点に潰れ, 二重の射 α が現れる. 点 = 関手, 射 = 自然変換 (例 listToMaybe : [] ⇒ Maybe).">
  <defs>
    <marker id="lad2-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="6.5" markerHeight="6.5" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <!-- 図3: 点 = 圏, 射 = 関手 -->
  <rect x="12" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="26" y="46" fill="currentColor" font-size="10" opacity="0.8">図3  圏 Cat</text>
  <g fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.4">
    <circle cx="62" cy="97" r="17"/>
    <circle cx="158" cy="97" r="17"/>
  </g>
  <g fill="currentColor" opacity="0.8">
    <circle cx="56" cy="92" r="1.5"/><circle cx="68" cy="92" r="1.5"/><circle cx="62" cy="104" r="1.5"/>
    <circle cx="152" cy="92" r="1.5"/><circle cx="164" cy="92" r="1.5"/><circle cx="158" cy="104" r="1.5"/>
  </g>
  <g stroke="currentColor" stroke-width="0.7" opacity="0.7">
    <line x1="57" y1="92" x2="66" y2="92"/><line x1="60" y1="94" x2="62" y2="102"/>
    <line x1="153" y1="92" x2="162" y2="92"/><line x1="156" y1="94" x2="158" y2="102"/>
  </g>
  <line x1="84" y1="97" x2="136" y2="97" stroke="currentColor" stroke-width="1.4" marker-end="url(#lad2-arrow)"/>
  <text x="110" y="84" fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="15" text-anchor="middle">F</text>
  <g fill="currentColor" font-size="10.5" text-anchor="middle" opacity="0.8">
    <text x="62" y="132">圏 <tspan font-family="Georgia, 'Times New Roman', serif" font-style="italic">C</tspan></text>
    <text x="158" y="132">圏 <tspan font-family="Georgia, 'Times New Roman', serif" font-style="italic">D</tspan></text>
  </g>
  <g fill="currentColor" text-anchor="middle">
    <text x="110" y="193" font-size="12" font-weight="600">点 = 圏, 射 = 関手</text>
    <text x="110" y="208" font-size="9.5" opacity="0.85">Ob = {C, D, …}</text>
    <text x="110" y="222" font-size="9.5" opacity="0.85">Mor = {F, G∘F, Id, …}</text>
    <text x="110" y="237" font-size="10" opacity="0.7">関手の節までの世界</text>
  </g>
  <line x1="214" y1="100" x2="282" y2="100" stroke="currentColor" stroke-width="1.2" marker-end="url(#lad2-arrow)" opacity="0.75"/>
  <g fill="currentColor" text-anchor="middle">
    <text x="248" y="78" font-size="10" font-weight="600">一歩引く</text>
  </g>
  <!-- 図4: 点 = 関手, 射 = 自然変換 -->
  <rect x="258" y="26" width="196" height="150" rx="10" fill="none" stroke="currentColor" stroke-width="1.2" opacity="0.75"/>
  <text x="272" y="46" fill="currentColor" font-size="10" opacity="0.8">図4  関手圏</text>
  <g fill="rgba(13,148,136,0.18)" stroke="currentColor" stroke-width="1.4">
    <circle cx="308" cy="97" r="17"/>
    <circle cx="404" cy="97" r="17"/>
  </g>
  <g fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="13" text-anchor="middle">
    <text x="308" y="102">F</text>
    <text x="404" y="102">G</text>
  </g>
  <g stroke="currentColor" stroke-width="1.4">
    <line x1="330" y1="93" x2="376" y2="93"/>
    <line x1="330" y1="101" x2="376" y2="101"/>
  </g>
  <path d="M 376 88 L 388 97 L 376 106 z" fill="currentColor"/>
  <text x="354" y="80" fill="currentColor" font-family="Georgia, 'Times New Roman', serif" font-style="italic" font-size="15" text-anchor="middle">α</text>
  <g fill="currentColor" text-anchor="middle">
    <text x="356" y="193" font-size="12" font-weight="600">点 = 関手, 射 = 自然変換</text>
    <text x="356" y="208" font-size="9.5" opacity="0.85">Ob = {F, G, …}</text>
    <text x="356" y="222" font-size="9.5" opacity="0.85">Mor = {α, β∘α, id, …}</text>
    <text x="356" y="237" font-size="10" opacity="0.7">例: listToMaybe : [] ⇒ Maybe</text>
  </g>
</svg>

ここで先頭の $\forall a$ ([第7章](fp7.html)で導入した **全称命題** の量化子) が効いています. 「**すべての** 型 `a` について `f a -> g a` が成り立つ」, つまり `a` がどんな型でも同じ仕組みで変換できる関数だけが自然変換になります. 「`a` を覗き見て型ごとに振る舞いを変える」ことはできず, **中身の値には触れず, 関手の構造 (外側の形) だけを組み替える**のが特徴です. この「型に依らない一様さ」は **パラメトリック多相 (parametric polymorphism)** と呼ばれ, 本章冒頭で挙げた「多相型 = 自然変換」という対応の正体です.

代表例が, リストと `Maybe` を相互に変換する 2 つの関数です (どちらも `Data.Maybe` にあります).

~~~ haskell
-- リストを Maybe へ: 先頭があれば Just, 空なら Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe []      = Nothing
listToMaybe (x : _) = Just x

-- Maybe をリストへ: Just x は [x], Nothing は []
maybeToList :: Maybe a -> [a]
maybeToList Nothing  = []
maybeToList (Just x) = [x]

main :: IO ()
main = do
  print (listToMaybe [1, 2, 3 :: Int])  -- Just 1
  print (listToMaybe ([] :: [Int]))     -- Nothing
  print (maybeToList (Just 5 :: Maybe Int))  -- [5]
  print (maybeToList (Nothing :: Maybe Int)) -- []
~~~

`listToMaybe :: forall a. [a] -> Maybe a` は, 関手 `[]` から関手 `Maybe` への自然変換です. 型 `a` が `Int` でも `String` でも何でも, 「先頭要素があれば `Just`, 無ければ `Nothing`」という同じ規則で変換します. `maybeToList` はその逆向き (`Maybe` から `[]` へ) の自然変換です. どちらも中身の値 `a` を加工せず, 関手の構造だけを移し替えています. Exercise CH9-4 の `toEither reason :: Maybe a -> Either e a` も同じ仲間 (`Maybe` から `Either e` への自然変換) でした.

自然変換が満たすべき条件を **自然性条件 (naturality)** といいます. 「先に中身を変換してから構造を移す」のと「先に構造を移してから中身を変換する」のとで結果が一致する, という条件です. 自然変換 $\alpha$ と任意の関数 `g :: a -> b` について,

$$\mathrm{fmap}\ g \circ \alpha = \alpha \circ \mathrm{fmap}\ g$$

が成り立ちます. 左辺は「$\alpha$ で構造を移してから `fmap g` で中身を変換」, 右辺は「`fmap g` で中身を変換してから $\alpha$ で構造を移す」です.

<svg viewBox="0 0 520 282" width="100%" style="max-width: 560px; display: block; margin: 1.5em auto;" xmlns="http://www.w3.org/2000/svg" role="img" aria-label="自然性条件の可換四角形. 左上 [10,20,30], 右上 [11,21,31], 左下 Just 10, 右下 Just 11. 上下の横向きの射は fmap (+1) (中身の変換), 左右の縦向きの射は listToMaybe (構造の移し替え). 上を回っても下を回っても Just 11 に一致する. これが自然性 fmap g ∘ α = α ∘ fmap g である.">
  <defs>
    <marker id="nat-arrow" viewBox="0 0 10 10" refX="8.5" refY="5" markerWidth="7" markerHeight="7" orient="auto-start-reverse">
      <path d="M 0 1 L 9 5 L 0 9 z" fill="currentColor"/>
    </marker>
  </defs>
  <g stroke="currentColor" stroke-width="1.5" fill="none">
    <line x1="186" y1="52" x2="330" y2="52" marker-end="url(#nat-arrow)"/>
    <line x1="176" y1="196" x2="340" y2="196" marker-end="url(#nat-arrow)"/>
    <line x1="120" y1="70" x2="120" y2="176" marker-end="url(#nat-arrow)"/>
    <line x1="404" y1="70" x2="404" y2="176" marker-end="url(#nat-arrow)"/>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="15" font-weight="600" text-anchor="middle">
    <text x="120" y="46">[10,20,30]</text>
    <text x="404" y="46">[11,21,31]</text>
    <text x="120" y="202">Just 10</text>
    <text x="404" y="202">Just 11</text>
  </g>
  <g fill="currentColor" font-family="monospace" font-size="13">
    <text x="258" y="41" text-anchor="middle">fmap (+1)</text>
    <text x="258" y="188" text-anchor="middle">fmap (+1)</text>
    <text x="112" y="127" text-anchor="end">listToMaybe</text>
    <text x="412" y="127" text-anchor="start">listToMaybe</text>
  </g>
  <g fill="currentColor" font-size="10.5" opacity="0.72">
    <text x="258" y="63" text-anchor="middle">(中身の変換)</text>
    <text x="258" y="208" text-anchor="middle">(中身の変換)</text>
    <text x="112" y="145" text-anchor="end">(α: 構造の移し替え)</text>
    <text x="412" y="145" text-anchor="start">(α: 構造の移し替え)</text>
  </g>
  <text x="260" y="240" fill="currentColor" font-size="12" text-anchor="middle">どちらの経路でも Just 11. 四角形が閉じる = 自然性 fmap g ∘ α = α ∘ fmap g</text>
  <text x="260" y="262" fill="currentColor" font-size="10.5" text-anchor="middle" opacity="0.75">第8章の準同型の四角形 (stats), 「最初の関手」の四角形と同じ形</text>
</svg> たとえば `listToMaybe` なら, `[10,20,30]` に対して

- 先に `fmap (+1)` してから `listToMaybe`: `[11,21,31]` → `Just 11`
- 先に `listToMaybe` してから `fmap (+1)`: `Just 10` → `Just 11`

と, どちらの順でも `Just 11` で一致します. パラメトリック多相な関数は中身の値に触れないため, この自然性は自動的に成り立ちます (型さえ合えば自然性が従う, という強い性質があります).

::: note

### Exercise CH9-5

**自然変換 `firstTwo` の実装**

リストから先頭 2 要素までを取り出して `Maybe` のペアにする自然変換 `firstTwo :: [a] -> Maybe (a, a)` を実装してください. 要素が 2 つ以上あれば先頭 2 つを `Just (x, y)` で, それ未満なら `Nothing` を返します. この関数は型 `a` に依らず一様に動く (パラメトリック多相な) 点を意識してください.

~~~ haskell
-- 実行例
main :: IO ()
main = do
  print (firstTwo [1, 2, 3 :: Int])  -- Just (1,2)
  print (firstTwo [1 :: Int])        -- Nothing
  print (firstTwo ([] :: [Int]))     -- Nothing
  print (firstTwo "abc")             -- Just ('a','b')
~~~

<details class="protected" data-pass="yakagika">
    <summary> 回答例 </summary>

~~~ haskell
firstTwo :: [a] -> Maybe (a, a)
firstTwo (x : y : _) = Just (x, y)
firstTwo _           = Nothing

main :: IO ()
main = do
  print (firstTwo [1, 2, 3 :: Int])  -- Just (1,2)
  print (firstTwo [1 :: Int])        -- Nothing
  print (firstTwo ([] :: [Int]))     -- Nothing
  print (firstTwo "abc")             -- Just ('a','b')
~~~

`firstTwo` は要素の型 `a` を一切覗かず (`Int` でも `Char` でも同じ規則), 「先頭 2 つがあるか」という構造だけで結果を決めています. このため `[a]` から `Maybe (a, a)` への自然変換になっており, `firstTwo "abc"` のように文字列でもそのまま動きます.

</details>

:::

## 関手圏

図4 に **関手圏 (functor category)** と名前を付けたからには, Cat のときと同じ宿題が残っています. この世界も本当に圏なのか. 成分は同じ手順で揃います.

**自然変換は合成できます.** $\alpha : F \Rightarrow G$ と $\beta : G \Rightarrow H$ があれば, 続けて適用する $\beta \circ \alpha : F \Rightarrow H$ も自然変換です. Haskell では多相関数どうしの, ただの関数合成です. 実物も手元にあります. `listToMaybe` (`[] ⇒ Maybe`) と `maybeToList` (`Maybe ⇒ []`) を継ぐと, 「先頭 1 要素だけ残す」という `[]` から `[]` への自然変換になります.

~~~ haskell
ghci> (maybeToList . listToMaybe) [10, 20, 30]
[10]
ghci> (maybeToList . listToMaybe) ([] :: [Int])
[]
~~~

自然性も引き継がれます. 前節の可換四角形を 2 つ横に継げば, 外周がまた可換な四角形になります. Haskell 側では, パラメトリック多相な関数の合成はまたパラメトリック多相なので, 自動で成り立ちます.

**恒等自然変換 (identity natural transformation).** どの型でも何もしない多相関数 $\mathrm{id}_F : \forall a.\ f\,a \to f\,a$ (Haskell の `id` をこの型で読んだもの) は, 関手 $F$ から $F$ 自身への自然変換で, 上の合成の単位になります.

照合しましょう. $\mathrm{Ob} = \{F, G, \dots\}$ (関手たち), $\mathrm{Mor} = \{\alpha,\ \beta,\ \beta \circ \alpha,\ \mathrm{id}_F, \dots\}$ (自然変換たち), 合成 = 関数合成, 恒等射 = 恒等自然変換. 結合律と恒等律は関数のそれをそのまま受け継ぎます. つまり **図4 の世界も圏** (対象が関手, 射が自然変換の **関手圏**) であり, 冒頭の階段の最終段「関手圏: 台 = 自然変換, 単位 = 恒等自然変換」も, これで回収されました.

そして, 「圏の圏 Cat」の節とこの節で作った部品が, そのまま [第10章](fp10.html)の素材になります. モナドの単位 $\eta : \mathrm{Id} \Rightarrow T$ は「**恒等関手** からの自然変換」, 乗法 $\mu : T \circ T \Rightarrow T$ は「**関手の合成** からの自然変換」です. どちらも, 本章で定義した言葉だけで読める型をしています.

## まとめ

これで, 本章冒頭で掲げた対応が一通り埋まりました. 型を **対象**, 関数を **射** とみる圏 Hask の上で, 型引数を持つ多相データ型は **関手** (`fmap` を備える), 型に依らない一様な多相関数は **自然変換** ($\forall a$ で量化された `f a -> g a`) に対応します. 本章の道のりを, 前 2 章から続く 1 本の物語として振り返ります.

| 段階 | 数学 | Haskell | 本章の節 |
| --- | --- | --- | --- |
| 台の転換 | 台 = 値の集まり → **射の集まり** | 関数 `a -> b` を素材にする | 導入 |
| 圏 | 組 $(\mathrm{Ob}, \mathrm{Mor};\ \mathrm{dom}, \mathrm{cod}, \circ)$ + 結合律・恒等律 | 型と関数, `(.)` と `id` (圏 Hask) | 圏の定義 / Hask |
| 一般化 | モノイド = 一点圏 (圏の退化) | `<>` = 合成, `mempty` = `id` | 圏はモノイドの一般化 |
| 圏の準同型 | **関手** (合成と恒等射を保つ) | `Functor` クラス (`fmap`) + 関手則 | 関手 |
| 関手のモデル | $1 + a$, $a + b$, $X \cong 1 + a \times X$, … | `Maybe` / `Either a` / `[]` / `Map k` / `Tree` | 多相データ型 |
| 関手どうしの変換 | **自然変換** (自然性条件) | `forall a. f a -> g a` | 自然変換 |
| 階段の照合 | Cat・関手圏もまた圏 (単位 = 恒等関手・恒等自然変換) | `map (map f)` / `maybeToList . listToMaybe` | 圏の圏 Cat / 関手圏 |

そして, 冒頭に地図として掲げた **俯瞰の階段** の各段を, すべて実物 (定義とコード) で確かめ終えました.

| 段 | 図 | 台 (= 前段の「構造を保つ写像」) | 単位 | どこで |
| --- | --- | --- | --- | --- |
| モノイド | 図1 | 値 | `mempty` | [第8章](fp8.html) |
| 圏 Hask | 図2 | 関数 (= 集合の写像) | `id` | 本章 |
| 圏 Cat | 図3 | **関手 (= 圏の準同型)** | 恒等関手 | 本章 |
| 関手圏 | 図4 | **自然変換 (= 関手どうしの変換)** | 恒等自然変換 | 本章・[第10章](fp10.html) |

図の系列で言えば, 図0 (集合だけ) → 図1 (点 = 台, 射 = 演算, [第7章](fp7.html)・[第8章](fp8.html)) → 図2 (点 = 型, 射 = 関数, 導入) → 図3 (点 = 圏, 射 = 関手, 関手の節) → 図4 (点 = 関手, 射 = 自然変換, 前節) と, **射に何を据えるか** を一段ずつ持ち上げてきました.

どの段でも, やっていることは同じでした. **組 (台 + 演算) を選び, 法則 (結合律と単位律) を課す**. 変わるのは台に何を据えるかだけです. [第10章](fp10.html)では, この階段の最上段で **モノイドのレシピをもう一度回します**. 台に自己関手を据え, 演算に関手の合成を, 単位に恒等関手からの自然変換を選ぶ. そうして得られる構造が **モナド (monad)** です. 本章で手にした関手・関手の合成・恒等関手・自然変換が, そのまま素材になります.
