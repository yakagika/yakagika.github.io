---
title: Hakyllでブログ作成
description: 最初の投稿. Hakyllでブログを作成するあれこれ.
tags: haskell
featured: true
katex: true
tableOfContents: true
---


# はじめに

講義資料の公開,置き場やら自分のプロフィール載せるために取り敢えずブログを作った.

取り敢えずHaskell静的サイトでググって最初に出てきたHakyllを使うことにした.

[Haklly](https://jaspervdj.be/hakyll/index.html)開発者のjaspervdjの[ブログ](https://jaspervdj.be)の[ソースコード](https://github.com/jaspervdj/jaspervdj)をほとんどそのまま使っている
ため,今後色々変えていく予定.

取り敢えずの変更点として,

- ↑はcabalで開発していたので,stackにした
- Hakyll+KaTeXで数式を書けるようにした
- シンタックスハイライトの追加
- 目次の追加
- safari対応
- 細かなデザインの変更

など. ソースコードは[こちら](https://github.com/yakagika/yakagika.github.io).
今後なにか変更を加えたら書いていく.

# stack

package.yamlを追加したのみ.
これで,

~~~ sh
stack build

stack exec main build (2回目以降はrebuild)

stack exec main watch

~~~

で確認できる.
(執筆段階では,まだローカルで試しているだけ)
package.yamlは以下.

~~~ yaml
ependencies:
  - base
  - binary
  - directory
  - filepath
  - hakyll
  - pandoc
  - process
  - text
  - containers

library:
  source-dirs: src

_exe-defs: &exe-defaults
  dependencies: blog


executables:
  main:
    <<: *exe-defaults
    main:                Main.hs
    source-dirs:         src
~~~

# KaTeX

基本的にはこちらの[サイト](https://axiomatic.neophilus.net/using-katex-with-hakyll/)を参考にした2015年の記事で現在はHakyllを使っておらず,ソースコードが消えていたので,補うのに苦労した.
KaTeXの情報が古かったので,mathCtxを最新のKaTeXの[テンプレ](https://github.com/KaTeX/KaTeX)にした.

~~~ haskell
mathCtx :: Context a
mathCtx = field "katex" $ \item -> do
    katex <- getMetadataField (itemIdentifier item) "katex"
    return $ case katex of
                    Just "false" -> ""
                    Just "off" -> ""
                    _ -> "<link rel=\"stylesheet\" href=\"https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/katex.min.css\" integrity=\"sha384-wcIxkf4k558AjM3Yz3BBFQUbk/zgIYC2R0QpeeYb+TwlBVMrlgLqwRjRtGZiK7ww\" crossorigin=\"anonymous\">\n\
                             \<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/katex.min.js\" integrity=\"sha384-hIoBPJpTUs74ddyc4bFZSM1TVlQDA60VBbJS0oA934VSz82sBx1X7kSx2ATBDIyd\" crossorigin=\"anonymous\"></script>\n\
                             \<script defer src=\"https://cdn.jsdelivr.net/npm/katex@0.16.10/dist/contrib/auto-render.min.js\" integrity=\"sha384-43gviWU0YVjaDtb/GhzOouOXtZMP/7XUzwPTstBeZFe/+rCMvRwr4yROQP43s0Xk\" crossorigin=\"anonymous\" onload=\"renderMathInElement(document.body);\"></script>"
~~~

これでコンテクストを作って,mappend (<>)でdefaultContext (template/default.html)についかする.

~~~ haskell
match ("lectures/*.md" .||. "lectures/*.html" .||. "lectures/*.lhs") $ do
        route   $ setExtension ".html"
        compile $ pandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/lecture.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/content.html" (mathCtx <> defaultContext )
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
                >>= relativizeUrls
~~~

もとのブログは,markdownのメタデータにおいて,
katex : trueとなっているものだけにKaTeXを適用するのが方針で,
以下のように書くことで実現できる.
"\$\$"で数式が書けるようにdelimitersを設定する必要があるのだが,Hakyllの仕様で,"\$"が消えるので全部二重にしたら上手く行った.

~~~ html
<!DOCTYPE html>
<html lang="en" $if(dark)$class="dark"$endif$>
    <head>
        <meta charset="UTF-8">
        <meta name="viewport" content="width=device-width">

        <title>yakagika - $title$</title>

        <!-- Stylesheets. -->
        <link rel="stylesheet" type="text/css" href="/style.css?v=0">

        <!-- RSS. -->
        <link rel="alternate" type="application/rss+xml" title="yakagika" >
        <!-- href="http://jaspervdj.be/rss.xml" -->

        <!-- Metadata. -->
        <meta name="keywords" content="yakagika Haskell ExchangeAlgebra">
        <meta name="description" content="Personal home page and blog of yakagika.">
        $if(katex)$
        <!-- KaTeXのスタイルシートとJavaScriptのリンクを動的に挿入 -->
        $katex$
        $endif$
        $if(description)$<meta property="og:description" content="$description$" />$endif$
    </head>
    <body>
        <div id ="navigation">
            <h1>Contents</h1>
            <a href="/">Home</a>
            <a href="/posts.html">Blog</a>
            <a href="/lectures.html">Lecture</a>
            <a href="/research.html">Research</a>
            <a href="/contact.html">Contact</a>
            <!-- <a href="/cv.html">CV</a> -->
            <h1>Links</h1>
            <a href="http://github.com/yakagika">GitHub</a>
        </div>

        $body$
        <!-- GUID -->
        <div style="display: none">ce0f13b2-4a83-4c1c-b2b9-b6d18f4ee6d2</div>
        $if(katex)$
        <!-- KaTeX JavaScript and auto-render extension -->
        <script>
          document.addEventListener("DOMContentLoaded", function() {
            renderMathInElement(document.body, {
              delimiters: [
                {left: "$$$$", right: "$$$$", display: true},
                {left: "$$", right: "$$", display: false} // インライン数式用のデリミタを追加
              ]
            });
          });
        </script>
        $endif$
    </body>
</html>

~~~

とりあえずこんな感じで出せる.

~~~ tex
あいうえお \\( f(あ) = a^2 \\) かきくけこ

あいうえお $ f(あ) = a^2 $ かきくけこ

$$ f(x) = \frac{1}{x}  $$

~~~

あいうえお \\( f(あ) = a^2 \\) かきくけこ

あいうえお $ f(あ) = a^2 $ かきくけこ

$$ f(x) = \frac{1}{x}  $$

# シンタクスハイライトの変更

シンタックスハイライトを変更した
シンタックスは,pandocCompilerのオプションとして指定できる.

~~~ haskell
import Text.Pandoc.Highlighting
pandocCodeStyle :: Style
pandocCodeStyle = breezeDark
customWriterOptions:: Pandoc.WriterOptions
customWriterOptions = defaultHakyllWriterOptions { Pandoc.writerHTMLMathMethod = Pandoc.MathJax ""
                                                 , Pandoc.writerHighlightStyle = Just pandocCodeStyle}

myPandocCompiler :: Compiler (Item String)
myPandocCompiler =
    pandocCompilerWith defaultHakyllReaderOptions customWriterOptions

~~~

このように適用することでこのブログの見た目になっている.

~~~ haskell
 match ("posts/*.md" .||. "posts/*.html" .||. "posts/*.lhs") $ do
        route   $ setExtension ".html"
        compile $ myPandocCompiler
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/content.html" (mathCtx <> defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
                >>= relativizeUrls
~~~

# 目次の生成
[こちらのサイト](https://svejcar.dev/posts/2019/11/27/table-of-contents-in-hakyll/)そのままで導入した.
だんだんPandocのOptionが長くなっていく...

~~~ haskell
import           Prelude
import           Data.Functor.Identity (runIdentity)

tocTemplate = either error Prelude.id . runIdentity . Pandoc.compileTemplate "" $ T.unlines
  [ "<div class=\"toc\"><div class=\"header\">Table of Contents</div>"
  , "$toc$"
  , "</div>"
  , "$body$"
  ]

customWriterOptions:: Pandoc.WriterOptions
customWriterOptions = defaultHakyllWriterOptions
                        { Pandoc.writerHTMLMathMethod   = Pandoc.MathJax ""       -- LaTeX
                        , Pandoc.writerNumberSections   = True
                        , Pandoc.writerHighlightStyle   = Just pandocCodeStyle    -- Syntax
                        , Pandoc.writerTableOfContents  = True                    -- toc
                        , Pandoc.writerTOCDepth         = 3
                        , Pandoc.writerTemplate          = Just tocTemplate
                        }
~~~

として,

~~~ haskell
    match ("posts/*.md" .||. "posts/*.html" .||. "posts/*.lhs") $ do
        route   $ setExtension ".html"
        compile $ do
                underlying <- getUnderlying
                toc        <- getMetadataField underlying "tableOfContents"
                let writerOptions' = maybe defaultHakyllWriterOptions (const customWriterOptions) toc

                pandocCompilerWith defaultHakyllReaderOptions writerOptions'
                >>= saveSnapshot "content"
                >>= return . fmap demoteHeaders
                >>= loadAndApplyTemplate "templates/post.html" (postCtx tags)
                >>= loadAndApplyTemplate "templates/content.html" (mathCtx <> defaultContext)
                >>= loadAndApplyTemplate "templates/default.html" (mathCtx <> defaultContext)
                >>= relativizeUrls
~~~


yakagika