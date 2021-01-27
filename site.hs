--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import Data.Semigroup ((<>))
import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Text.Pandoc.Options

-- http://travis.athougies.net/posts/2013-08-13-using-math-on-your-hakyll-blog.html
pandocMathCompiler :: Compiler (Item String)
pandocMathCompiler = pandocCompilerWith defaultHakyllReaderOptions writerOptions
  where
    mathExtensions = [Ext_tex_math_dollars, Ext_tex_math_double_backslash,
                      Ext_tex_math_single_backslash, Ext_latex_macros]
    defaultExtensions = writerExtensions defaultHakyllWriterOptions
    withTexExtensions = foldr enableExtension defaultExtensions mathExtensions
    writerOptions = defaultHakyllWriterOptions {
      writerExtensions = withTexExtensions,
      writerHTMLMathMethod = MathML
    }
  
--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*.scss" $ do
        route $ setExtension ".css"
        let compressCssItem = fmap compressCss
        compile (compressCssItem <$> sassCompiler)

    match (fromList ["about.md"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match ("posts/*.md" .||. "posts/*.markdown" .||. "posts/*.tex") $ do
        tags <- buildTags ("posts/*.md" .||. "posts/*.markdown" .||. "posts/*.tex") (fromCapture "tags/*.html")
        tagsRules tags $ \tag pattern -> do
            let title = "Posts tagged \"" ++ tag ++ "\""
            route idRoute
            compile $ do
                posts <- recentFirst =<< loadAll pattern
                let ctx = constField "title" title
                          <> listField "posts" postCtx (return posts)
                          <> defaultContext
                makeItem ""
                    >>= loadAndApplyTemplate "templates/tag.html" ctx
                    >>= loadAndApplyTemplate "templates/default.html" ctx
                    >>= relativizeUrls
        route $ setExtension "html"
        compile $ pandocMathCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtxWithTags tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtxWithTags tags)
            >>= relativizeUrls
    
    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    match "index.html" $ do
        route idRoute
        compile $ do
            recentPosts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "recentPosts" postCtx (return recentPosts) <>
                    constField "title" "Home"                            <>
                    defaultContext

            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateBodyCompiler


--------------------------------------------------------------------------------
postCtxWithTags :: Tags -> Context String
postCtxWithTags tags = tagsField "tags" tags <> postCtx

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
  defaultContext
