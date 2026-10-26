--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.Monoid (mappend)
import           Hakyll
import           Hakyll.Web.Sass (sassCompiler)
import           Text.Jasmine
import           Data.List              (isInfixOf, isSuffixOf)
import           System.FilePath.Posix  (splitFileName, takeBaseName, takeDirectory, (</>), takeFileName)


--------------------------------------------------------------------------------
config :: Configuration
config = defaultConfiguration { 
    -- destinationDirectory = "_site" we blanco this out because we need to generate files to docs for github to render
    destinationDirectory = "docs"
}

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = do
    let minifyJS = C.unpack . minify . C.pack . itemBody
    s <- getResourceString
    return $ itemSetBody (minifyJS s) s

-- tags render as bare links; templates wrap them in .tags and CSS adds the "#"
postCtx :: Tags -> Context String
postCtx tags =
  dateField "date" "%B %e, %Y" <>
  tagsFieldWith getTags simpleRenderLink mconcat "tags" tags <>
  siteCtx

siteCtx :: Context String
siteCtx =
  activeClassField <>
  defaultContext

activeClassField :: Context a
activeClassField = functionField "activeClass" $ \[p] _ -> do
  path <- toFilePath <$> getUnderlying
  return $ if path == p then "active" else "inactive"

-- clean url extensions (www.xyz/about.html -> www.xyz/about)
-- from https://www.rohanjain.in/hakyll-clean-urls/
cleanRoute :: Routes
cleanRoute = customRoute createIndexRoute
  where
    createIndexRoute ident = takeDirectory p </> takeBaseName p </> "index.html"
        where p = toFilePath ident

cleanIndexUrls :: Item String -> Compiler (Item String)
cleanIndexUrls = return . fmap (withUrls cleanIndex)

cleanIndexHtmls :: Item String -> Compiler (Item String)
cleanIndexHtmls = return . fmap (replaceAll pattern replacement)
    where
      pattern = "/index.html"
      replacement = const "/"

cleanIndex :: String -> String
cleanIndex url
    | idx `isSuffixOf` url = take (length url - length idx) url
    | otherwise            = url
  where idx = "index.html"

main :: IO ()
main = hakyllWith config $ do
    -- from the `tags:` field in each post; each tag gets a page at /tags/<tag>/
    tags <- buildTags "posts/*" (fromCapture "tags/*/index.html")

    tagsRules tags $ \tag pattern -> do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll pattern
            let tagCtx =
                    constField "title" ("#" ++ tag) <>
                    listField "posts" (postCtx tags) (return posts) <>
                    siteCtx
            makeItem ""
                >>= loadAndApplyTemplate "templates/tag.html" tagCtx
                >>= loadAndApplyTemplate "templates/default.html" tagCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "css/*.scss" $ do
        route   $ setExtension "css"
        compile sassCompiler
    match "images/favicon/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "images/others/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "index.html" $ do
        route   idRoute
        compile $ do
            posts <- fmap (take 5) . recentFirst =<< loadAll "posts/*"
            let indexCtx =
                    listField "posts" (postCtx tags) (return posts) <>
                    siteCtx
            getResourceBody
                >>= applyAsTemplate indexCtx
                >>= loadAndApplyTemplate "templates/default.html" indexCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match (fromList["about.md"]) $ do
        route $ cleanRoute
        compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/about.html" siteCtx
                >>= loadAndApplyTemplate "templates/default.html" siteCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "resume.pdf" $ do
        route   idRoute
        compile copyFileCompiler

    -- Self-hosted PDF.js (see js/pdfjs/README.md); powers the /resume viewer.
    match "js/pdfjs/*.js" $ do
        route   idRoute
        compile copyFileCompiler

    match (fromList["resume.md"]) $ do
        route $ cleanRoute
        compile $ pandocCompiler
                -- >>= loadAndApplyTemplate "templates/about.html" postCtx
                -- >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= loadAndApplyTemplate "templates/default.html" siteCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
            >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
            >>= relativizeUrls
            >>= cleanIndexUrls

    create ["stylesheet/default.css"] $ do
        route idRoute
        compile $ do
            items <- loadAll "css/*.scss"
            makeItem $ concatMap itemBody (items :: [Item String])

    match "index.js" $ do
        route idRoute
        compile compressJsCompiler

    create ["writing/index.html"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let writingCtx =
                    listField "posts" (postCtx tags) (return posts) <>
                    field "taglist" (\_ -> renderTagList tags)      <>
                    constField "title" "Writing"                    <>
                    siteCtx

            makeItem ""
                >>= loadAndApplyTemplate "templates/writing.html" writingCtx
                >>= loadAndApplyTemplate "templates/default.html" writingCtx
                >>= relativizeUrls
                >>= cleanIndexUrls

    match "templates/*" $ compile templateCompiler