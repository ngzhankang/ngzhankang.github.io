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

postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" <>
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
                    listField "posts" postCtx (return posts) <>
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
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["stylesheet/default.css"] $ do
        route idRoute
        compile $ do
            items <- loadAll "css/*.scss"
            makeItem $ concatMap itemBody (items :: [Item String])

    match "index.js" $ do
        route idRoute
        compile compressJsCompiler

    create ["archive.html"] $ do
        route   idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) `mappend`
                    constField "title" "Archives"            `mappend`
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls

    match "templates/*" $ compile templateCompiler