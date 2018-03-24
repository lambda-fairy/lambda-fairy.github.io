{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import Data.Maybe
import Data.Monoid
import System.Environment
import System.FilePath.Posix

import Hakyll
import Text.Pandoc.Definition
import Text.Pandoc.Options
import Text.Pandoc.Walk

import Templates


main :: IO ()
main = (getConfig >>=) . flip hakyllWith $ do
    -- Copy static files unmodified
    match ("CNAME" .||. "circle.yml" .||. ("images/**" .&&. complement "*.xcf")
            .||. "styles/*.css") $ do
        route   idRoute
        compile copyFileCompiler

    -- Compile styles using Sass
    match "styles/*.sass" $ do
        route   $ setExtension ".css"
        compile $ getResourceString >>= withItemBody (unixFilter "sass" ["-s"])

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*")

    -- Render each and every post
    match "posts/*" $ do
        route $ gsubRoute "^posts/[[:digit:]]+-[[:digit:]]+-[[:digit:]]+-" (const "blog/")
                    `composeRoutes` prettyUrlRoute
        compile $ do
            pandocCompilerWithTransform myReaderOptions defaultHakyllWriterOptions myPandocTransform
                >>= saveSnapshot "content"  -- used in atom.xml
                >>= return . fmap demoteHeaders
                >>= applyLucidTemplate blogPostTemplate (postContext tags)
                >>= applyLucidTemplate defaultTemplate myContext

    let buildPostList title pattern = do
            posts <- recentFirst =<< loadAll pattern
            postList <- applyLucidTemplateList postItemTemplate (postContext tags) posts
            let ctx = constField "title" title
                    <> constField "posts" postList
                    <> myContext
            makeItem ""
                >>= applyLucidTemplate postListTemplate ctx
                >>= applyLucidTemplate defaultTemplate ctx

    let buildAtomFeed title pattern = do
        let feedCtx = bodyField "description" <> myContext
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots pattern "content"
        renderAtom (feedConfiguration title) feedCtx posts

    -- Post list
    create ["blog"] $ do
        route prettyUrlRoute
        compile $ buildPostList "Blog" "posts/*"

    -- Create a page for each tag
    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged ‘" ++ tag ++ "’"

        route prettyUrlRoute
        compile $ buildPostList title pattern

        -- Atom feed, for Planet Haskell
        version "atom" $ do
            route $ setExtension "xml"
            compile $ buildAtomFeed (Just title) pattern

    -- Atom feed
    create ["atom.xml"] $ do
        route idRoute
        compile $ buildAtomFeed Nothing "posts/*"

    -- Static pages
    match ("index.md" .||. "cv.md") $ do
        route prettyUrlRoute
        compile $ do
            pandocCompiler
                >>= return . fmap demoteHeaders
                >>= applyLucidTemplate defaultTemplate myContext


getConfig :: IO Configuration
getConfig = do
    homeDirectory <- getEnv "HOME"
    destinationDirectory' <-
        fromMaybe (destinationDirectory defaultConfiguration) <$>
        lookupEnv "OUTPUT"
    return defaultConfiguration
        { destinationDirectory = destinationDirectory'
        , storeDirectory = homeDirectory </> ".cache/hakyll"
        , ignoreFile = \path ->
            let name = takeFileName path
            in  ignoreFile defaultConfiguration name
                || name == "4913"  -- Vim
        }


prettyUrlRoute :: Routes
prettyUrlRoute = customRoute $ prettify . toFilePath
  where
    prettify p
      | name == "index" = appendIndexHtml directory
      | otherwise = appendIndexHtml $ directory </> name
      where
        (directory, (name, _)) = second splitExtension $ splitFileName p
        appendIndexHtml = (</> "index.html") . dropExtension


shortUrlField :: String -> Context a
shortUrlField key = field key $
    fmap (maybe empty (removeIndexHtml . toUrl)) . getRoute . itemIdentifier


removeIndexHtml :: FilePath -> FilePath
removeIndexHtml = uncurry combine . second frobnicate . splitFileName
  where
    frobnicate name
      | name == "index.html" = ""
      | otherwise = name


myReaderOptions :: ReaderOptions
myReaderOptions = defaultHakyllReaderOptions
    { readerExtensions = disableExtension Ext_implicit_figures $
        readerExtensions defaultHakyllReaderOptions }


myPandocTransform :: Pandoc -> Pandoc
myPandocTransform (Pandoc meta blocks) = Pandoc meta (walk addImageLink blocks)
  where
    addImageLink (Para [image@(Image _ _ target)]) = Para [Link nullAttr [image] target]
    addImageLink block = block


postContext :: Tags -> Context String
postContext tags = mconcat
    [ dateField "date" "%b %e, %Y"
    , tagsField "tags" tags
    , myContext
    ]


myContext :: Context String
myContext = shortUrlField "url" <> defaultContext


feedConfiguration :: Maybe String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle = fromMaybe "lambda fairy's blog" title
    , feedDescription = "inane ramblings"
    , feedAuthorName = "lambda fairy"
    , feedAuthorEmail = "lambda.fairy@gmail.com"
    , feedRoot = "https://lambda.xyz"
    }
