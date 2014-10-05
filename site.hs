{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>), mconcat)
import System.Environment (getEnvironment)
import System.FilePath.Posix

import Hakyll

import Templates


main :: IO ()
main = (getConfig >>=) . flip hakyllWith $ do
    -- Copy static files unmodified
    match ("CNAME" .||. ("images/**" .&&. complement "*.xcf")) $ do
        route   idRoute
        compile copyFileCompiler

    -- Compile styles using Sass
    match "styles/*.sass" $ do
        route   $ setExtension ".css"
        compile $ getResourceString
                    >>= withItemBody (unixFilter "sass" ["-s"])
                    >>= return . fmap compressCss

    -- Build tags
    tags <- buildTags "posts/*" (fromCapture "tags/*")

    -- Render each and every post
    match "posts/*" $ do
        route $ gsubRoute "^posts/[[:digit:]]+-[[:digit:]]+-[[:digit:]]+-" (const "blog/")
                    `composeRoutes` prettyUrlRoute
        compile $ do
            pandocCompiler
                >>= saveSnapshot "content"  -- used in atom.xml
                >>= return . fmap demoteHeaders
                >>= applyBlazeTemplate blogPostTemplate (postContext tags)
                >>= applyBlazeTemplate defaultTemplate myContext

    let buildPostList title pattern = do
            posts <- recentFirst =<< loadAll pattern
            postList <- applyBlazeTemplateList postItemTemplate (postContext tags) posts
            let ctx = constField "title" title
                    <> constField "posts" postList
                    <> myContext
            makeItem ""
                >>= applyBlazeTemplate postListTemplate ctx
                >>= applyBlazeTemplate defaultTemplate ctx

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
                >>= applyBlazeTemplate defaultTemplate myContext


getConfig :: IO Configuration
getConfig = do
    homeDirectory <- fromMaybe (error "$HOME not defined") . lookup "HOME"
                    <$> getEnvironment
    return defaultConfiguration
        { destinationDirectory = "../master"
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
