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
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "styles/*.sass" $ do
        route   $ setExtension ".css"
        compile $ getResourceString
                    >>= withItemBody (unixFilter "sass" ["-s"])
                    >>= return . fmap compressCss

    tags <- buildTags "posts/*" (fromCapture "tags/*")

    match "posts/*" $ do
        route $ gsubRoute "^posts/[[:digit:]]+-[[:digit:]]+-[[:digit:]]+-" (const "blog/")
                    `composeRoutes` prettyUrlRoute
        compile $ do
            defaultCompiler
                >>= saveSnapshot "content"  -- used in atom.xml
                >>= applyBlazeTemplate blogPostTemplate (postCtx tags)
                >>= applyBlazeTemplate defaultTemplate defaultContext

    let buildPostList title pattern = do
            posts <- recentFirst =<< loadAll pattern
            postList <- applyBlazeTemplateList postItemTemplate (postCtx tags) posts
            let ctx = constField "title" title
                    <> constField "posts" postList
                    <> defaultContext
            makeItem ""
                >>= applyBlazeTemplate postListTemplate ctx
                >>= applyBlazeTemplate defaultTemplate ctx

    let buildAtomFeed title pattern = do
        let feedCtx = bodyField "description" <> defaultContext
        posts <- fmap (take 10) . recentFirst =<<
            loadAllSnapshots pattern "content"
        renderAtom (feedConfiguration title) feedCtx posts

    create ["blog"] $ do
        route prettyUrlRoute
        compile $ buildPostList "Blog" "posts/*"

    tagsRules tags $ \tag pattern -> do
        let title = "Posts tagged ‘" ++ tag ++ "’"

        route prettyUrlRoute
        compile $ buildPostList title pattern

        version "atom" $ do
            route $ setExtension "xml"
            compile $ buildAtomFeed (Just title) pattern

    create ["atom.xml"] $ do
        route idRoute
        compile $ buildAtomFeed Nothing "posts/*"

    -- Static pages
    match (fromList pages) $ do
        route prettyUrlRoute
        compile $ do
            defaultCompiler
                >>= applyBlazeTemplate defaultTemplate defaultContext

  where
    pages =
        [ "index.mkd"
        , "cv.mkd"
        ]

    defaultCompiler
        = pandocCompiler
            >>= return . fmap demoteHeaders


getConfig :: IO Configuration
getConfig = do
    homeDirectory <- fromMaybe (error "$HOME not defined") . lookup "HOME"
                    <$> getEnvironment
    return defaultConfiguration
        { destinationDirectory = "../master"
        , storeDirectory = homeDirectory </> ".cache/hakyll"
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


postCtx :: Tags -> Context String
postCtx tags = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%b %e, %Y"
    , shortUrlField "url"
    , tagsField "tags" tags
    , defaultContext
    ]


feedConfiguration :: Maybe String -> FeedConfiguration
feedConfiguration title = FeedConfiguration
    { feedTitle = fromMaybe "lambda fairy's blog" title
    , feedDescription = "inane ramblings"
    , feedAuthorName = "lambda fairy"
    , feedAuthorEmail = "lambda.fairy@gmail.com"
    , feedRoot = "http://lfairy.github.io"
    }
