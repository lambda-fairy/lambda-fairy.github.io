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

    match "posts/*" $ do
        route $ gsubRoute "^posts/[[:digit:]]+-[[:digit:]]+-[[:digit:]]+-" (const "blog/")
                    `composeRoutes` prettyUrlRoute
        compile $ do
            defaultCompiler
                >>= applyBlazeTemplate blogPostTemplate postCtx
                >>= saveSnapshot "content"  -- used in atom.xml
                >>= applyBlazeTemplate defaultTemplate postCtx

    create ["blog"] $ do
        route prettyUrlRoute
        compile $ do
            list <- postList "posts/*" recentFirst
            let ctx = constField "title" "Blog"
                    <> constField "posts" list
                    <> defaultContext
            makeItem ""
                >>= applyBlazeTemplate postListTemplate ctx
                >>= applyBlazeTemplate defaultTemplate ctx

    create ["atom.xml"] $ do
        route idRoute
        compile $ do
            let feedCtx = postCtx <> bodyField "description"
            posts <- fmap (take 10) . recentFirst =<<
                loadAllSnapshots "posts/*" "content"
            renderAtom feedConfiguration feedCtx posts

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


postCtx :: Context String
postCtx = mconcat
    [ modificationTimeField "mtime" "%U"
    , dateField "date" "%b %e, %Y"
    , shortUrlField "url"
    --, tagsField "tags" tags
    , defaultContext
    ]


postList :: Pattern -> ([Item String] -> Compiler [Item String])
         -> Compiler String
postList pattern scramble = do
    posts <- scramble =<< loadAll pattern
    applyBlazeTemplateList postItemTemplate postCtx posts


feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration
    { feedTitle = "lambda fairy's blog"
    , feedDescription = "inane ramblings"
    , feedAuthorName = "lambda fairy"
    , feedAuthorEmail = "lambda.fairy@gmail.com"
    , feedRoot = "http://lfairy.github.io"
    }
