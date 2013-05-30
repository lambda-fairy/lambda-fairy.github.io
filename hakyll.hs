{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe (fromMaybe)
import System.Environment (getEnvironment)
import System.FilePath

import Hakyll

import Templates.Default

main :: IO ()
main = (getConfig >>=) . flip hakyllWith $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

{-
    match "favicon.ico" $ do
        route   idRoute
        compile copyFileCompiler
-}

    match "styles/*.sass" $ do
        route   $ setExtension ".css"
        compile $ getResourceString
                    >>= withItemBody (unixFilter "sass" ["-s"])
                    >>= return . fmap compressCss

    -- Static pages
    forM_ pages $ \p ->
        match p $ do
            route   prettyUrlRoute
            compile defaultCompiler

{-
    -- 404 page
    match "404.html" $ do
        route   idRoute
        compile defaultCompiler
-}

  where
    pages :: [Pattern]
    pages =
        [ "index.mkd"
        , "cv.mkd"
        , "code.mkd"
        ]

    defaultCompiler
        = pandocCompiler
            >>= return . fmap demoteHeaders
            >>= applyBlazeTemplate defaultTemplate defaultContext

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
      | name `elem` indexSynonyms = appendIndexHtml directory
      | otherwise = appendIndexHtml $ directory </> name
      where
        (directory, (name, _)) = second splitExtension $ splitFileName p

    appendIndexHtml = (</> "index.html") . dropExtension

    indexSynonyms = ["index", "default"]
