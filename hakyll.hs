{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import Control.Monad
import Data.Maybe (fromMaybe)
import System.Environment (getEnvironment)
import System.FilePath

import Hakyll
import Text.Pandoc.Definition (Pandoc())
import Text.Pandoc.Shared (headerShift)

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
                    >>> unixFilter "sass" ["-s"]
                    >>^ compressCss

    -- Read in templates
    match "templates/*" $ compile templateCompiler

    -- TODO: Add updating navigation bar: see .current in styles.sass

    -- Static pages
    forM_ pages $ \p ->
        match p $ do
            route   $ prettyUrlRoute
            compile $ defaultCompiler >>> relativizeUrlsCompiler

    -- Don't relativize the 404 page
    match "404.html" $ do
        route   idRoute
        compile defaultCompiler

  where
    pages :: [Pattern a]
    pages =
        [ "index.mkd"
        , "about.mkd"
        , "contact.mkd"
        , "cv.mkd"
        ]

    defaultCompiler = pageCompilerWithTransformer (headerShift 1)
        >>> applyTemplateCompiler "templates/default.hamlet"

getConfig :: IO HakyllConfiguration
getConfig = do
    homeDirectory <- fromMaybe (error "$HOME not defined") . lookup "HOME"
                    <$> getEnvironment
    return defaultHakyllConfiguration
        { destinationDirectory = "../master"
        , storeDirectory = homeDirectory </> ".cache/hakyll"
        }

prettyUrlRoute :: Routes
prettyUrlRoute = customRoute $ prettify . toFilePath
  where
    prettify p
      | name `elem` indexSynonyms = frobnicate directory
      | otherwise = frobnicate $ directory </> name
      where
        (directory, (name, _)) = second splitExtension $ splitFileName p

    frobnicate = (</> "index.html") . dropExtension

    indexSynonyms = ["index", "default"]

pageCompilerWithTransformer :: (Pandoc -> Pandoc) -> Compiler Resource (Page String)
pageCompilerWithTransformer
    = pageCompilerWithPandoc defaultHakyllParserState defaultHakyllWriterOptions
