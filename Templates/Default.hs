{-# LANGUAGE OverloadedStrings #-}

module Templates.Default (defaultTemplate) where


import Control.Applicative
import Control.Monad
import Hakyll
import Lucid
import System.IO.Unsafe

import Templates.Core


-- | The main page template.
defaultTemplate = LucidTemplate $ \ask -> do
    home <- getStringField $ ask "home" <|> pure (StringField "false")
    title <- getStringField $ ask "title"
    pageTitle <- getStringField $ ask "page-title" <|> pure (StringField title)
    body <- getStringField $ ask "body"
    url <- getStringField $ ask "url"

    doctypehtml_ $ do

        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ $ do
                when (home /= "true") $ toHtml title <> " « "
                "lambda fairy"
            stylesheet "/styles/styles.css"
            stylesheet "/styles/hk-pyg.css"
            stylesheet "//fonts.googleapis.com/css?family=Cabin:400,700,400italic,700italic"
            meta_ [name_ "theme-color", content_ "#5c3566"]
            meta_ [name_ "viewport", content_ "width=device-width"]
            link_ [rel_ "alternate", type_ "application/atom+xml", href_ "/atom.xml"]

        body_ $ do
            header_ $ h1_ $ a_ [href_ "/"] "lambda fairy"

            div_ [id_ "midriff"] $ do

                nav_ $ renderNavigation defaultLinks url

                section_ $ do
                    h1_ $ toHtml pageTitle
                    toHtmlRaw body

            footer_ $ do
                p_ $ do
                    "Licensed under "
                    a_ [href_ "https://creativecommons.org/licenses/by-sa/4.0/"]
                        "CC BY-SA 4.0"
                    "."

            toHtmlRaw analytics


defaultLinks :: [Link]
defaultLinks
    = link' "/index.html" "/" "Home"
    : link "/cv" "CV"
    : link "/blog" "Blog"
    : link "https://github.com/lambda-fairy" "GitHub"
    : []


analytics :: String
analytics = unsafePerformIO $ readFile "analytics.html"
{-# NOINLINE analytics #-}
