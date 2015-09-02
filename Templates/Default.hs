{-# LANGUAGE OverloadedStrings #-}

module Templates.Default (defaultTemplate) where


import Control.Applicative
import Control.Monad
import Data.Monoid

import Templates.Core


-- | The main page template.
defaultTemplate = LucidTemplate $ \ask -> do
    StringField home <- lift $ ask "home" <|> pure (StringField "false")
    StringField title <- lift $ ask "title"
    StringField pageTitle <- lift $ ask "page-title" <|> pure (StringField title)
    StringField body <- lift $ ask "body"
    StringField url <- lift $ ask "url"

    doctypehtml_ $ do

        head_ $ do
            meta_ [charset_ "utf-8"]
            title_ $ do
                when (home /= "true") $ toHtml title <> " « "
                "lambda fairy"
            stylesheet "/styles/styles.css"
            stylesheet "//fonts.googleapis.com/css?family=Cabin:400,700,400italic,700italic"
            meta_ [name_ "viewport", content_ "width=device-width"]

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
                    br_ []
                    a_ [href_ "https://blockchain.info/address/12oNDcYKQgH3QTMm2rChMC4a6FdQC65wLb"]
                        "Bitcoin"


defaultLinks :: [Link]
defaultLinks
    = link' "/index.html" "/" "Home"
    : link "/cv" "CV"
    : link "/blog" "Blog"
    : link "https://github.com/lfairy" "GitHub"
    : []
