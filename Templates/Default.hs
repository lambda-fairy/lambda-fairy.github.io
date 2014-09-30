{-# LANGUAGE OverloadedStrings #-}

module Templates.Default
    ( defaultTemplate
    ) where

import Control.Applicative
import Data.Monoid
import Text.Blaze.Html ((!), preEscapedToHtml, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Templates.Core


-- | The main page template.
defaultTemplate = BlazeTemplate $ \get ->
    page <$> (get "home" <|> pure "false") <*> get "title" <*> (get "page-title" <|> get "title") <*> get "body" <*> get "url"
  where
    page home title pageTitle body url = H.docTypeHtml $ do

        H.head $ do
            H.meta ! A.charset "utf-8"
            H.title $ do
                if home == "true"
                    then mempty
                    else toHtml title <> " « "
                "lambda fairy"
            stylesheet "/styles/styles.css"
            stylesheet "//fonts.googleapis.com/css?family=Cabin:400,700,400italic,700italic"
            H.meta ! A.name "viewport" ! A.content "width=device-width"

        H.body $ do
            H.header $ do
                H.h1 $ H.a ! A.href "/" $ "lambda fairy"

            H.div ! A.id "midriff" $ do

                H.nav $ renderNavigation defaultLinks url

                H.section $ do
                    H.h1 $ toHtml pageTitle
                    preEscapedToHtml body

            H.footer $ do
                H.p $ do
                    "Licensed under "
                    H.a ! A.href "http://creativecommons.org/licenses/by-sa/4.0/" $
                        "CC BY-SA 4.0"
                    "."
                    H.br
                    "Like what I do? "
                    H.a ! A.href "https://blockchain.info/address/12oNDcYKQgH3QTMm2rChMC4a6FdQC65wLb" $
                        "Send me a Bitcoin!"


defaultLinks :: [Link]
defaultLinks
    = link' "/index.html" "/" "Home"
    : link "/cv" "CV"
    : link "/blog" "Blog"
    : link "https://github.com/lfairy" "GitHub"
    : []
