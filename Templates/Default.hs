{-# LANGUAGE OverloadedStrings #-}

module Templates.Default
    ( defaultTemplate
    ) where

import Control.Applicative
import Data.List (intersperse)
import Data.Monoid (mconcat)
import Text.Blaze.Html ((!), preEscapedToHtml, toHtml)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Templates.Core


-- | The main page template.
defaultTemplate = BlazeTemplate $ \get ->
    page <$> (get "home" <|> pure "false") <*> get "title" <*> get "body" <*> get "url"
  where
    page home title body url = H.docTypeHtml $ do

        H.head $ do
            H.meta ! A.charset "utf-8"
            H.title $ if home == "true"
                        then "Lambda Fairy"
                        else toHtml title
            stylesheet "/styles/styles.css"
            stylesheet "http://fonts.googleapis.com/css?family=Cabin:400,700,400italic,700italic"

        H.body $ do
            H.div ! A.id "topbit" $ do
                H.header $ do
                    H.h1 $ H.a ! A.href "/" $ "Lambda Fairy"

                H.nav $ renderNavigation defaultLinks url

            H.div ! A.id "main" $ do
                H.h1 $ toHtml title
                preEscapedToHtml body

            H.footer $ do
                H.p . buildFooter $
                    [ "© 2013 Chris Wong."
                    , H.a ! A.href "http://creativecommons.org/licenses/by-sa/3.0/" $ "Some rights reserved."
                    , do
                        "Proudly generated with "
                        H.a ! A.href "http://www.jaspervdj.be/hakyll/" $ "Hakyll"
                        "."
                    , do
                        "Bitcoin:\xA0"
                        H.a ! A.href "http://blockchain.info/fb/12ondc" $ "12oNDcYKQgH3QTMm2rChMC4a6FdQC65wLb"
                        "."
                    ]

    buildFooter = mconcat . intersperse "\n" . map (H.span ! A.class_ "block")


defaultLinks :: [Link]
defaultLinks
    = link' "/index.html" "/" "Home"
    : link "/cv" "CV"
    : link "/code" "Code"
    : link "/blog" "Blog"
    : []
