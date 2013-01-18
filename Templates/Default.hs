{-# LANGUAGE OverloadedStrings #-}

module Templates.Default
    ( module Templates
    , defaultTemplate
    ) where

import Control.Applicative
import Text.Blaze.Html (Html, (!), preEscapedToHtml, toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Templates

defaultTemplate :: BlazeTemplate
defaultTemplate = BlazeTemplate $ \metadata ->
    page <$> metadata "title" <*> metadata "body" <*> metadata "url"
  where
    page title body url = H.docTypeHtml $ do

        H.head $ do
            H.meta ! A.charset "utf-8"
            H.title $ toHtml title
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
            H.p $ do
                "© 2012 Chris Wong. "
                H.a ! A.href "http://creativecommons.org/licenses/by-sa/3.0/" $ "Some rights reserved"
                ". Proudly generated with "
                H.a ! A.href "http://www.jaspervdj.be/hakyll/" $ "Hakyll"
                "."

defaultLinks :: [Link]
defaultLinks
    = Link "/index.html" "Home"
    : Link "/cv" "CV"
    : Link "/code" "Code"
    : Link "/blog" "Blog"
    : []

stylesheet :: String -> Html
stylesheet url = H.link ! A.rel "stylesheet" ! A.href (toValue url)
