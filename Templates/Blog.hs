{-# LANGUAGE OverloadedStrings #-}

module Templates.Blog
    ( blogPostTemplate
    , postItemTemplate
    , postListTemplate
    ) where

import Control.Applicative
import Text.Blaze.Html ((!), preEscapedToHtml, toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Templates.Core


blogPostTemplate :: BlazeTemplate
blogPostTemplate = BlazeTemplate $ \metadata ->
    build <$> metadata "date"
        <*> metadata "body"
  where
    build date body = do
        H.p ! A.id "post-date" $ do
            "(Posted on "
            H.time $ toHtml date
            ")"
        preEscapedToHtml body


-- | A single entry on the archive page.
postItemTemplate :: BlazeTemplate
postItemTemplate = BlazeTemplate $ \metadata ->
    build <$> metadata "title"
        <*> metadata "date"
        <*> metadata "description"
        <*> metadata "url"
  where
    build title date description url = do
        H.dt $ do
            H.a ! A.href (toValue url) $ toHtml title
            H.span ! A.class_ "date-thing" $ do
                -- keep en-dash and date together across newline
                " –\x2060\xA0"
                H.time $ toHtml date
        H.dd $ do
            preEscapedToHtml description


postListTemplate :: BlazeTemplate
postListTemplate = BlazeTemplate $ \metadata ->
    build <$> metadata "posts"
  where
    build posts = H.dl ! A.id "posts" $ preEscapedToHtml posts
