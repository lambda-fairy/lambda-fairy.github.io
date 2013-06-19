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
blogPostTemplate = BlazeTemplate $ \get ->
    build <$> get "date" <*> get "body" <*> get "url"
  where
    build date body url = do
        H.p ! A.id "post-date" $ do
            H.a ! A.href (toValue url) ! A.title "link to this post" $ do
                H.time $ toHtml date
        preEscapedToHtml body


-- | A single entry on the archive page.
postItemTemplate :: BlazeTemplate
postItemTemplate = BlazeTemplate $ \get ->
    build <$> get "title" <*> get "date" <*> get "description" <*> get "url"
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
postListTemplate = BlazeTemplate $ \get ->
    build <$> get "posts"
  where
    build posts = H.dl ! A.id "posts" $ preEscapedToHtml posts
