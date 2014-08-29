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


blogPostTemplate = BlazeTemplate $ \get ->
    build <$> get "date" <*> get "body" <*> get "url"
  where
    build date body url = do
        H.p ! A.class_ "timestamp" $ do
            H.a ! A.href (toValue url) ! A.title "link to this post" $ do
                H.time $ toHtml date
        preEscapedToHtml body


-- | A single entry on the archive page.
postItemTemplate = BlazeTemplate $ \get ->
    build <$> get "title" <*> get "date" <*> get "url"
  where
    build title date url = do
        H.li $ do
            H.a ! A.href (toValue url) $ toHtml title
            " "
            H.time ! A.class_ "timestamp" $ toHtml date


postListTemplate = BlazeTemplate $ \get ->
    build <$> get "posts"
  where
    build posts = H.ul ! A.id "posts" $ preEscapedToHtml posts
