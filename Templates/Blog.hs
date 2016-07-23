{-# LANGUAGE OverloadedStrings #-}

module Templates.Blog where


import Control.Monad.Trans.Class
import qualified Data.Text as Text
import Hakyll
import Lucid

import Templates.Core


blogPostTemplate = LucidTemplate $ \ask -> do
    StringField date <- lift $ ask "date"
    StringField body <- lift $ ask "body"
    StringField url <- lift $ ask "url"

    p_ [class_ "timestamp"] $
        a_ [href_ (Text.pack url), title_ "link to this post"] $
            time_ $ toHtml date

    toHtmlRaw body


-- | A single entry on the archive page.
postItemTemplate = LucidTemplate $ \ask -> do
    StringField title <- lift $ ask "title"
    StringField date <- lift $ ask "date"
    StringField url <- lift $ ask "url"

    li_ $ do
        a_ [href_ (Text.pack url)] $ toHtml title
        " "
        time_ [class_ "timestamp"] $ toHtml date


postListTemplate = LucidTemplate $ \ask -> do
    StringField posts <- lift $ ask "posts"

    ul_ [id_ "posts"] $ toHtmlRaw posts
