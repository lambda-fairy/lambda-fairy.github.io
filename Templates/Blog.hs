{-# LANGUAGE OverloadedStrings #-}

module Templates.Blog where


import qualified Data.Text as Text
import Lucid

import Templates.Core


blogPostTemplate = LucidTemplate $ \ask -> do
    date <- getStringField $ ask "date"
    body <- getStringField $ ask "body"
    url <- getStringField $ ask "url"

    toHtmlRaw body

    p_ $ small_ $ do
        "(Posted on "
        a_ [href_ (Text.pack url), title_ "link to this post"] $
            time_ $ toHtml date
        ".)"


-- | A single entry on the archive page.
postItemTemplate = LucidTemplate $ \ask -> do
    title <- getStringField $ ask "title"
    date <- getStringField $ ask "date"
    url <- getStringField $ ask "url"

    li_ $ do
        a_ [href_ (Text.pack url)] $ toHtml title
        " "
        small_ $ time_ $ toHtml date


postListTemplate = LucidTemplate $ \ask -> do
    posts <- getStringField $ ask "posts"

    ul_ [id_ "posts"] $ toHtmlRaw posts
