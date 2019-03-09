{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}

-- | Utilities for rendering Lucid templates and the navigation bar.

module Templates.Core
    (
    -- * Templates
      LucidTemplate(..)
    , applyLucidTemplate
    , applyLucidTemplateList
    , getStringField

    -- * Navigation bar
    , Link()
    , link
    , link'
    , matchLink
    , renderNavigation

    -- * Everything else
    , stylesheet
    ) where

import Control.Monad.Trans.Class
import Data.List (isPrefixOf)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import System.FilePath.Posix

import Hakyll
import Lucid


newtype LucidTemplate = LucidTemplate { runLucidTemplate :: (String -> Compiler ContextField) -> HtmlT Compiler () }


applyLucidTemplate :: LucidTemplate -> Context a -> Item a -> Compiler (Item String)
applyLucidTemplate tpl context item = do
    body <- fmap LText.unpack . renderTextT $ runLucidTemplate tpl lookupMeta
    return $ itemSetBody body item
  where
    lookupMeta key = unContext context key [] item


applyLucidTemplateList :: LucidTemplate -> Context a -> [Item a] -> Compiler String
applyLucidTemplateList tpl context items = do
    items' <- mapM (applyLucidTemplate tpl context) items
    return $ concatMap itemBody items'


getStringField :: Compiler ContextField -> HtmlT Compiler String
getStringField m = do
    f <- lift m
    case f of
        StringField f' -> return f'
        _ -> error "expected string"


-- | Represents an entry on the navigation bar.
--
-- Links can be constructed using the 'link' and 'link'' functions, and
-- matched using 'matchLink'.
--
data Link = Link
    { _pattern :: FilePath
    , _href    :: FilePath
    , _label   :: String
    }


-- | Construct a link, given a URL and label.
link
    :: FilePath  -- ^ Destination URL
    -> String    -- ^ Label
    -> Link
link href label = Link href href label


-- | Construct a link, using different URLs for matching and display.
--
-- For example, to create a link to the home page, you would use:
--
-- > home = link' "/index.html" "/" "Home"
--
-- The result will link to @/@, but @/index.html@ will be used for
-- deciding whether the link will be highlighted or not.
--
link'
    :: FilePath  -- ^ URL to match against
    -> FilePath  -- ^ Destination URL
    -> String    -- ^ Label
    -> Link
link' = Link


-- | Given the path of the current page, determine whether this link
-- should be highlighted or not.
matchLink :: Link -> FilePath -> Bool
matchLink (Link pattern _ _) current
    = splitDirectories pattern `isPrefixOf` splitDirectories (normalise current)


-- | Render the navigation bar.
renderNavigation :: Monad m => [Link] -> FilePath -> HtmlT m ()
renderNavigation links current = ul_ $ foldMap process links
  where
    process :: Monad m => Link -> HtmlT m ()
    process l@(Link _ href label) =
        li_ [class_ "current" | matchLink l current] $
            a_ [href_ (Text.pack href)] (toHtml label)


-- | Insert a stylesheet.
stylesheet :: Monad m => String -> HtmlT m ()
stylesheet url = link_ [rel_ "stylesheet", href_ (Text.pack url)]
