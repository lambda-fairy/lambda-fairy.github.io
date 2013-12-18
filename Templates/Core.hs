{-# LANGUAGE OverloadedStrings #-}

-- | Utilities for rendering Blaze templates and the navigation bar.

module Templates.Core
    (
    -- * Templates
      BlazeTemplate(..)
    , applyBlazeTemplate
    , applyBlazeTemplateList

    -- * Navigation bar
    , Link()
    , link
    , link'
    , matchLink
    , renderNavigation

    -- * Everything else
    , stylesheet
    ) where

import Control.Applicative
import Data.Foldable (foldMap)
import Data.List (isPrefixOf)
import System.FilePath.Posix
import Text.Blaze.Html.Renderer.String (renderHtml)
import Text.Blaze.Html (Html, (!), toHtml, toValue)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Hakyll


newtype BlazeTemplate = BlazeTemplate { runBlazeTemplate :: (String -> Compiler String) -> Compiler Html }


applyBlazeTemplate :: BlazeTemplate -> Context a -> Item a -> Compiler (Item String)
applyBlazeTemplate tpl context item = do
    body <- renderHtml <$> runBlazeTemplate tpl lookupMeta
    return $ itemSetBody body item
  where
    lookupMeta key = unContext context key item >>= \result ->
        case result of
            StringField s -> return s
            ListField{} -> error $ "Field '" ++ key ++ "' is a list. I don't like lists."


applyBlazeTemplateList :: BlazeTemplate -> Context a -> [Item a] -> Compiler String
applyBlazeTemplateList tpl context items = do
    items' <- mapM (applyBlazeTemplate tpl context) items
    return $ concatMap itemBody items'


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
renderNavigation :: [Link] -> FilePath -> Html
renderNavigation links current = H.ul $ foldMap process links
  where
    process l@(Link _ href label) =
        let addClass
              | matchLink l current = (! A.class_ "current")
              | otherwise = id
        in H.li $ addClass $ H.a ! A.href (toValue href) $ toHtml label


-- | Insert a stylesheet.
stylesheet :: String -> Html
stylesheet url = H.link ! A.rel "stylesheet" ! A.href (toValue url)
