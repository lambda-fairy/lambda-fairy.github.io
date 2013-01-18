{-# LANGUAGE OverloadedStrings #-}

module Templates
    ( BlazeTemplate(..)
    , applyBlazeTemplate
    , Link(..)
    , matchLink
    , renderNavigation
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
    let metadata k = unContext context k item
    body <- renderHtml <$> runBlazeTemplate tpl metadata
    return $ itemSetBody body item

data Link = Link FilePath String

matchLink :: Link -> FilePath -> Bool
matchLink (Link href _) url
    = splitDirectories href `isPrefixOf` splitDirectories (normalise url)

-- | Render the navigation bar.
renderNavigation :: [Link] -> String -> Html
renderNavigation links url = H.ul $ foldMap process links
  where
    process link@(Link href label) =
        let addClass
              | matchLink link url = (! A.class_ "current")
              | otherwise = id
        in H.li $ addClass $ H.a ! A.href (toValue href) $ toHtml label
