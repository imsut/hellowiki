{-# LANGUAGE ScopedTypeVariables #-}
module Handler.Edit where

import Data.Text
import Import

import Model.WikiForm

getEditR :: String -> Handler RepHtml
getEditR page = do
  Entity _ wiki <- runDB $ getBy404 $ UniqueWiki $ pack page
  (formWidget, formEnctype) <- generateFormPost $ wikiForm $ Just wiki
  defaultLayout $ do
    let url = WikiR page
    setTitle "Edit wiki page"
    $(widgetFile "edit")
