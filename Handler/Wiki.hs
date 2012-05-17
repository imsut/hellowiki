{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Wiki where

import Import
import Data.List
import Data.Text
import Text.Blaze
import Text.Html

import Model.WikiForm

getWikiR :: String -> Handler RepHtml
getWikiR page = do
  Entity _ (Wiki title body) <- runDB $ getBy404 $ UniqueWiki $ pack page
  let bodyHtml = show $ linesToHtml $ Data.List.lines $ unpack body
  defaultLayout $ do
    setTitle $ toMarkup ("HelloWiki: " ++ unpack title)
    $(widgetFile "wiki")

postWikiR :: String -> Handler RepHtml
postWikiR page = do
  ((result, formWidget), formEnttype) <- runFormPost $ wikiForm Nothing
  case result of
    FormSuccess (title, body) -> do
      runDB $ do
        Entity wikiId _ <- getBy404 $ UniqueWiki $ pack page
        update wikiId [ WikiTitle =. title, WikiBody =. unTextarea body ]
      redirect $ WikiR $ unpack title
    _ -> undefined

