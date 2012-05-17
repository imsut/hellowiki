module Handler.New where

import Data.Text
import Import

import Model.WikiForm

getNewR :: Handler RepHtml
getNewR = do
  (formWidget, formEnctype) <- generateFormPost $ wikiForm Nothing
  defaultLayout $ do
    setTitle "Creating new wiki page"
    $(widgetFile "new")

postNewR :: Handler RepHtml
postNewR = do
  ((result, _), _) <- runFormPost $ wikiForm Nothing
  case result of
    FormSuccess (title, body) -> do
      runDB $ insert $ Wiki title $ unTextarea body
      redirect $ WikiR $ unpack title
    _ -> undefined
