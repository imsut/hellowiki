module Model.WikiForm where

import Import

wikiForm :: Maybe Wiki -> Form (Text, Textarea)
wikiForm wiki = renderDivs $ (,)
           <$> areq textField "Page title" (wikiTitle <$> wiki)
           <*> areq textareaField "Contents" (Textarea <$> wikiBody <$> wiki)
