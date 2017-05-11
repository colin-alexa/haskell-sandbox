{-# LANGUAGE OverloadedStrings #-}

module GoServ.Templates where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes

newAliasPage = 
  docTypeHtml $
    body $
      H.form ! action "" ! method "POST" $
        (input ! type_ "text" ! name "alias_name" ! placeholder "alias to...") >>
        (button ! type_ "submit" $ "Submit")
