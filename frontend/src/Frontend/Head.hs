{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend.Head (pageHead) where

import Data.Text
import Obelisk.Generated.Static
import Reflex.Dom

pageHead :: DomBuilder t m => m ()
pageHead = do
  elAttr "base" ("href" =: "/") blank
  el "title" $ text "RandomFloodFill"
  styleSheet $ static @"css/style.css"

--  styleSheet are functions to add links to html <head>
styleSheet :: DomBuilder t m => Text -> m ()
styleSheet myLink = elAttr "link" attrs blank
  where attrs = "rel" =: "stylesheet"
             <> "type" =: "text/css"
             <> "href" =: myLink
