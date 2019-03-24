{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Frontend where

import Frontend.Head
import Frontend.Page.RandomFloodFill.Page

import Obelisk.Frontend
import Obelisk.Route
import Reflex.Dom.Core

--import qualified Reflex.Dom as RD
-- import Reflex.Dom

-- import Common.Api
import Common.Route
-- import Obelisk.Generated.Static

frontend ::  Frontend (R FrontendRoute)
frontend = Frontend
  { _frontend_head = pageHead
  , _frontend_body = do
      el "header" $ do
      _ <- mainContainer $ do
        prerender nothing randomfloodfill
      return ()
  }

nothing :: DomBuilder t m => m ()
nothing = do
  el "hello" blank

-- | The @<main>@ tag that will contain most of the site's content
mainContainer :: DomBuilder t m => m () -> m (Event t ())
mainContainer w = domEvent Click . fst <$> el' "main" w