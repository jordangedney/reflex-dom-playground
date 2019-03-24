{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Page.RandomFloodFill.FrontendUtil where

import JSDOM (currentWindowUnchecked)
import JSDOM.Generated.Window (getInnerWidth, getInnerHeight)

import JSDOM.Types (liftDOM, Uint8ClampedArray(..), RenderingContext(..))
import JSDOM.Types (MonadJSM, CanvasRenderingContext2D, JSM, Uint8ClampedArray, liftDOM)
import qualified Reflex.Dom.CanvasDyn as CDyn
import qualified Reflex.Dom.CanvasBuilder.Types as CBT
import qualified Data.Text as T
import qualified Data.Map as Map
import Obelisk.Frontend (ObeliskWidget)
import Reflex.Dom
import Control.Monad.Ref
import Control.Monad.Fix
import GHC.IORef
import Reflex.Host.Class
import qualified Reflex.Dom.Core as RD
import Frontend.Page.RandomFloodFill.App
import qualified Data.ByteString.Base64 as B64 (encode)
import GHCJS.Buffer (getArrayBuffer, MutableBuffer)
import Data.Word
import JSDOM.ImageData
import qualified GHCJS.DOM                      as JSDOM
import qualified Data.ByteString as BS
import GHCJS.Buffer.Types (SomeBuffer(..))
import Language.Javascript.JSaddle (js, js1, jss, jsg, jsg1,
                                    new, pToJSVal, GHCJSPure(..), ghcjsPure, JSM,
                                    fromJSVal, toJSVal, Object, liftJSM)
import Data.Text.Encoding (decodeUtf8)

canvasAttrs :: Integer -> Integer -> Map.Map T.Text T.Text
canvasAttrs width height =
  ("width" =: (T.pack . show $ width)) <>
  ("height" =: (T.pack . show $ height)) <>
  ("style" =: "background-color: white")

createBlankCanvas attrs = do
  (innerEle, _) <- elAttr' "canvas" attrs blank

  let emptyConfig = CBT.CanvasConfig innerEle mempty
      innerCanvasInfo = CDyn.dContext2d emptyConfig

  dCx <- (fmap . fmap) CBT._canvasInfo_context innerCanvasInfo
  return dCx

screenSize :: (MonadJSM m, DomBuilder t m) =>  m (Integer, Integer)
screenSize = do
  w <- currentWindowUnchecked
  width <- getInnerWidth w
  height <- getInnerHeight w
  return (fromIntegral width, fromIntegral height)

-- https://stackoverflow.com/questions/43571803/how-to-convert-a-bytestring-value-to-a-jsval
uint8ClampedArrayFromByteString :: BS.ByteString -> JSM (Uint8ClampedArray)
uint8ClampedArrayFromByteString bs = do
  buffer <- SomeBuffer <$> jsg1 (T.pack "h$newByteArrayFromBase64String")
                                (decodeUtf8 $ B64.encode bs)
  arrbuff <- ghcjsPure (getArrayBuffer (buffer :: MutableBuffer))
  liftDOM (Uint8ClampedArray <$> new (jsg (T.pack "Uint8ClampedArray")) [pToJSVal arrbuff])

makeImageData' :: Integer -> Integer -> [Word8] -> JSM ImageData
makeImageData' width height dat
  = do dat' <-  (uint8ClampedArrayFromByteString (BS.pack dat))
       newImageData dat' (fromIntegral width) (Just (fromIntegral height))

makeImageData x y z = do
  x' <- liftJSM $ makeImageData' x y z
  return x'

-- | Run functions using the raw canvas context object. This will be either a
-- Context2D or ContextWebGL depending on which type you initialised.
drawWithCx
  :: ( MonadWidget t m
     , CBT.HasRenderFn c ( CBT.RenderContext c )
     )
  => Dynamic t ( CBT.RenderContext c )
  -> Dynamic t ( CBT.RenderContext c -> Double -> JSM a )
  -> Event t ()
  -> m ( Event t a )
drawWithCx dContext dAction eApply =
  let
    nextFrame cx f = liftJSM $
      JSDOM.nextAnimationFrame (f cx)
  in
    RD.performEvent
    ( nextFrame
      <$> RD.current dContext
      <*> RD.current dAction
      <@ eApply
    )
