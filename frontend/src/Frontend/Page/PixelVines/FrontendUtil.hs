{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Page.PixelVines.FrontendUtil where

import JSDOM (currentWindowUnchecked)
import JSDOM.Generated.Window (getInnerWidth, getInnerHeight)

import Reflex.Host.Class
import Control.Monad.Ref
import Control.Monad.Fix
import GHC.IORef

import JSDOM.Types (
  Uint8ClampedArray(..),
  MonadJSM, JSM, Uint8ClampedArray, liftDOM, CanvasRenderingContext2D)

import qualified Reflex.Dom.CanvasDyn as CDyn
import qualified Reflex.Dom.CanvasBuilder.Types as CBT
import qualified Data.Text as T
import qualified Data.Map as Map
import Reflex.Dom
import qualified Reflex.Dom.Core as RD
import qualified Data.ByteString.Base64 as B64 (encode)
import GHCJS.Buffer (getArrayBuffer, MutableBuffer)
import Data.Word
import JSDOM.ImageData
import qualified GHCJS.DOM                      as JSDOM
import qualified Data.ByteString as BS
import GHCJS.Buffer.Types (SomeBuffer(..))
import Language.Javascript.JSaddle (jsg, jsg1, new, pToJSVal, ghcjsPure, liftJSM)
import Data.Text.Encoding (decodeUtf8)

canvasAttrs :: Integer -> Integer -> Map.Map T.Text T.Text
canvasAttrs width height =
  ("width" =: (T.pack . show $ width)) <>
  ("height" =: (T.pack . show $ height)) <>
  ("style" =: "background-color: white")

createBlankCanvas
  :: (DomBuilder t m,
      Reflex.Host.Class.MonadReflexCreateTrigger t m, PostBuild t m,
      PerformEvent t m, MonadJSM m, MonadJSM (Performable m),
      TriggerEvent t m, HasJSContext m, HasJSContext (Performable m),
      HasDocument m, Control.Monad.Ref.MonadRef m,
      Control.Monad.Ref.MonadRef (Performable m), MonadHold t m,
      Control.Monad.Fix.MonadFix m, MonadSample t (Performable m),
      DomBuilderSpace m ~ GhcjsDomSpace,
      Control.Monad.Ref.Ref m ~ GHC.IORef.IORef,
      Control.Monad.Ref.Ref (Performable m) ~ GHC.IORef.IORef) =>
    Map.Map T.Text T.Text
  -> m (Dynamic t JSDOM.Types.CanvasRenderingContext2D)
createBlankCanvas attrs = do
  (innerEle, _) <- elAttr' "canvas" attrs blank

  let emptyConfig = CBT.CanvasConfig innerEle mempty
      innerCanvasInfo = CDyn.dContext2d emptyConfig

  dCx <- (fmap . fmap) CBT._canvasInfo_context innerCanvasInfo
  return dCx

screenSize :: (MonadJSM m) =>  m (Integer, Integer)
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

makeImageData :: MonadJSM m => Integer -> Integer -> [Word8] -> m ImageData
makeImageData x y z = do
  x' <- liftJSM $ makeImageData' x y z
  return x'

drawWithCx
  :: (PerformEvent t1 m, MonadJSM (Performable m))
  => Dynamic t1 t2
  -> Dynamic t1 (t2 -> Double -> JSM a1)
  -> Event t1 a2
  -> m (Event t1 a1)
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
