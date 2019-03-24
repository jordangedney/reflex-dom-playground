{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Page.RandomFloodFill.Page (randomfloodfill) where

import Data.Time (getCurrentTime)
import Control.Monad.Trans (liftIO)
import JSDOM.Types (MonadJSM, CanvasRenderingContext2D)
import Reflex.Dom
import Control.Monad.Ref
import Control.Monad.Fix
import GHC.IORef
import Reflex.Host.Class
import qualified Reflex.Dom.Core as RD
import Frontend.Page.RandomFloodFill.FrontendUtil
import Frontend.Page.RandomFloodFill.App
import System.Random

import qualified JSDOM.CanvasRenderingContext2D as C

alignToPixels :: Integer -> Integer
alignToPixels x = 2 * ((x `floorDiv` pixelSize) `floorDiv` 2)

randomfloodfill :: (DomBuilder t m, MonadReflexCreateTrigger t m, PostBuild t m,
    PerformEvent t m, MonadJSM m, MonadJSM (Performable m),
    TriggerEvent t m, HasJSContext m, HasJSContext (Performable m),
    HasDocument m, MonadRef m, MonadRef (Performable m), MonadHold t m,
    MonadSample t (Performable m), MonadFix m,
    DomBuilderSpace m ~ GhcjsDomSpace, Ref m ~ IORef,
    Ref (Performable m) ~ IORef) =>  m ()
randomfloodfill = divClass "canvas_container" $ do
  (width'', height'') <- screenSize
  let (w', h') = (width'' - 20, height'' - 20)
  let (canvasWidth, canvasHeight) = (alignToPixels w', alignToPixels h')
  let randomNum = randomIO :: IO Integer

  evStart <- RD.getPostBuild
  c <- liftIO $ coordsToVisit canvasWidth canvasHeight randomNum
  let pixelsToDraw = c <$ evStart

  evTick <- RD.tickLossy 0.01 =<< liftIO getCurrentTime
  -- evTick <- RD.tickLossy 1 =<< liftIO getCurrentTime
  dyGameTick <- RD.count evTick

  let dyToDraw = (\tick -> ((tick :: Integer), [c !! (fromIntegral tick)])) <$> dyGameTick
  let dyState = RD.traceDyn "" $ (getState w' canvasWidth h' canvasHeight) <$> dyToDraw

  dCx <- createBlankCanvas $
          ("style" =: "image-rendering: pixelated; background-color: white;") <>
          (canvasAttrs w' h')

  let renderer = (\state cx _ -> render state cx) <$> dyState
  _ <- drawWithCx dCx renderer (() <$ evTick)

  pure ()

render :: MonadJSM m => State -> CanvasRenderingContext2D -> m ()
render (State sW w sH h [] t) cx = do pure ()
render state@(State _ w _ h (p:pixels) t) cx = do
  let ((x, y), pix) = p
  drawPixel cx pix (x * pixelSize) (y * pixelSize)
  render state { toDraw = pixels } cx

drawPixel :: MonadJSM m => CanvasRenderingContext2D -> Pixel -> Integer -> Integer ->  m ()
drawPixel cx pixel xPos yPos = do
  img <- makeImageData pixelSize pixelSize pixel
  C.putImageData cx img (fromIntegral xPos) (fromIntegral yPos)
