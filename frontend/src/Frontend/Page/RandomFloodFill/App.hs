{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Frontend.Page.RandomFloodFill.App where

import Control.Monad.Trans (liftIO)
import Data.Word

pixelSize :: Integer
pixelSize = 2

floorDiv :: Integral a => a -> a -> a
x `floorDiv` y = (\(r, _) -> r) $ x `divMod` y

type Pixel = [Word8]
type Coords = [((Integer, Integer), Pixel)]

mkPixel :: [a] -> [a]
mkPixel color = concat $ take (fromIntegral (pixelSize * pixelSize)) $ repeat color

pos :: Integer -> Integer
pos x = x * pixelSize

blackPixel :: Pixel
blackPixel = mkPixel [0x00, 0x00, 0x00,0xff]

getState
  :: Integer
     -> Integer
     -> Integer
     -> Integer
     -> (Integer, [(Integer, Integer)])
     -> State
getState sw w sH h (gTick, toDraw) = State sw w sH h coords gTick
  where coords = zip toDraw $ repeat blackPixel

data State = State
  { screenWidth :: Integer
  , width :: Integer
  , screenHeight :: Integer
  , height :: Integer
  , pixelsToDraw :: [((Integer, Integer), Pixel)]
  , gameTick :: Integer
  } deriving (Show, Eq, Ord)

inBounds :: (Ord b, Ord a, Num b, Num a) => (b, a) -> b -> a -> Bool
inBounds (x, y) w h = and [x >= 0, x <= w, y >= 0, y <= h]

type Coord = (Integer, Integer)

toVisit :: (Coord -> Bool) -> [Coord] -> [Coord] -> [Integer] -> [Coord]
toVisit _ _ _ [] = []
toVisit inbounds visited frontier (r:randomIndexes) = toVisit'
  where (nextNode, frontierWithout) = popRandomNode r frontier
        nextNeighbors = filter (\x -> inbounds x && unseenNode x) $ neighbors nextNode
        unseenNode node = not (node `elem` visited || node `elem` frontier)
        newFrontier = frontierWithout ++ nextNeighbors
        toVisit' = case newFrontier of
                     [] -> [nextNode]
                     _ -> nextNode:(
                       toVisit inbounds (nextNode:visited) newFrontier randomIndexes)

neighbors :: (Integer, Integer) -> [(Integer, Integer)]
neighbors (x, y) = [(x-1, y), (x, y - 1), (x+1, y), (x, y+1)]

startingCoords :: Integer -> Integer -> IO Integer -> IO (Integer, Integer)
startingCoords maxWidth maxHeight rnd = do
  startingX <- rnd
  startingY <- rnd
  let x = (startingX `mod` maxWidth)
  let y = (startingY `mod` maxHeight)
  if inBounds (x + bW, y + bH) (bound maxWidth) (bound maxHeight)
  then pure (x + bW, y + bH)
  else startingCoords maxWidth maxHeight rnd
  -- else pure [(0,0)]
  where bound num = num - (num `floorDiv` 4)
        bW = maxWidth `floorDiv` 8
        bH = maxWidth `floorDiv` 8

coordsToVisit :: Integer -> Integer -> IO Integer -> IO [(Integer, Integer)]
coordsToVisit maxWidth maxHeight rnd = do
  (x, y) <- liftIO $ startingCoords maxWidth maxHeight rnd
  let rest = toVisit (\x' -> inBounds x' maxWidth maxHeight) [] [(x,y)] [1..]
  pure $ [(x, y)] ++ takeWhile (\coord -> inBounds coord maxWidth maxHeight) rest


removeIndex :: Int -> [a] -> [a]
removeIndex ind xs = (take (ind - 1) xs) ++ (drop ind xs)

popRandomNode :: Integer -> [a] -> (a, [a])
popRandomNode _ (x:[]) = (x, [])
popRandomNode rnd xs = ((xs !! removed), removeIndex removed xs)
  where removed = ((fromIntegral rnd) + 1) `mod` (length xs)
