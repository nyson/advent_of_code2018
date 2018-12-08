{-# LANGUAGE FlexibleInstances, RecordWildCards, OverloadedStrings, RankNTypes #-}
module Day3 where

import Data.Char (isDigit)
import Text.Read (readPrec, get, step, lexP)
import Data.List (group, sort, nubBy)

data Rect = Rect { claim  :: Int
                 , left   :: Int, top    :: Int
                 , width  :: Int, height :: Int
                 } deriving Show

instance Read Rect where
  readPrec = Rect
    <$> (get  >> step readPrec)
    <*> (lexP >> step readPrec)
    <*> (get  >> step readPrec)
    <*> (get  >> step readPrec)
    <*> (get  >> step readPrec)

-- | Positions covered by the rectangle
rectPos :: Rect -> [(Int, Int)]
rectPos Rect {..} = [ (x,y) | x <- [left .. left + width  - 1]
                            , y <- [top  .. top  + height - 1]
                            ]

-- | nubBy, but also checks against already nubbed elements
goodNubBy :: (a -> a -> Bool) -> [a] -> [a]
goodNubBy f xs = gnb f [] xs
  where gnb _ _  [] = []
        gnb f rms (x:xs)
          | any (f x) xs || any (f x) rms = gnb f (x:rms) xs
          | otherwise = x:gnb f rms xs

-- | check if two rectangles overlaps
overlaps :: Rect -> Rect -> Bool
overlaps r1 r2 = top r1                 < top r2  + height r2
                 && top r1  + height r1 > top r2
                 && left r1             < left r2 + width r2
                 && left r1 + width r1  > left r2

part1 :: FilePath -> IO Int
part1 = fmap f . readFile
  where f = length . filter ((> 1) . length)
            . group . sort
            . concatMap (rectPos . read) . lines

part2 :: FilePath -> IO Int
part2 = fmap (claim . head . goodNubBy overlaps . map read . lines) . readFile



