{-# LANGUAGE FlexibleInstances, RecordWildCards #-}
module Day3 where

import Data.Char (isDigit)

import           Data.Default
import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import           Data.List (group, sort, nubBy)
import qualified Data.Vector.Generic.Mutable as VM
import qualified Data.Vector.Generic         as V
import           Data.Vector (Vector)

data Rect = Rect { claim  :: Int
                 , left   :: Int, top    :: Int
                 , width  :: Int, height :: Int
                 }

rectPos :: Rect -> [(Int, Int)]
rectPos Rect {..} = [ (x,y) | x <- [left .. left + width - 1]
                            , y <- [top .. top + height - 1]
                            ]

parseClaim :: String -> Rect
parseClaim ('#':s)
    = let (claim, ' ':'@':' ':rest) = break (not . isDigit) s
          (left, ',':rest1) = break (not . isDigit) rest
          (top, ':':' ':rest2) = break (not . isDigit) rest1
          (width, 'x':rest3) = break (not . isDigit) rest2
          height = takeWhile isDigit rest3
      in Rect { claim=  read claim
              , left=   read left
              , top=    read top
              , width=  read width
              , height= read height }

part1 :: FilePath -> IO Int
part1 = fmap (length . filter ((> 1) . length) . group . sort . concatMap (rectPos . parseClaim) . lines) . readFile


goodNubBy :: (a -> a -> Bool) -> [a] -> [a]
goodNubBy f xs = gnb f [] xs
  where gnb _ _  [] = []
        gnb f rms (x:xs)
          | any (f x) xs || any (f x) rms = gnb f (x:rms) xs
          | otherwise = x:gnb f rms xs

overlaps :: Rect -> Rect -> Bool
overlaps r1 r2 = top r1 < top r2 + height r2 && top r1 + height r1 > top r2
             && left r1 < left r2 + width r2 && left r1 + width r1 > left r2

part2 :: FilePath -> IO Int
part2 = fmap (claim . head . goodNubBy overlaps . map parseClaim . lines) . readFile



