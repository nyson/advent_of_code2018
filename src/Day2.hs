{-# LANGUAGE TupleSections #-}
module Day2 where

import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import           Data.List (sort, intersect)
import           Data.Bifunctor

multiChars :: String -> [Int]
multiChars = squash . filter (> 1) . eqs . sort
  where eqs [] = []
        eqs (x:xs) = uncurry (:) . bimap ((+1) . length) eqs
                     $ break (/= x) xs
        squash (x:xs@(y:_))
          | x == y = squash xs
          | otherwise = x:squash xs
        squash xs = xs

part1 :: String -> IO Int
part1 = fmap pt1 . readFile
  where count im x
          | x `IM.member` im = IM.adjust (+1) x im
          | otherwise        = IM.insert x 1 im
        pt1 = product . map snd . IM.toList
              . foldl (foldl count) IM.empty
              . map multiChars . lines


part2 :: String -> IO String
part2 = fmap pt2 . readFile
  where
    pt2 = head . map (uncurry intersect) . rowsWithDiffs . lines
    rowsWithDiffs [] = []
    rowsWithDiffs (x:xs) = map (x,) (filter ((== 1) . diff x) xs)
                   ++ rowsWithDiffs xs
    diff xs = length . filter (\(a, b) -> a /= b) . zip xs

