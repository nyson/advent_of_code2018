module Day1 where

import qualified Data.Set as S

modulations :: String -> [Int]
modulations = cycle . map read . lines . filter (/= '+')

part1 :: String -> IO Int
part1 = fmap (sum . modulations) . readFile

part2 :: String -> IO Int
part2 = fmap (fr S.empty . scanl1 (+) . modulations) . readFile
  where fr set (x:xs)
          | x `S.member` set = x
          | otherwise        = fr (S.insert x set) (xs)

