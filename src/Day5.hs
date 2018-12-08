module Day5 where

import Data.List (group, sort)
import Data.Char (isLower, toLower, toUpper, isAlpha)

part1 :: String -> Int
part1 = length . rc [] . filter isAlpha
  where canRec :: Char -> Char -> Bool
        canRec c1 c2 = toLower c1 == toLower c2 && isLower c1 /= isLower c2

        rc buf [] = reverse buf
        rc [] (x:xs) = rc [x] xs
        rc (x:xs) (y:ys) | x `canRec` y = rc xs ys
                         | otherwise = rc (y:x:xs) ys

part2 :: String -> Int
part2 s = head . sort . map (reactWithoutPolymer s) $ (polysInChain s)
  where polysInChain = map head . group . sort . map toLower
        reactWithoutPolymer chain c = part1 $ filter ((/= c) . toLower) s
