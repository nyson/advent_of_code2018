{-# LANGUAGE LambdaCase #-}
module Day4 where

import Data.List (group, sort, sortBy)
import Text.Read as TR

part1 :: String -> [Log]
part1 = sortBy (compareBy time) . map read . lines

compareBy :: Ord g => (a -> g) -> a -> a -> Ordering
compareBy f a b = f a `compare` f b

guards :: [Log] -> [Int]
guards = map head . group . sort . map (\(ShiftBegins x) -> x) . filter isGuard . map action
  where isGuard (ShiftBegins _) = True
        isGuard _ = False

-- [1518-11-01 00:00] Guard #10 begins shift
-- [1518-11-01 00:05] falls asleep
-- [1518-11-01 00:25] wakes up

data Action = ShiftBegins Int | FallsAsleep | WakesUp
  deriving Show

instance Read Action where
  readPrec = lexP >>= \case
      TR.Ident "Guard" -> ShiftBegins <$> (lexP >> step readPrec <* lexP <* lexP)
      TR.Ident "falls" -> lexP >> return FallsAsleep
      TR.Ident "wakes" -> lexP >> return WakesUp
      badLex -> error $ "Bad lexeme: " ++ show badLex
data Log = Log { time :: OldTime
               , action :: Action
               } deriving Show

instance Read Log where
  readPrec = Log
    <$> step readPrec
    <*> step readPrec

data OldTime = OldTime
  { year :: Int
  , month :: Int
  , day :: Int
  , hour :: Int
  , minutes :: Int
  } deriving (Show, Eq, Ord)

instance Read OldTime where
  readPrec = OldTime
    <$> (get >> step readPrec)
    <*> (get >> step readPrec)
    <*> (get >> step readPrec)
    <*> step readPrec
    <*> (get >> step readPrec <* get)
