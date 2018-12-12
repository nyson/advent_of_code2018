module Driver where

perform :: FilePath -> (String -> a) -> IO a
perform file f = f <$> readFile file

-- | nubBy, but also checks against already nubbed elements
goodNubBy :: (a -> a -> Bool) -> [a] -> [a]
goodNubBy f xs = gnb f [] xs
  where gnb _ _  [] = []
        gnb f rms (x:xs)
          | any (f x) xs || any (f x) rms = gnb f (x:rms) xs
          | otherwise = x:gnb f rms xs

-- | Filters by predicate and count instances
lengthBy :: (a -> Bool) -> [a] -> Int
lengthBy _ [] = 0
lengthBy p (x:xs)
  | p x       = 1 + lengthBy p xs
  | otherwise = lengthBy p xs
