{-# LANGUAGE TupleSections #-}
module Day7 where

import Driver
import qualified Data.Map as M
import           Data.Map   (Map)
import           Debug.Trace
import           Data.List (sort)

newtype Dep = Dep (String, String)

dependsOn :: String -> String -> Dep
dependsOn a = Dep . (a,)

parseDep :: String -> Dep
parseDep = (\(_0:a:_1:_2:_3:_4:_5:b:_) -> (b `dependsOn` a)) . words

getDeps :: (String) -> [String] -> Int
getDeps e = lengthBy (== e)

deps :: [Dep] -> Map String String
deps xs = foldl f (M.fromList $ map (\(Dep (x, y)) -> (y, [])) xs) xs
  where f m (Dep (item, dep))
          | item `M.member` m = M.adjust (head dep:) item m
          | otherwise = M.insert item [head dep] m


getConstructionOrder :: Map String String -> Either String [String]
getConstructionOrder m
  | M.size m == 0 = return []
  | otherwise = do
      step <- trace (show m )
        $ case M.filterWithKey (\k v -> length v == 0) m of
          m' | M.size m' > 0 -> Right . map head . M.keys $ m'
          m' -> Left $ "assumed results with no deps but got "
                ++ show m' ++ "\nfrom " ++ show m


      let process :: Map String String -> Map String String
          process = trace ("Removed " ++ step)
            $ M.map (filter (not . (`elem` step))) . M.filter (/= "")

      (sort step:) <$> getConstructionOrder (process m)

