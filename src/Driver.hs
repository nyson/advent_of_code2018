module Driver where

perform :: FilePath -> (String -> a) -> IO a
perform file f = f <$> readFile file
