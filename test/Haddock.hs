-- Check haddock completeness
module Main (main) where
import Data.Char      (isSpace, isDigit)
import Data.Maybe     (mapMaybe)
import Text.Read      (readMaybe)
import Control.Monad  (when)
import System.Exit    (exitFailure)
import System.Process (readProcess)

required :: Double
required = 90

main :: IO ()
main = do
    output <- readProcess "cabal" ["haddock"] ""
    when (any (<= required) $ match output) $ do
      putStrLn $ "Not all modules have " ++ show required
                 ++ "% of their content documented!"
      putStrLn output
      exitFailure

match :: String -> [Double]
match = mapMaybe matchPercent . lines
  where
    matchPercent = readMaybe . takeWhile isDigit . dropWhile isSpace