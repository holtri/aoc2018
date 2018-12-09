import Data.Char
import Data.List.Split

main :: IO ()
main = do
   input <- readFile "03/input"
   print "eof"


   --   print (parseInput input)
   --
   -- parseInput =
   --   map (read::String->Int) $ filter (not . null) $ splitWhen (not . isDigit)
