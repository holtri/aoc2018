import Data.Char
import Data.List (maximumBy)
import Data.Function (on)

main :: IO ()
main = do
   input <- readFile "src/05/input"
   let parsedInput = init input
   print ("Task 1: " ++ show (length $ reducePolymer [] parsedInput))
   let droppedPolymers = map ((\f -> reducePolymer [] (f parsedInput)) . dropChar) ['a'..'z']
   print ("Task 2: " ++ show (minimum $ map length droppedPolymers))

reducePolymer :: String -> String -> String

reducePolymer [] (x:xs) = reducePolymer [x] xs
reducePolymer acc [] = reverse acc
reducePolymer acc@(a:as) (x:xs)
   | abs (ord a - ord x) == 32 = reducePolymer as xs
   | otherwise = reducePolymer (x:acc) xs

dropChar c = filter (\x -> x /= toLower c && x /= toUpper c)
