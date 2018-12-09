import Data.Char
import Data.List.Split

n = 1000

main :: IO ()
main = do
   input <- readFile "src/03/input"
   let parsedInput = map parse $ lines input
       output = intersectCuts $ map getcut $ take 20 parsedInput
   print output

parse =
   let splitbynondigit = splitWhen (not . isDigit)
       filterempty = filter (not . null)
   in map (read::String->Integer) . filterempty . splitbynondigit

inbox :: (Num t, Num t1, Ord t1) => (t1, t1) -> [t1] -> t
inbox (x,y) [id,from_left,from_top,width,height] =
   if in_horizontal && in_vertical then 1 else 0
   where in_horizontal = x  > from_left && x <= from_left + width
         in_vertical = y > from_top && y <= from_top + height

getcut claim = [(x,y) `inbox` claim | x <- [1..n], y <- [1..n]]

intersectCuts :: (Foldable t, Integral c) => t [c] -> Int
intersectCuts = length . filter (> 1) . foldl1 (zipWith (+))

a = [1,1,3,4,4]
b = [2,3,1,4,4]
c = [3,5,5,2,3]

-- input <- readFile "src/03/input"
-- x = map parseInput $lines input
