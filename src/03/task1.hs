import Data.Char
import Data.List.Split
import qualified Data.Map as M

n = 1000

main :: IO ()
main = do
   input <- readFile "src/03/input"
   let parsedInput = map parse $ lines input
       output = countIntersects $ parsedInput
   print output

parse =
   let splitbynondigit = splitWhen (not . isDigit)
       filterempty = filter (not . null)
   in map (read::String->Integer) . filterempty . splitbynondigit

inbox :: (Num t1, Ord t1) => (t1, t1) -> [t1] -> Bool
inbox (x,y) [id,from_left,from_top,width,height] =
   in_horizontal && in_vertical
   where in_horizontal = x  > from_left && x <= from_left + width
         in_vertical = y > from_top && y <= from_top + height

getcut :: Num a => [Integer] -> M.Map (Integer, Integer) a
getcut claim = M.fromList [((x,y), 1) | x <- [1..n], y <- [1..n], (x,y) `inbox` claim]

a = [1,1,3,4,4]
b = [2,3,1,4,4]
c = [3,5,5,2,3]
x = [a,b,c]
-- input <- readFile "src/03/input"
-- x = map parseInput $lines input

countIntersects =
   M.size . M.filter (>1) . intersectCuts
   where intersectCuts = foldl (M.unionWith (+)) M.empty . map getcut
