import Data.List.Split
import Data.List (sortBy, maximumBy, nub)
import Data.Function (on)
import qualified Data.Map as M

main :: IO ()
main = do
   input <- readFile "src/06/input"
   let parsedInput = map parse $ lines input
       border = borderLocations parsedInput
       candidates = M.toList $ M.fromListWith (+) [(x,1) | x <- mapLocations parsedInput, x `notElem` border]
       maximum_space = maximum $ map snd candidates
   print ("Task 1: " ++ show maximum_space)

toTuple [x,y] = (x,y)

parse x = toTuple $ map (\x-> read x :: Integer) $ splitOn "," x

dist :: (Integer, Integer) -> (Integer, Integer) -> Integer
dist (x1,y1) (x2,y2) = abs (x1 - x2) + abs (y1 - y2)

closestLocation :: (Integer, Integer) -> [(Integer, Integer)] -> Integer
closestLocation (x,y) xs =
   if snd (head mins) == snd (mins !! 1)
   then 0
   else fst $ head mins
   where distances = zip [1..fromIntegral(length xs)] (map (dist (x,y)) xs)
         mins = sortBy (compare `on` snd) distances

boundingBox xs =
    let x_min =  minimum $ map fst xs
        x_max =  maximum $ map fst xs
        y_min =  minimum $ map snd xs
        y_max =  maximum $ map snd xs
    in (x_min,x_max,y_min,y_max)

mapLocations xs =
   [closestLocation (x,y) xs | x <- [x_min..x_max], y <- [y_min..y_max]]
   where (x_min,x_max,y_min,y_max) = boundingBox xs

borderLocations :: [(Integer, Integer)] -> [Integer]
borderLocations xs =
      nub $ [closestLocation (x, y) xs | x <- [x_min..x_max], y <- [y_min, y_max]] ++
      [closestLocation (x, y) xs | x <- [x_min, x_max], y <- [y_min..y_max]]
      where (x_min,x_max,y_min,y_max) = boundingBox xs

tmp = [(1, 1), (1, 6), (8, 3), (3, 4), (5, 5), (8, 9)]
