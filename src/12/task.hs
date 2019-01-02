import qualified Data.Set as S
import qualified Data.List.Zipper as Z
import Data.List (elemIndex, elemIndices)
import Data.Maybe (fromMaybe)
import Debug.Trace

initialState= "..#..###...#####.#.#...####.#..####..###.##.#.#.##.#....#....#.####...#....###.###..##.#....#######"

main :: IO ()
main = do
  input <- readFile "src/12/input"
  let spreadPlan = S.fromList [head x | x <- words <$> lines input, last x /= "."]
      n = 20
      fps = finalPotSum $ run spreadPlan n ([],0) (initialState, 0)
  print ("Task 1: " ++ show fps)
  let n = 50000000000
      fps = finalPotSum $ run spreadPlan n ([],0) (initialState, 0)
  print ("Task 2: " ++ show fps)

finalPotSum (xs, i, diff, n') = potSum (xs, i) + diff * n'

potSum (gen, offset) = sum $ map (+ offset) $ elemIndices '#' gen

run :: S.Set String -> Int -> (String, Int) -> (String, Int) -> (String, Int, Int, Int)
run spreadPlan n (xs', i') (xs, i)
   | xs' == xs || n == 0= (xs, i, ps - ps', n)
   | otherwise = f
   where ps = potSum (xs, i)
         ps' = potSum (xs', i')
         f = run spreadPlan (n-1) (xs, i) (nextGen spreadPlan xs i)

nextGen :: S.Set String -> String -> Int -> (String, Int)
nextGen spreadPlan xs i
  | firstLeft < 4 = nextGen spreadPlan (replicate (4 - firstLeft) '.' ++ xs) (i - 4 + firstLeft)
  | firstLeft > 4 = nextGen spreadPlan (drop (firstLeft - 4) xs) (i + firstLeft - 4)
  | otherwise = (mutate spreadPlan [] xs, i)
     where firstLeft = fromMaybe (error "no living plant") $ elemIndex '#' xs

plant x spreadPlan
  | x `S.member` spreadPlan = '#'
  | otherwise = '.'

mutate spreadPlan [] xs = mutate spreadPlan ".." xs
mutate spreadPlan acc [] = reverse acc
mutate spreadPlan acc xs
  | length xs == 5 && xs /= terminate = mutate spreadPlan acc (xs ++ ".")
  | length xs == 5 && xs == terminate = reverse acc
  | otherwise = mutate spreadPlan (plant (take 5 xs) spreadPlan : acc) $ tail xs
  where terminate = "....."
