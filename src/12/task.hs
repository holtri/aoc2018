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
      task1 = run spreadPlan 20 (initialState, 0)
  print ("Task 1: " ++ show (potSum task1))

potSum (finalGen, offset) = sum $ map (+ offset) $ elemIndices '#' finalGen

run :: S.Set String -> Int -> (String, Int) -> (String, Int)
run spreadPlan 0 (xs, i) = (xs,i)
run spreadPlan n (xs, i) = run spreadPlan (n-1) (nextGen spreadPlan xs i)

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
