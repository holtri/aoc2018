import qualified Data.Set as S
import Data.Char
import Text.Regex.Posix
import Control.Arrow

main :: IO()
main = do
    input <- readFile "src/10/input"
    let (positions, velocities) = unzip $ map parse $ lines input
        startAt = 10000
        initialPositions = next [(fst v * startAt, snd v * startAt) | v <- velocities] positions
        x = map printGrid $ take 100 $ iterate (next velocities) initialPositions
    showGrid x startAt

showGrid [] i = putStr "end"
showGrid (x:xs) i = do
    putStr $ "Message " ++ show i ++ "\n" ++ x ++ "\n\n"
    showGrid xs (i+1)

symbol x xs
    | x `elem` xs = '#'
    | otherwise = '.'

next velocities positions = [((+) (fst p) *** (+) (snd p)) v | (p,v) <- zip positions velocities]

printGrid :: [(Int,Int)] -> String
printGrid points =
    let max_y = maximum $ map snd points
        min_y = minimum $ map snd points
        max_x = maximum $ map fst points
        min_x = minimum $ map fst points
    in if ((max_x - min_x) < 100) && ((max_y - min_y) < 100)
    then
        let gridLines = [[symbol (x,y) points | x <- [min_x..max_x]] | y <- [min_y..max_y]]
        in foldl1 (\a b -> a ++ "\n" ++ b) gridLines
    else "out of limits"

parse :: String -> ((Int,Int), (Int,Int))
parse l =
    let [x,y,vx,vy] = map (read::String->Int) $ filter (/=empty) $ map (filter (\x -> isDigit x || x == '-')) $ words l
    in ((x,y),(vx, vy))
