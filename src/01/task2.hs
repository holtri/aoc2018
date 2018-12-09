
main :: IO ()
main = do
  input <- readFile "01/input1"
  print (findMatch [] (cycle $ parse input))

cleanInput :: String -> String
cleanInput [] = []
cleanInput (x:xs)
    | x == '+' = xs
    | otherwise = x:xs

parse :: String -> [Int]
parse input = map ((read::String->Int) . cleanInput) $ lines input

findMatch :: (Eq a, Num a) => [a] -> [a] -> a
findMatch [] (y:ys) = findMatch [y] ys
findMatch _ [] = error "pattern not found"
findMatch history@(x:_) (y:ys)
    | s `elem` history = s
    | otherwise = findMatch (s:history) ys
    where s = x + y
