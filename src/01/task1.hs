
main :: IO ()
main = do
  input <- readFile "01/input1"
  print (sum $ parse input)

cleanInput :: String -> String
cleanInput [] = []
cleanInput (x:xs)
    | x == '+' = xs
    | otherwise = x:xs

parse :: String -> [Int]
parse input = do
    let x = lines input
    map ((read::String->Int) . cleanInput) x
