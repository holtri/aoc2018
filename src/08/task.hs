
main :: IO ()
main = do
   input <- readFile "src/08/input"
   let parsedInput =  map (read::String->Integer) $ words input
   print ("Task 1: " ++ show (processTree parsedInput))

processTree :: [Integer] -> Integer
processTree (a:b:xs) = acc
   where (acc, _ ) = process 0 a b xs

process :: Integer -> Integer -> Integer -> [Integer] -> (Integer, [Integer])
process acc _ _ [] = (acc, [])
process acc 0 payload xs = (acc + sum (take p xs), drop p xs)
   where p = fromInteger payload
process acc childs payload (c:p:xs) =
      let (acc2, remaining) = process acc c p xs
      in process acc2 (childs - 1) payload remaining

tmp = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]
