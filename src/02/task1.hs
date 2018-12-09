import System.IO
import Data.Map

main = do
  input <- readFile "02/input"
  let parsedInput = parse input
  let checksum = (countOccurences 2 parsedInput) * (countOccurences 3 parsedInput)
  print checksum

parse input = lines input

charCount s = toList $ fromListWith (+) [(c,1) | c <- s]

rev cc = [(b,a) | (a,b) <- cc]

ismember x s = member x (fromList (rev $ charCount s))

countOccurences d input = sum [1 | x <- input, ismember d x]
