import System.IO
import qualified Data.Map as M

main = do
  input <- readFile "src/02/input"
  let parsedInput = lines input
      checksum = countOccurences 2 parsedInput * countOccurences 3 parsedInput
  print ("Task 1 : " ++ show checksum)
  print ("Task 2 : " ++ show (findId parsedInput))

charCount s = M.toList $ M.fromListWith (+) [(c,1) | c <- s]

rev cc = [(b,a) | (a,b) <- cc]

contains digit_count s = M.member digit_count (M.fromList (rev $ charCount s))

countOccurences digit_count input = sum [1 | x <- input, contains digit_count x]

stringOverlap s1 s2 = [c1 | (c1, c2) <- zip s1 s2, c1 == c2]

findId :: [String] -> String
findId (x:xs) =
    case overlap of
        [] -> findId xs
        (y:ys) -> y
    where overlap = dropWhile (\i -> length i /= length x - 1) $ map (stringOverlap x) xs
