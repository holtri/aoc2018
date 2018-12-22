import Data.List (sortBy, sort)
import qualified Data.Map as M

main :: IO ()
main = do
   input <- readFile "src/04/input"

   let parsed_input = map parse $ sort $ lines input
   let processed_log = sleeptimes parsed_input []
   let split_minutes = [(worker_id, minute) | (worker_id, minutes) <- processed_log, minute <- minutes]

   let most_sleeping_id = fst . findMaxByValue $ M.toList $ M.fromListWith (+) $ map (\x -> (fst x, 1)) split_minutes
   let minute_count = M.toList $ M.fromListWith (+) [(minute, 1) | (worker_id, minute) <- split_minutes, worker_id==most_sleeping_id]

   let wid = read most_sleeping_id :: Integer
   let top_min = fst $ findMaxByValue minute_count
   print ("Task 1: " ++ show (wid * top_min))

   let asleep_count = findMaxByValue $ M.toList $ M.fromListWith (+) $ map (\x -> (x, 1)) split_minutes
   let wid = read $ fst . fst $ asleep_count :: Integer
   let selected_min = snd . fst $ asleep_count
   print ("Task 2:" ++ show (wid * selected_min))

findMaxByValue :: (Ord b) => [(a,b)] -> (a, b)
findMaxByValue = foldl1 (\x y -> if snd x > snd y then x else y)

parse :: String -> (Integer, String)
parse logentry =
   let logparts = words logentry
       minute =  read $ take 2 $ drop 3 $ logparts !! 1  :: Integer
       action = logparts !! 3
   in  (minute, action)

-- [(0,"#10"),(27,"asleep"),(54,"up"),(42,"asleep")]
-- [("10", [42..?]), ("10", [27..53]), ("10", [])]

sleeptimes :: [(Integer, String)] -> [(String, [Integer])] -> [(String, [Integer])]
sleeptimes ((_, '#':id):xs) [] = sleeptimes xs [(id, [])]
sleeptimes ((_, '#':id):xs) acc = sleeptimes xs ((id, []):acc)
sleeptimes ((_, "up"):xs) acc = sleeptimes xs acc
sleeptimes ((x1, "asleep"):(x2,_):xs) acc = sleeptimes xs ((fst $ head acc, [x1..x2-1]):acc)
sleeptimes _ acc = acc
