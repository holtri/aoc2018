import qualified Data.Map as M

main :: IO ()
main = do
   print ("Task 1: " ++ show (maximum runGame))

-- numPlayers = 10; lastMarble = 25
-- numPlayers = 10; lastMarble = 1618
-- numPlayers = 13; lastMarble = 7999
-- numPlayers = 17; lastMarble = 1104
-- numPlayers = 21; lastMarble = 6111
-- numPlayers = 30; lastMarble = 5807
numPlayers = 459; lastMarble = 71320

addMarble xs m =
  let (l,r) = splitAt 2 xs
  in m:r ++ l

runGame = play scores [0] 1
  where scores = M.fromList $ zip [1..numPlayers] (repeat 0)

update scores player m = M.adjustWithKey (\key x -> x + m) player scores

play :: M.Map Int Int -> [Int] -> Int -> M.Map Int Int
play scores xs m
  | m > lastMarble = scores
  | m `mod` 23 == 0 =
    let player = m `mod` numPlayers
        (ls, r:rs) = splitAt (length xs - 7) xs
        updatedScores = update scores player (m + r)
        updatedMarbles = rs ++ ls
    in play updatedScores updatedMarbles (m + 1)
  | otherwise = play scores (addMarble xs m) (m + 1)
