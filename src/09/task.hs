import qualified Data.Map as M
import qualified Data.List.Zipper as Z

main :: IO ()
main = do
  let numPlayers = 459; lastMarble = 71320
  print ("Task 1: " ++ show (maximum $ runGame numPlayers lastMarble))
  print ("Task 2: " ++ show (maximum $ runGame numPlayers (lastMarble*100)))

moveLeft :: Z.Zipper Int -> Int -> Z.Zipper Int
moveLeft z@(Z.Zip [] _) c = move (Z.end z) c
moveLeft z c = move (Z.left z) (c+1)

moveRight :: Z.Zipper Int -> Int -> Z.Zipper Int
moveRight z@(Z.Zip _ []) c = move (Z.start z) c
moveRight z c = move (Z.right z) (c-1)

move :: Z.Zipper Int -> Int -> Z.Zipper Int
move z c
  | c < 0 = moveLeft z c
  | c > 0 = moveRight z c
  | otherwise = z

runGame :: Int -> Int -> M.Map Int Int
runGame numPlayers lastMarble = play scores (Z.fromList [0]) 1 numPlayers lastMarble
  where scores = M.fromList $ zip [1..numPlayers] (repeat 0)

update :: M.Map Int Int -> Int -> Int -> M.Map Int Int
update scores player m = M.adjustWithKey (\key x -> x + m) player scores

play :: M.Map Int Int -> Z.Zipper Int -> Int -> Int -> Int -> M.Map Int Int
play scores xs m numPlayers lastMarble
  | m > lastMarble = scores
  | m `mod` 23 == 0 =
    let player = m `mod` numPlayers
        xs' = move xs (-7)
        removedMarble = Z.cursor xs'
        xs'' = Z.delete xs'
        updatedScores = update scores player (m + removedMarble)
    in play updatedScores xs'' (m + 1) numPlayers lastMarble
  | otherwise = play scores xs' (m + 1) numPlayers lastMarble
      where xs' = Z.insert m $ move xs 2
