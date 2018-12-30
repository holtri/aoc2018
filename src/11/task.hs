import Data.Char
import qualified Data.Map as M

input = 9306

main :: IO ()
main =
  let maxCell = foldl1 (\x y -> if snd x > snd y then x else y) $ cellLevels input
  in putStr (show maxCell)

cellLevels sn = [ ((x,y), totalPower (x,y) sn) | x <- [1..297], y <- [1..297]]

totalPower :: (Int, Int) -> Int -> Int
totalPower (x, y) sn =  sum [ powerLevel (xc, yc) sn | xc <- [x..x+2], yc <- [y..y+2]]

powerLevel :: (Int, Int) -> Int -> Int
powerLevel (x, y) sn = digitToInt (reverse (show p) !! 2) - 5
  where rackId = x + 10
        p = (rackId * y + sn) * rackId
