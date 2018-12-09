import Data.Time
import Data.List (sortBy)
import Data.Function (on)

main :: IO ()
main = do
   input <- readFile "src/04/input"
   let tmp = sortBy (compare `on` fst) $ map parselog $ lines input
   print (map fst $ take 3 tmp)

parsetime :: String -> UTCTime
parsetime  = parseTimeOrError True defaultTimeLocale "[%Y-%m-%d %H:%M]"

parselog x = (parsetime $ fst logmsg, snd logmsg)
  where logmsg = splitAt 19 x


a = "[1518-05-31 00:27] falls asleep"
b = "[1518-04-15 00:54] wakes up"
c = "[1518-04-06 00:42] falls asleep"
x = [a,b,c]
