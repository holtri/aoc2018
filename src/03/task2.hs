import Data.Char
import Data.List.Split
import qualified Data.Map as M

main :: IO ()
main = do
   input <- readFile "src/03/input"
   let parsedInput = map parse $ lines input
       cuts = M.unionsWith (++) $ map getcut parsedInput
       output_t1 = M.size $ M.filter (>1) $ M.map length cuts

       single_cuts = M.filter (\ v -> length v == 1) cuts
       cut_keys = map (M.keys . getcut) parsedInput
       filtered_single_cut = map (containsAll single_cuts) cut_keys
       output_t2 = [y | (x, y:ys) <- zip filtered_single_cut parsedInput, x]
   print output_t1
   print output_t2

containsAll cuts = all (`M.member` cuts)

parse = map (read::String->Integer) . filter (not . null) . splitWhen (not . isDigit)

getcut :: [Integer] -> M.Map (Integer, Integer) [Integer]
getcut [id,l_space,t_space,width,height] = M.fromList [((x,y), [id]) | x <- x_span, y <- y_span]
   where x_span = [l_space+1 .. l_space + width]
         y_span = [t_space + 1 .. t_space + height]
