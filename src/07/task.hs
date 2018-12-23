import Data.List (sortBy, sort, nub, partition)
import Data.Function (on)
import Debug.Trace

main :: IO ()
main = do
  input <- readFile "src/07/input"
  let parsedInput = map parse $ lines input
  let u = topsort parsedInput [] $ nodes parsedInput
  print ("Task 01: " ++ u)

parse x = (head $ i !! 1, head $ i !! 7)
   where i = words x

inDegree xs e = length $ filter ((==e) . snd) xs

nodes xs = nub $ concat [[a,b] | (a,b) <- xs]

topsort :: [(Char,Char)] -> String -> String -> String
topsort [] s nodes = s
topsort xs [] nodes = topsort xs (filter (\a -> inDegree xs a == 0) nodes) nodes
topsort xs stack@(s:ss) nodes =
  let remaining_edges = [x | x <- xs, fst x /= s] -- remove outgoing edges s -> ? from xs
      remaining_nodes =  [x | x <- nodes, x `notElem` stack] -- only consider nodes that are not already on the stack
      freenodes = filter (\a -> inDegree remaining_edges a == 0) remaining_nodes  -- put nodes with indegree 0 on stack
  in s : topsort remaining_edges (sort $ freenodes ++ ss) remaining_nodes -- sort available nodes on the stack alphabetically

-- tmp= "Step C must be finished before step A can begin.\n\
-- \Step C must be finished before step F can begin.\n\
-- \Step A must be finished before step B can begin.\n\
-- \Step A must be finished before step D can begin.\n\
-- \Step B must be finished before step E can begin.\n\
-- \Step D must be finished before step E can begin.\n\
-- \Step F must be finished before step E can begin."
