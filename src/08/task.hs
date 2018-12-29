
data Tree = Tree {metadata :: [Integer], childs :: [Tree]} deriving (Show)

main :: IO ()
main = do
   input <- readFile "src/08/input"
   let parsedInput =  map (read::String->Integer) $ words input
   print ("Task 1: " ++ show (sumMetadata $ buildTree parsedInput))

tmp = [2, 3, 0, 3, 10, 11, 12, 1, 1, 0, 1, 99, 2, 1, 1, 2]

buildTree :: [Integer] -> Tree
buildTree (nc:nm:xs) = root
   where (root, _ ) = parseNode [] nc nm xs

parseNode :: [Tree] -> Integer -> Integer -> [Integer] -> (Tree, [Integer])
parseNode childs 0 n_meta xs = (Tree{metadata=metadata, childs=childs}, remaining)
   where (metadata, remaining) = splitAt (fromIntegral n_meta) xs
parseNode childs n_childs n_meta (nc:nm:xs) =
   let (child, remaining) = parseNode [] nc nm xs
   in parseNode (child:childs) (n_childs - 1) n_meta remaining

sumMetadata :: Tree -> Integer
sumMetadata tree = sum (metadata tree) + sum (map sumMetadata (childs tree))
