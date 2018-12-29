
data Tree = Tree {metadata :: [Integer], childs :: [Tree]} deriving (Show)

main :: IO ()
main = do
   input <- readFile "src/08/input"
   let parsedInput =  map (read::String->Integer) $ words input
       tree = buildTree parsedInput
   print ("Task 1: " ++ show (sumMetadata tree))
   print ("Task 2: " ++ show (value tree))

buildTree :: [Integer] -> Tree
buildTree (nc:nm:xs) = root
   where (root, _ ) = parseNode [] nc nm xs

parseNode :: [Tree] -> Integer -> Integer -> [Integer] -> (Tree, [Integer])
parseNode childs 0 n_meta xs = (Tree{metadata=metadata, childs=reverse childs}, remaining)
   where (metadata, remaining) = splitAt (fromIntegral n_meta) xs
parseNode childs n_childs n_meta (nc:nm:xs) =
   let (child, remaining) = parseNode [] nc nm xs
   in parseNode (child:childs) (n_childs - 1) n_meta remaining

sumMetadata :: Tree -> Integer
sumMetadata tree = sum (metadata tree) + sum (map sumMetadata (childs tree))

value :: Tree -> Integer
value (Tree metadata []) = sum metadata
value (Tree metadata childs) =
   let validChilds = filter (\x -> x/=0 && x <= fromIntegral (length childs)) metadata
   in sum $ map (\i -> value (childs !! fromIntegral (i-1))) validChilds
