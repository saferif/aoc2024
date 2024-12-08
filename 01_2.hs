import Data.IntMap (IntMap, empty, insertWith, findWithDefault)

word1 :: String -> (String, String)
word1 = proc [] where
    proc acc (x:xs) = if (x == ' ') then (acc, xs) else proc (acc++[x]) xs
    
readTwo :: (String, String) -> (Int, Int)
readTwo (a, b) = (read a, read b)

parseLine :: String -> (Int, Int)
parseLine = readTwo . word1

separate :: [(Int, Int)] -> ([Int], [Int])
separate = proc [] [] where
    proc left right ((a, b):rest) = proc (a:left) (b:right) rest
    proc left right [] = (left, right)
    
count :: [Int] -> IntMap Int
count = proc empty where
    proc acc [] = acc
    proc acc (x:xs) = proc (insertWith (+) x 1 acc) xs

distances :: ([Int], [Int]) -> [Int]
distances (left, right) = proc [] (count right) left where 
    proc acc counts (x:xs) = proc ((x*(findWithDefault 0 x counts)):acc) counts xs
    proc acc counts _ = acc

main = interact $ show . sum . distances . separate . fmap parseLine . lines
