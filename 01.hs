import Data.List

word1 :: String -> (String, String)
word1 = proc [] where
    proc acc (x:rest) = if (x == ' ') then (acc, rest) else proc (acc++[x]) rest
    
readTwo :: (String, String) -> (Int, Int)
readTwo (a, b) = (read a, read b)

parseLine :: String -> (Int, Int)
parseLine = readTwo . word1

separate :: [(Int, Int)] -> ([Int], [Int])
separate = proc [] [] where
    proc left right ((a, b):rest) = proc (a:left) (b:right) rest
    proc left right [] = (sort left, sort right)

distances :: ([Int], [Int]) -> [Int]
distances = proc [] where 
    proc acc ((a:left), (b:right)) = proc (acc++[abs (a-b)]) (left, right)
    proc acc _ = acc

main = interact $ show . sum . distances . separate . fmap parseLine . lines
