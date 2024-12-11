import Data.IntMap (IntMap, insertWith, empty, assocs, elems)

changeStone :: Int -> [Int]
changeStone x = let s = show x
                    l = length s
                in if odd l then [if x == 0 then 1 else x * 2024]
                            else let (a, b) = splitAt (l `div` 2) s 
                                 in [read a, read b]

parse :: String -> IntMap Int
parse = (foldl (\m x -> insertWith (+) x 1 m) empty) . (fmap read) . words

evolve :: IntMap Int -> IntMap Int
evolve m = foldl (\m' (stone, count) -> foldl (\m'' newStone -> insertWith (+) newStone count m'') m' (changeStone stone)) empty (assocs m)

main = interact $ show . sum . elems . (!! 75) . (iterate evolve) . parse