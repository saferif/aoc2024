changeStone :: Int -> [Int]
changeStone x = let s = show x
                    l = length s
                in if odd l then [if x == 0 then 1 else x * 2024]
                            else let (a, b) = splitAt (l `div` 2) s 
                                 in [read a, read b]

main = interact $ show . length . (!! 25) . (iterate (>>= changeStone)) . (fmap read . words)
