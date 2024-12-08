import Data.Ix (range, inRange)

evaluate :: [(Int, Int)] -> Bool
evaluate l = let d  = all ((inRange (1, 3)) . abs . (uncurry (-))) l
                 ai = all (uncurry (<)) l
                 ad = all (uncurry (>)) l
             in d && (ai || ad)
               
isSafe :: [Int] -> Bool
isSafe l = evaluate (zip l (tail l))

generateSkips :: [Int] -> [[Int]]
generateSkips l = fmap skip (range (0, length l)) where
    skip i = let (l1, l2) = splitAt i l in (l1 ++ (drop 1 l2))

isSafe' :: [Int] -> Bool
isSafe' = (any isSafe) . generateSkips

main = interact $ show . length . (filter (isSafe' . fmap read . words)) . lines
