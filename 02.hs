import Data.Ix (inRange)

evaluate :: [(Int, Int)] -> Bool
evaluate l = let d  = all ((inRange (1, 3)) . abs . (uncurry (-))) l
                 ai = all (uncurry (<)) l
                 ad = all (uncurry (>)) l
             in d && (ai || ad)

isSafe :: [Int] -> Bool
isSafe l = evaluate (zip l (tail l))

main = interact $ show . length . (filter (isSafe . fmap read . words)) . lines
