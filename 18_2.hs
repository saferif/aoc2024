import Data.Set (Set, fromList, member, insert, empty, union, elems)

parsePair :: String -> (Int, Int)
parsePair s = let (x, _:y) = break (==',') s in (read y, read x)

maxc = 70
leftWall = fromList $ fmap (\i -> (i, 0)) [1..(maxc + 1)]
topWall = fromList $ fmap (\i -> (0, i)) [1..(maxc + 1)]
rightWall = fromList $ fmap (\i -> (i, maxc)) [0..maxc]
bottomWall = fromList $ fmap (\i -> (maxc, i)) [0..maxc]

dfs :: Set (Int, Int) -> Set (Int, Int) -> Set (Int, Int) -> [(Int, Int)] -> Bool
dfs _ _ _ [] = False
dfs path finishes seen (pos@(r, c) : queue)
    | r < 0 || r > maxc || c < 0 || c > maxc || (member pos seen) || (not (member pos path)) = dfs path finishes seen queue
    | member pos finishes = True
    | otherwise = 
        let seen' = insert pos seen
            next = ((r - 1, c) : (r + 1, c) : (r, c - 1) : (r, c + 1) : (r - 1, c - 1) : (r - 1, c + 1) : (r + 1, c - 1) : (r + 1, c + 1) : queue)
        in dfs path finishes seen' next

simulate :: [(Int, Int)] -> Int -> Bool
simulate bytes cnt =
    let bytes' = fromList $ take cnt bytes
        fromLeft = dfs bytes' (union topWall rightWall) empty (elems leftWall)
        fromTop = dfs bytes' (union leftWall bottomWall) empty (elems topWall)
    in fromLeft || fromTop

binarySearchD :: Int -> Int -> (Int -> Bool) -> Int
binarySearchD lo hi p
  | lo == hi = lo
  | p mid     = binarySearchD lo mid p
  | otherwise = binarySearchD (mid + 1) hi p
  where
    mid = (lo + hi) `div` 2

main = do
    text <- getContents
    let bytes = fmap parsePair $ lines text
        idx = binarySearchD 1 (length bytes) (simulate bytes)
        (r, c) = bytes !! (idx - 1)
    putStr $ (show c) ++ "," ++ (show r)