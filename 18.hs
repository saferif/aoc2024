import Data.Map.Strict (Map, fromList, member, insert, (!))
import Data.Sequence (Seq(Empty, (:<|)), (><), singleton)

parsePair :: String -> (Int, Int)
parsePair s = let (x, _:y) = break (==',') s in (read y, read x)

maxc = 70
bytesCnt = 1024

bfs :: Map (Int, Int) Int -> Seq ((Int, Int), Int) -> Map (Int, Int) Int
bfs dists Empty = dists
bfs dists ((pos@(r, c), dist) :<| queue)
    | r < 0 || r > maxc || c < 0 || c > maxc || (member pos dists) = bfs dists queue
    | otherwise =
        let dists' = insert pos dist dists
            next = queue >< (((r - 1, c), dist + 1) :<| ((r + 1, c), dist + 1) :<| ((r, c - 1), dist + 1) :<| ((r, c + 1), dist + 1) :<| Empty)
        in bfs dists' next

main = do
    text <- getContents
    let fallen = take bytesCnt $ lines text
        dists = fromList $ fmap (\line -> (parsePair line, -1)) fallen
        dists' = bfs dists $ singleton ((0, 0), 0)
    print $ dists' ! (maxc, maxc)