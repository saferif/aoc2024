import Data.Char (digitToInt)
import Data.Sequence (Seq((:<|), (:|>), Empty), breakl, (><))

data DiskStatus = Free | Full Int deriving Eq
dsAsInt Free = 0
dsAsInt (Full x) = x

parse :: String -> Seq (DiskStatus, Int)
parse = parse' 0 True
        where parse' idx False (x:xs) = ((Nothing, digitToInt x) :<| parse' idx True xs)
              parse' idx True (x:xs) = ((Just idx, digitToInt x) :<| parse' (idx + 1) False xs)
              parse' _ _ [] = Empty

compact :: [(DiskStatus, Int)] -> Seq (DiskStatus, Int) -> [(DiskStatus, Int)]
compact suffix Empty = suffix
compact suffix (xs :|> space@(Free, _)) = compact (space : suffix) xs
compact suffix (xs :|> file@(Full _, size)) = case tryFit xs file of
                                                Nothing -> compact (file:suffix) xs
                                                Just new -> compact ((Free, size):suffix) new

tryFit :: Seq (DiskStatus, Int) -> (DiskStatus, Int) -> Maybe (Seq (DiskStatus, Int))
tryFit disk file@(Full _, size) = case breakl (\(st, sz) -> sz >= size && st == Free) disk of
                                    (_, Empty) -> Nothing
                                    (prefix, ((Free, freeSize) :<| rest)) -> Just (prefix >< (file :<| (Free, freeSize - size) :<| rest))

asTape :: [(DiskStatus, Int)] -> [DiskStatus]
asTape = (>>= (uncurry $ flip replicate))

checksum :: [DiskStatus] -> Int
checksum = foldl (\acc (i, ds) -> acc + i * (dsAsInt ds)) 0 . zip [0..]

main = interact $ show . checksum . asTape . (compact []) . parse
