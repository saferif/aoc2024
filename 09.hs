import Data.Char (digitToInt)
import Data.Sequence (Seq((:<|), (:|>), Empty))

data DiskStatus = Free | Full Int deriving Show
dsAsInt Free = 0
dsAsInt (Full x) = x

parse :: String -> Seq (DiskStatus, Int)
parse = parse' 0 True
        where parse' idx False (x:xs) = ((Nothing, digitToInt x) :<| parse' idx True xs)
              parse' idx True (x:xs) = ((Just idx, digitToInt x) :<| parse' (idx + 1) False xs)
              parse' _ _ [] = Empty

compact :: Seq (DiskStatus, Int) -> [(DiskStatus, Int)]
compact Empty = []
compact (file@(Full _, _) :<| xs) = file : compact xs
compact (xs :|> (Free, _)) = compact xs
compact ((Free, freeSize) :<| (xs :|> (Full idx, fullSize)))
  | freeSize < fullSize = (Full idx, freeSize) : compact (xs :|> (Full idx, fullSize - freeSize))
  | freeSize > fullSize = (Full idx, fullSize) : compact ((Free, freeSize - fullSize) :<| xs)
  | freeSize == fullSize = (Full idx, freeSize) : compact xs

asTape :: [(DiskStatus, Int)] -> [DiskStatus]
asTape = (>>= (uncurry $ flip replicate))

checksum :: [DiskStatus] -> Int
checksum = foldl (\acc (i, ds) -> acc + i * (dsAsInt ds)) 0 . zip [0..]

main = interact $ show . checksum . asTape . compact . parse