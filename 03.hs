import Text.Regex.Posix

pairs :: String -> [(Int, Int)]
pairs s = fmap (pair . fmap read . tail) matches where
    matches = s =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]
    pair [x, y] = (x, y)

main = interact $ show . sum . (fmap (uncurry (*))) . pairs
