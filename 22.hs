import Data.Bits (xor)

parse :: String -> [Int]
parse s = fmap read $ lines s

mix :: Int -> Int -> Int
mix secret x = secret `xor` x

prune :: Int -> Int
prune secret = secret `mod` 16777216

evolve :: Int -> Int
evolve secret =
    let part1 = prune $ secret `mix` (secret * 64)
        part2 = prune $ part1 `mix` (part1 `div` 32)
        part3 = prune $ part2 `mix` (part2 * 2048)
    in part3

main = do
    text <- getContents
    let secrets = parse text
    print $ sum $ fmap (\s -> iterate evolve s !! 2000) secrets
