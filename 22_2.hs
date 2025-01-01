import Data.Bits (xor)
import Data.Maybe (catMaybes)
import Data.Map.Strict (Map, keys, union, empty, insertWith, (!?))

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

totalPrices :: [Map (Int, Int, Int, Int) Int] -> (Int, Int, Int, Int) -> Int
totalPrices ms d =  sum $ catMaybes $ fmap (\m -> m !? d) ms

allFourths :: [Int] -> Map (Int, Int, Int, Int) Int
allFourths = allFourths' empty
    where
        allFourths' m (x1 : x2 : x3 : x4 : x5 : xs) =
            allFourths' (insertWith (flip const) ((x2 - x1), (x3 - x2), (x4 - x3), (x5 - x4)) x5 m) (x2 : x3 : x4 : x5 : xs)
        allFourths' m _ = m

main = do
    text <- getContents
    let secrets = parse text
        lastDigits = fmap (\x -> fmap (\y -> y `mod` 10) $ take 2001 $ iterate evolve x) secrets
        fourths = fmap allFourths lastDigits
        un = foldr union empty fourths
    print $ maximum $ fmap (totalPrices fourths) $ keys un



