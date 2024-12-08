import Data.List (isSuffixOf)

parse :: String -> (Int, [Int])
parse l = let (testValue, _:rest) = break (==':') l
              values = words rest
          in (read testValue, reverse $ fmap read values)

isValid :: (Int, [Int]) -> Bool
isValid (target, [x]) = target == x
isValid (target, (x:xs)) = ((target - x > 0) && isValid (target - x, xs))
                           || ((target `mod` x == 0) && isValid (target `div` x, xs))
                           || ((isSuffixOf (show x) (show target)) && isValid (target `div` (10^(length (show x))), xs))

main = do
    text <- getContents
    let equations = fmap parse (lines text)
    let filtered = filter isValid equations
    print $ sum (fmap fst filtered)
