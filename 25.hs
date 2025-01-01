split :: (Eq a) => a -> [a] -> [[a]]
split delim line = case break (==delim) line of
                    (x, []) -> [x]
                    (x, _:y) -> x : split delim y

isLock :: [String] -> Bool
isLock (('#' : _) : _) = True
isLock _ = False

fits :: [String] -> [String] -> Bool
fits [] [] = True
fits (('#' : _) : _) (('#' : _) : _) = False
fits ([] : xs) ([] : ys) = fits xs ys
fits ((_ : xxs) : xs) ((_ : yys) : ys) = fits (xxs : xs) (yys : ys)

parse :: String -> ([[String]], [[String]])
parse s = foldl (\(keys, locks) s -> if isLock s then (keys, s : locks) else (s : keys, locks)) ([], []) $ split [] $ lines s

main = do
    text <- getContents
    let (keys, locks) = parse text
    print $ length [() | k <- keys, l <- locks, fits k l]