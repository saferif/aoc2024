parse :: String -> (Int, [Int])
parse l = let (testValue, _:rest) = break (==':') l
              values = words rest
          in (read testValue, fmap read values)
          
solutions :: [Int] -> Int -> [Int]
solutions [] acc = [acc]
solutions (x:xs) acc = let r1 = solutions xs (acc + x)
                           r2 = solutions xs (acc * x)
                       in (r1 ++ r2)

main = do
    text <- getContents
    let equations = fmap parse (lines text)
    let filtered = filter (\(v, (x:n)) -> any (==v) (solutions n x)) equations
    print $ sum (fmap fst filtered)
