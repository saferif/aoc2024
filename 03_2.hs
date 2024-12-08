import Text.Regex.Posix

eval :: [String] -> (Int -> Int, Int)
eval [op, arg1, arg2] = case op of 
                            ('m':_) -> (id, (read arg1) * (read arg2))
                            ('d':'o':'(':_) -> (const 1, 0)
                            ('d':'o':'n':_) -> (const 0, 0)

pairs :: String -> [[String]]
pairs s = s =~ "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)"

process :: (Int, Int) -> [String] -> (Int, Int)
process (flag, ans) l = let (f, p) = eval l in (f flag, ans + p * flag)

main = interact $ show . snd . (foldl process (1, 0)) . pairs
