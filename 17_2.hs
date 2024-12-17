import Control.Monad (void)
import Data.Bits (shift, xor, (.&.), (.|.))
import Text.Parsec.String (Parser)
import Text.Parsec (runParser, string, digit, many1, sepBy1, char)

parseInput :: Parser (Int, Int, Int, [Int])
parseInput = do
    void $ string "Register A: "
    a <- many1 digit
    void $ char '\n'
    void $ string "Register B: "
    b <- many1 digit
    void $ char '\n'
    void $ string "Register C: "
    c <- many1 digit
    void $ char '\n'
    void $ char '\n'
    void $ string "Program: "
    prog <- sepBy1 (many1 digit) (char ',')
    return (read a, read b, read c, fmap read prog)

combo :: Int -> Int -> Int -> Int -> Int
combo a b c x
    | x >= 0 && x <= 3 = x
    | x == 4 = a
    | x == 5 = b
    | x == 6 = c

run :: [Int] -> [Int] -> Int -> Int -> Int -> [Int]
run orig [] a b c = []
run orig (0:arg:rest) a b c = run orig rest (shift a (negate $ combo a b c arg)) b c
run orig (1:arg:rest) a b c = run orig rest a (xor b arg) c
run orig (2:arg:rest) a b c = run orig rest a ((combo a b c arg) .&. 7) c
run orig (3:arg:rest) a b c = if a == 0 then (run orig rest a b c) else (run orig (drop arg orig) a b c)
run orig (4:_:rest) a b c = run orig rest a (xor b c) c
run orig (5:arg:rest) a b c = ((combo a b c arg) .&. 7) : (run orig rest a b c)
run orig (6:arg:rest) a b c = run orig rest a (shift a (negate $ combo a b c arg)) c
run orig (7:arg:rest) a b c = run orig rest a b (shift a (negate $ combo a b c arg))

search :: [Int] -> Int -> (Int, Int) -> [Int] -> [Int]
search target idx (needBits, readyBits) candidates =
    let check = (\x -> (take (idx + 1) (run target target x 0 0)) == (take (idx + 1) target))
        slam = (\old new -> old .|. (shift new readyBits))
        newCandidates = filter check [slam old new | old <- candidates, new <- [0..(shift 1 needBits)]]
    in if idx == (length target)
       then candidates
       else search target (idx + 1) (3, readyBits + needBits) newCandidates

main = do
    text <- getContents
    let (Right (_, _, _, prog)) = runParser parseInput () "" text
    print $ minimum $ search prog 0 (10, 0) [0]
