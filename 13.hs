import Data.Maybe (catMaybes)
import Data.Ratio (Rational, (%), numerator, denominator)
import Text.Regex.Posix ((=~))

parse :: String -> [((Integer, Integer), (Integer, Integer), (Integer, Integer))]
parse = fmap (parse' . (fmap read) . tail) . (=~ "Button A: X\\+([0-9]+), Y\\+([0-9]+)\nButton B: X\\+([0-9]+), Y\\+([0-9]+)\nPrize: X=([0-9]+), Y=([0-9]+)")
  where parse' [ax, ay, bx, by, px, py] = ((ax, ay), (bx, by), (px, py))

inv :: (Rational, Rational, Rational, Rational) -> (Rational, Rational, Rational, Rational)
inv (a, b, c, d) = let dt = a * d - b * c in (d / dt, (-b) / dt, (-c) / dt, a / dt)

mul :: (Rational, Rational, Rational, Rational) -> (Rational, Rational) -> (Rational, Rational)
mul (a, b, c, d) (x, y) = (a * x + b * y, c * x + d * y)

solve :: ((Integer, Integer), (Integer, Integer), (Integer, Integer)) -> Maybe Integer
solve ((a1, a2), (b1, b2), (c1, c2)) = let a = (a1 % 1, b1 % 1, a2 % 1, b2 % 1)
                                           c = (c1 % 1, c2 % 1)
                                           a' = inv a
                                           (x, y) = mul a' c
                                       in if denominator x == 1 && denominator y == 1
                                          then Just $ numerator (x * 3 + y)
                                          else Nothing

main = interact $ show . sum . catMaybes . fmap solve . parse