module Temperature (tempToC, tempToF) where

{- Implement the function `tempToC` to convert
`  Fahrenheit to Celsius                    -}

--T(°C) = (T(°F) - 32) / 1.8.
tempToC :: Integer -> Float
tempToC temp = (temp - 32) / 1.8


{- Implement the function `tempToF` to convert
`  Celsius to Fahrenheit                    -}
--T(°F) = T(°C) × 1.8 + 32.
tempToF :: Float -> Integer
tempToF temp = (+32) $ (*) temp 1.8
