module OtherMonads where
import Data.Char


shout :: IO String
shout = do
         xs <- getLine
         return (map toUpper xs ++ "!")


shout' :: IO String
shout' = getLine >>= \xs -> return (map toUpper xs ++ "!")
