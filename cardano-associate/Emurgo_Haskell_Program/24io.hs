getInteger :: IO Int
getInteger = getLine >>= \s -> return (read s)

add :: IO ()
add =
  getInteger >>= \a ->
  getInteger >>= \b ->
  putStrLn (show (a + b))

greet :: IO ()
greet = 
  getLine >>= \s ->
  getLine >>= \t ->
  putStrLn ("hello" ++ s ++ " and " ++ t ++ "!")

main :: IO ()
main = add
