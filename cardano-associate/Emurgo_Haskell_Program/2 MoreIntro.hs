 module IntroTwo where
 -- new changes on windows
 
 -- new changes on linux2333

 --Factorial is !5 = 1 * 2 * 3 * 4 * 5

 factorial :: Integer -> Integer
 factorial 0 = 1 
 factorial n = n * factorial (n-1)  

 -- Guards, Case, If-then-else, Patter matching

 --Guards
 factorialG :: Integer -> Integer
 factorialG n
  | n == 0      = 1
  | otherwise   = n * factorialG (n-1)

 --case

 factorialC :: Integer -> Integer
 factorialC n = case (n > 0) of
     False     -> 1
     True      -> n * factorialC (n-1)

 factorialI :: Integer -> Integer
 factorialI n = if n <= 0 then 1 else n * factorial (n-1)

 safeFactorial :: Integer -> Integer
 safeFactorial 0 = 1
 safeFactorial n = if n < 0 then (-1) else n * safeFactorial (n-1)

 safeFactorialG :: Integer -> Integer
 safeFactorialG 0 = 1
 safeFactorialG n 
  | n < 0     = (-1)
  | otherwise = n * safeFactorialG (n-1)

 safeFactorialC :: Integer -> Integer 
 safeFactorialC 0 = 1
 safeFactorialC n = case (n < 0) of
                        True       -> (-1)
                        False      -> n * safeFactorialC (n-1)
   
