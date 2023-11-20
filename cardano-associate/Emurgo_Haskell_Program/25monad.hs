import Prelude hiding ((>>=), return)

data Box a = Box { unBox :: a } deriving Show

return :: a -> Box a
return = Box

(>>=) :: Box a -> (a -> Box b) -> Box b
(Box a) >>= k = k a

x = Box "one"

y = Box "two" 

greet =
  x >>= \a ->
  y >>= \b ->
  return ("hello, " ++ a ++ " and " ++ b ++ "!")
