module Monoids where
import Data.Char


-- Parameters... Custom config elements on a config file.


--Chat bot
--Parameters of the filters for the spam filter
--We want to avoid emoji, kanjis, nothing that is not a letter or number and a few symbols.

-- [ifEmoji,ifKanji, ifLower,ifDigit
filters :: [(Char->Bool)] 
filters = [ifEmoji, ifKanji, ifLower, ifDigit] 

ifEmoji :: Char -> Bool
ifEmoji c = False

ifKanji :: Char -> Bool 
ifKanji c = False

ifLower :: Char -> Bool
ifLower c = isLower c

ifDigit :: Char -> Bool
ifDigit c = isDigit c

isForbidden :: Char -> Bool
isForbidden c = ifEmoji c || ifKanji c || ifLower c  -- any additional parameter will require code updating
    --  
mForbidden :: Char-> [(Char -> Bool)] -> Bool
mForbidden c filters =  error "to implement"
    --  predicates this is the list of parameters, that can change
    -- filters [ifEmoji,ifKanji, ifLower,ifDigit]
    
newtype Any = Any {getAny :: Bool} deriving (Show)

instance Semigroup Any where
  (<>) x y        = x

instance Monoid Any where
  -- mempty :: as
  mempty          = Any False
  -- mappend :: a -> a -> as
  mappend (c) (d) = Any $ (getAny c) || (getAny d)

newtype All = All {getAll :: Bool} deriving Show

instance Semigroup All where
  (<>) x y        = x

instance Monoid All where
  -- mempty :: as
  mempty          = All True 
  -- mappend :: a -> a -> as
  mappend (c) (d) = All $ (getAll c) && (getAll d)

fnc :: [(Int -> Int)] -> [Int] -> [Int]
fnc xs ys = foldr (\x g -> map x ys ++ g) [] xs

fnc' fList nbList = [funct nb | funct <- fList, nb <- nbList]