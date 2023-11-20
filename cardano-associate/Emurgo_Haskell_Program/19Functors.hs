module Functors where

--  f (DType lst) = map f lst

-- instance Functor BloodType where
-- fmap :: (a -> b) -> f a -> f b
-- map  :: (a -> b) -> [a] -> [b]
--                   [] a -> [] b

maybeMap :: (a -> b) -> Maybe a -> Maybe b
maybeMap _ Nothing = Nothing
maybeMap f (Just x) = Just (f x)

data RH = Positive | Negative deriving (Show,Eq)
data Bloodgroup = A | AB | B | O deriving (Show,Eq)
data BloodType = BType Bloodgroup RH deriving (Show,Eq)
data Blood a = Blood a deriving (Show,Eq)

x :: Blood BloodType
x = Blood (BType A Positive)

y :: Blood Bloodgroup
y = Blood A

z = Blood Positive

instance Functor Blood where
fmap f (Blood a) = Blood (f a)

onlyBG :: BloodType -> Bloodgroup
onlyBG (BType bg _) = bg

onlyRH :: BloodType -> RH
onlyRH (BType _ rh) = rh

-- fmap :: (a -> b) -> f a -> f b
-- fmap (BloodType -> Bloodgroup) -> Blood BloodType -> Blood Bloodgroup

-- patient1 :: DonatorType
-- patient1 = DType (BType A Positive) (BType O Positive)

-- patient2 :: DonatorType
-- patient2 = DType (BType B Positive) (BType O Positive)
--      --  = (True,True)

-- instance Functor BloodType where
  -- fmap :: ( a -> b) -> f a -> f b 
  -- fmap f dt = 
