module MyData.Quant where

data Quant a b =
    Finance
    | Desk a
    | Bloor b
    deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk x) = Desk x
    fmap f (Bloor x) = Bloor (f x)
