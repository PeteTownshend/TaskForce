module Zipper (
    Zipper(..),
    toList,
    slider,
    slideUp,
    slideDown,
    open,
    exists,
    prune,
    slideTo,
    modify,
    add
    ) where

import Data.Maybe (maybe)

data Zipper a
    = Empty
    | Zipper [a] a [a]
    deriving (Eq, Show, Read)

instance Functor Zipper where
    fmap _ Empty            = Empty
    fmap f (Zipper ds x us) = Zipper (fmap f ds) (f x) (fmap f us)
    
slideUp :: Zipper a -> Zipper a
slideUp Empty                = Empty
slideUp (Zipper [] x us)     = Zipper [] x us
slideUp (Zipper (x:ds) u us) = Zipper ds x (u:us)

slideDown :: Zipper a -> Zipper a
slideDown Empty                = Empty
slideDown (Zipper ds x [])     = Zipper ds x []
slideDown (Zipper ds d (x:us)) = Zipper (d:ds) x us

slider :: Zipper a -> Maybe a
slider Empty          = Nothing
slider (Zipper _ x _) = Just x

open :: Zipper a -> Zipper a
open Empty            = Empty
open (Zipper [] x us) = Zipper [] x us
open zipper           = open $ slideUp zipper

toList :: Zipper a -> [a]
toList Empty            = []
toList (Zipper ds x us) =  x:us ++ ds

exists :: (a -> Bool) -> Zipper a -> Bool
exists _ Empty            = False
exists p (Zipper ds x us) = p x || any p us || any p ds

prune :: (a -> Bool) -> Zipper a -> Zipper a
prune _ Empty = Empty
prune p (Zipper ds x us) = 
    if p x 
    then Zipper ds' x us'
    else case (ds', us') of
        ([], [])   -> Empty
        (x:ls, []) -> Zipper ls x []
        (ls, x:rs) -> Zipper ls x rs
    where
        ds' = filter p ds
        us' = filter p us

slideTo :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
slideTo p = slideTo' . open
    where
        checkSlider = maybe False p . slider
        slideTo' Empty  = Nothing
        slideTo' (Zipper ds x [])
            | p x       = Just $ Zipper ds x []
            | otherwise = Nothing
        slideTo' zipper =
            if   checkSlider zipper
            then Just zipper
            else slideTo' $ slideDown zipper

modify :: (a -> a) -> (Zipper a -> Zipper a)
modify _ Empty            = Empty
modify f (Zipper ds x us) = Zipper ds (f x) us

add :: (a -> a -> Bool) -> a -> Zipper a -> Zipper a
add _ x Empty = Zipper [] x []
add p x zipper@(Zipper ds u us) =
    if   exists (p x) zipper
    then zipper
    else Zipper ds x (u:us)