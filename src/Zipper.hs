module Zipper (
      Zipper
    , toList
    , slider
    , slideUp
    , slideDown
    , open
    , exists
    , slideTo
    , modify
    , add
    ) where

import Data.Maybe (maybe)

type Zipper a = ([a], [a])

toList :: Zipper a -> [a]
toList (ls, rs) = reverse rs ++ ls

slider :: Zipper a -> Maybe a
slider (_, []) = Nothing
slider (_, r : _) = Just r

slideUp :: Zipper a -> Maybe (Zipper a)
slideUp ([], _) = Nothing
slideUp (r : ls, rs) = Just (ls, r : rs)

slideDown :: Zipper a -> Maybe (Zipper a)
slideDown (_, []) = Nothing
slideDown (ls, l : rs) = Just (l : ls, rs)

open :: Zipper a -> Zipper a
open (ls, []) = (ls, [])
open zipper = maybe zipper open (slideDown zipper)

exists :: (a -> Bool) -> Zipper a -> Bool
exists predicate zipper = check $ open zipper
    where
        check zipper' = (maybe False predicate $ slider zipper') || (maybe False check $ slideUp zipper')

slideTo :: (a -> Bool) -> Zipper a -> Maybe (Zipper a)
slideTo predicate zipper = slideTo' $ open zipper
    where 
        checkSlider zipper' = maybe False predicate $ slider zipper'
        slideTo' zipper' = 
            if checkSlider zipper'
            then Just zipper'
            else (slideUp zipper') >>= slideTo'

modify :: (a -> a) -> (Zipper a -> Zipper a)
modify _ (ls, []) = (ls, [])
modify f (ls, r : rs) = (ls, f r : rs)

add :: (a -> a -> Bool) -> a -> Zipper a -> Maybe (Zipper a)
add predicate r zipper@(ls, rs) =
    if exists (predicate r) zipper
    then Nothing
    else Just $ (ls, r : rs)