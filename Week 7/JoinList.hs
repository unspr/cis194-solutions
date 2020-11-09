{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Data.Maybe
import Scrabble
import Buffer

data JoinList m a = Empty
    | Single m a
    | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) l r = Append (tag l <> tag r) l r

tag :: Monoid m => JoinList m a -> m
tag (Single a _) = a
tag (Append a _ _) = a
tag Empty = mempty

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ n Empty = Nothing
indexJ n jl
    | n + 1 < 1 = Nothing
    | n + 1 > getSize (size (tag jl)) = Nothing
indexJ n (Single _ e) = Just e
indexJ n (Append _ l r)
    | n + 1 <= getSize (size (tag l)) = indexJ n l
    | True = indexJ (n - getSize (size (tag l))) r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n Empty = Empty
dropJ n jl
    | n < 1 = jl
    | n >= getSize (size (tag jl)) = Empty
    
dropJ n x@(Append m l r)
    | n < getSize (size (tag l)) = dropJ n l
    | n == getSize (size (tag l)) = r
    | True = dropJ (n - (getSize (size (tag l)))) r

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ n Empty = Empty
takeJ n jl
    | n == 0 = Empty
    | n >= getSize (size (tag jl)) = jl
takeJ n (Append m l r)
    | n < getSize (size (tag l)) = takeJ n l
    | n == getSize (size (tag l)) = l
    | True = l +++ takeJ (n - (getSize (size (tag l)))) r

scoreLine :: String -> JoinList Score String
scoreLine str =  Single (scoreString str) str

-- res = scoreLine "yay " +++ scoreLine "haskell!"

instance Buffer (JoinList (Score, Size) String) where
    toString jl = case tag jl of
                    (_, n) -> unwords $ map (fromJust .(flip indexJ $jl)) [0..(getSize (size n)-1)]
    fromString str = foldr (+++) Empty (map (\str -> Single (scoreString str, Size 1) str) (lines str))
    line n jl = indexJ n jl
    replaceLine n str jl = takeJ n jl +++ Single (scoreString str, Size 1) str +++ dropJ (n+1) jl
    numLines jl = getSize (size (tag jl))
    value (Append ((Score p), _) _ _) = p
    value (Single ((Score p), _) _) = p