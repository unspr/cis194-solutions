module JoinList where

import Data.Monoid
import Sized
import Data.Maybe
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
indexJ idx Empty = Nothing
indexJ idx (Single m e) = if Size idx == size m then Just e else Nothing
indexJ idx (Append m l r) = let x = indexJ idx l in
            case x of
                (Just _) -> x
                _ -> indexJ idx r

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ n Empty = Empty
dropJ n jl
    | n == 0 = jl
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

