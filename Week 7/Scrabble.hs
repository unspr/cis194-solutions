module Scrabble where
import Data.Monoid
import Data.Char

data Score = Score Int
    deriving (Eq, Show)

instance Monoid Score where
    mempty = Score 0
    mappend = (\(Score x) (Score y) -> Score (x + y))

score :: Char -> Score
score c
  | c' `elem` "aeilnorstu" = Score 1
  | c' `elem` "dg"         = Score 2
  | c' `elem` "bcmp"       = Score 3
  | c' `elem` "fhvwy"      = Score 4
  | c' `elem` "k"          = Score 5
  | c' `elem` "jx"         = Score 8
  | c' `elem` "qz"         = Score 10
  | otherwise              = Score 0
    where c' = toLower c


scoreString :: String -> Score
scoreString [] = Score 0
scoreString (x:str) = score x <> scoreString str