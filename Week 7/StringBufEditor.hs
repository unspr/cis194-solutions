module Main where

import StringBuffer
import Editor
import JoinList
import Buffer
import Scrabble
import Sized

text = unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]
buff = Buffer.fromString text :: JoinList (Score, Size) String

main = runEditor editor $ buff
