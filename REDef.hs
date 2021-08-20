module REDef where

data R = Eps | Single Char | Cat R R | Or R R | Star R
    deriving (Eq, Show)
