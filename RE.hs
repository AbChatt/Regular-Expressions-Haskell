module RE where

import REDef

render :: R -> String
render r = error "TODO"

-- Helper for conditionally adding parentheses. You supply the condition.
paren :: Bool -> String -> String
paren True xs = "(" ++ xs ++ ")"
paren False xs = xs


match :: R -> String -> [String]
match = error "TODO"

isInRL :: R -> String -> Bool
isInRL r xs = "" `elem` match r xs
