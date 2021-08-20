module RE where

import REDef
import Data.List

render :: R -> String
render (Or r1 r2) = (renderHelper O r1) ++ "|" ++ (renderHelper O r2)
render (Cat r1 r2) = (renderHelper C r1) ++ (renderHelper C r2)
render (Star r1) = (renderHelper S r1) ++ "*"
render (Single r1) = r1 : []
render Eps = "e"

data InfoFromParent = C | O | S

renderHelper :: InfoFromParent -> R -> String
renderHelper _ Eps = paren False "e"
renderHelper _ (Single r1) = paren False (r1 : [])
renderHelper O (Or r1 r2) = paren False ((renderHelper O r1) ++ "|" ++ (renderHelper O r2))
renderHelper O (Cat r1 r2) = paren False ((renderHelper C r1) ++ (renderHelper C r2))
renderHelper O (Star r1) = paren False ((renderHelper S r1) ++ "*")
renderHelper C (Or r1 r2) = paren True ((renderHelper O r1) ++ "|" ++ (renderHelper O r2))
renderHelper C (Cat r1 r2) = paren False ((renderHelper C r1) ++ (renderHelper C r2))
renderHelper C (Star r1) = paren False ((renderHelper S r1) ++ "*")
renderHelper S (Or r1 r2) = paren True ((renderHelper O r1) ++ "|" ++ (renderHelper O r2))
renderHelper S (Cat r1 r2) = paren True ((renderHelper C r1) ++ (renderHelper C r2))
renderHelper S (Star r1) = paren False ((renderHelper S r1) ++ "*")

-- Helper for conditionally adding parentheses. You supply the condition.
paren :: Bool -> String -> String
paren True xs = "(" ++ xs ++ ")"
paren False xs = xs


match :: R -> String -> [String]
match r "" = [""]
match Eps xs = [xs]
match (Or a b) xs = (match a xs) ++ (match b xs)
match (Cat a b) xs = second
  where 
      leftovers = match a xs
      second = matchAndConcat b leftovers
match (Single a) (x:xs)
    | isPrefixOf (a : []) (x:xs) = ((x:xs) \\ (a : [])) : []
    | otherwise = []
match (Star a) xs = match Eps xs ++ matchStarAndConcat a xs


matchAndConcat :: R -> [String] -> [String]
matchAndConcat r [] = []
matchAndConcat r [""] = [""]
matchAndConcat r (x:xt) = (match r x) ++ (matchAndConcat r xt)

matchStarAndConcat :: R -> String -> [String]
matchStarAndConcat r xs
    | match r xs /= [] = (match r xs) ++ (matchStarAndConcat (Cat r r) xs)
    | otherwise = []


isInRL :: R -> String -> Bool
isInRL r xs = "" `elem` match r xs  -- is "" an element in match r xs?
