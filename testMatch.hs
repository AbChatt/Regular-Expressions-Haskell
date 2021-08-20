import           Data.Set (Set)
import qualified Data.Set as Set

import           REDef
import           RE (match)
import           TestLib

eqSet :: [String] -> [String] -> Test
eqSet = checkRel MkRel{rel = e, relName = "equal up to order and dups"}
  where
    e xs ys = Set.fromList xs == Set.fromList ys
    -- So go through binary seach trees to compare.
infix 5 `eqSet`

tests =
    [ "Eps" ~: match Eps "0101" `eqSet` ["0101"]
    , "Single match A" ~: match (Single '0') "0011" `eqSet` ["011"]
    , "Single nomatch A" ~: match (Single '1') "0011" `eqSet` []
    , "01" ~: match (Cat (Single '0') (Single '1')) "01ab" `eqSet` ["ab"]
    , "0|01" ~: match (Or (Single '0') (Cat (Single '0') (Single '1'))) "01ab"
      `eqSet` ["1ab", "ab"]
    , "0*" ~: match (Star (Single '0')) "0011" `eqSet` ["0011", "011", "11"]
    , "0*|0" ~:
      match (Or (Star (Single '0')) (Single '0')) "0011"
      `eqSet` ["0011", "011", "11", "011"]
    , "0*0" ~: match (Cat (Star (Single '0')) (Single '0')) "0011" `eqSet` ["011", "11"]
    ]
-- more tests when marking

main = testlibMain tests
