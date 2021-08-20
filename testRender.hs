import RE (render)
import REDef
import TestLib

tests =
    [ "epsilon" ~: render Eps ~?= "e"
    , "0" ~: render (Single '0') ~?= "0"
    , "0|1" ~: render (Or (Single '0') (Single '1')) ~?= "0|1"
    , "01" ~: render (Cat (Single '0') (Single '1')) ~?= "01"
    , "0*" ~: render (Star (Single '0')) ~?= "0*"
    , "0|1|2" ~: render (Or (Single '0') (Or (Single '1') (Single '2')))
              ~?= "0|1|2"
    , "01|10" ~: render (Or (Cat (Single '0') (Single '1'))
                            (Cat (Single '1') (Single '0')))
              ~?= "01|10"
    , "(0|1)(1|0)" ~: render (Cat (Or (Single '0') (Single '1'))
                                  (Or (Single '1') (Single '0')))
              ~?= "(0|1)(1|0)"
    , "012" ~: render (Cat (Single '0') (Cat (Single '1') (Single '2')))
              ~?= "012"
    , "01*" ~: render (Cat (Single '0') (Star (Single '1')))
            ~?= "01*"
    , "(01)*" ~: render (Star (Cat (Single '0') (Single '1')))
              ~?= "(01)*"
    , "0**" ~: render (Star (Star (Single '0'))) ~?= "0**"
    , "0(10)*|1e"
      ~: render (Or (Cat (Single '0') (Star (Cat (Single '1') (Single '0'))))
                    (Cat (Single '1') Eps))
      ~?= "0(10)*|1e"
    ]
-- more tests when marking

main = testlibMain tests
