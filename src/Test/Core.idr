module Test.Core

import Main


export
run_tests : IO ()
run_tests = do
    print (Main.my_plus 1 2)
