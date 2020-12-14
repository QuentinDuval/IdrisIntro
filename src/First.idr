module First


getHead : List a -> Maybe a
getHead [] = Nothing
getHead (x :: xs) = Just x


run_test : IO ()
run_test = do
    let l = [1, 2, 3]
    let h = getHead l
    -- printLn (h * 2) -- TODO: uncomment to show the compilation error
    case h of 
        Nothing => printLn "Empty List"
        Just v => printLn (v * 2)

