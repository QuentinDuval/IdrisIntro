module OutOfBound1


data Optional a = None | Value a


getAt : (n : Nat) -> (xs : List a) -> Optional a
getAt Z     (x :: xs) = Value x
getAt (S k) [] = None
getAt (S k) (x :: xs) = getAt k xs


run_test : IO ()
run_test = do
    let l = [1, 2, 3]
    let i = 2
    case getAt i l of
        None => printLn "No value"
        Value x => printLn x
