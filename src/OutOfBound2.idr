module OutOfBound2


isInBounds : (k : Nat) -> (xs : List a) -> Dec (InBounds k xs)
isInBounds k [] = No uninhabited
isInBounds Z (x :: xs) = Yes InFirst
isInBounds (S k) (x :: xs) with (isInBounds k xs)
    isInBounds (S k) (x :: xs) | (Yes prf) = Yes (InLater prf)
    isInBounds (S k) (x :: xs) | (No contra) = No (\p => case p of InLater y => contra y)


getAt : (n : Nat) -> (xs : List a) -> {auto ok : InBounds n xs} -> a
getAt Z     (x :: xs) {ok} = x
getAt (S k) (x :: xs) {ok = InLater p} = getAt k xs


run_test : IO ()
run_test = do
    let l = [1, 2, 3]
    let i = 2
    -- printLn (getAt l i)
    case isInBounds i l of
        (Yes _) => printLn (getAt i l)
        _ => printLn "Out of bound"
