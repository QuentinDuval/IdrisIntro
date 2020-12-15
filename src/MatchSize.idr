module MatchSize

import Data.Vect as Vect


data Matrix: (height: Nat) -> (width: Nat) -> Type where
    MakeMatrix: (content: Vect height (Vect width Double)) -> Matrix height width


-- TODO: try with proof of match of size instead (when you get two matrices at runtime)

matmul : Matrix m k -> Matrix k n -> Matrix m n
matmul {m} {n} {k} m1 m2 = ?def_matmul
    {-
    Vect.fromList [col_ i | i <- [0..m-1]] where
        col_ i = Vect.fromList [cell_ i j |j <- [0..n-1]]
        cell_ i j = foldl (+) 0 [Vect.index m1 i l * Vect.index m2 l j | l <- [0..k-1]]
    -}


Show (Matrix h w) where
    show (MakeMatrix m) = show m


full : (height: Nat) -> (width: Nat) -> Double -> Matrix height width
full h w val = MakeMatrix zeroes_ where
    zeroes_ = Vect.replicate h (Vect.replicate w val)


zeros : (height: Nat) -> (width: Nat) -> Matrix height width
zeros h w = full h w 0.0


ones : (height: Nat) -> (width: Nat) -> Matrix height width
ones h w = full h w 1.0


run_tests : IO ()
run_tests = do
    let m1 = ones 10 5
    let m2 = zeros 5 7
    let m3 = matmul m1 m2
    -- printLn m3
    printLn "Done"
