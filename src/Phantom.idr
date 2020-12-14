module Phantom


public export
data ValidationStatus = Validated | NotValidated


export 
record Statement (status : ValidationStatus) where
    constructor MakeStatement
    sqlQuery: String


export
newStatement : String -> Statement NotValidated
newStatement s = MakeStatement s


export
validate : Statement s -> Maybe (Statement Validated)
validate {s = Validated} stmt = Just stmt
validate {s = NotValidated} stmt =
    if isInfixOf "inject" (sqlQuery stmt)
        then Nothing
        else Just (MakeStatement (sqlQuery stmt))


export
execute : Statement Validated -> IO ()
execute stmt = do
    printLn ("Executing: " ++ sqlQuery stmt)


export
run_phantom: IO()
run_phantom = do
    query <- getLine
    let stmt = newStatement query
    -- execute stmt -- TODO: uncomment to show the compilation failure
    case validate stmt of
        Nothing => do
            printLn "Invalid statement"
        Just stmt => do
            execute stmt
