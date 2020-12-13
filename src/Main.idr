module Main


export
my_plus : Int -> Int -> Int
my_plus a b = a + b


-------------------------------------------------------------------------------
-- Schema definition
-------------------------------------------------------------------------------


infixr 5 .+.


data Schema : Type where
    IntVal : Schema
    StrVal : Schema
    (.+.) : Schema -> Schema -> Schema


Show Schema where
    show IntVal = "i"
    show StrVal = "s"
    show (a .+. b) = show a ++ show b


parse : String -> Maybe Schema
parse "" =  Nothing
parse "s" = pure StrVal
parse "i" = pure IntVal
parse s = do
    rest <- parse (strTail s)
    case strHead s of
        's' => pure $ StrVal .+. rest
        'i' => pure $ IntVal .+. rest


SchemaType : Schema -> Type
SchemaType IntVal = Integer
SchemaType StrVal = String
SchemaType (a .+. b) = (SchemaType a, SchemaType b)


displayVal : (SchemaType schema) -> String
displayVal {schema} v = "(" ++ recur schema v ++ ")" where
    recur (a .+. b) (va, vb) = recur a va ++ "," ++ recur b vb
    recur IntVal va = show va
    recur StrVal vb = vb


-------------------------------------------------------------------------------
-- Data store
-------------------------------------------------------------------------------


record Store where
    constructor MkStore
    schema: Schema
    content: List (SchemaType schema)


Show Store where
    show s = "{schema= " ++ show (schema s) ++ ", content= [" ++ contentStr ++ "]}" where
        contentStr = unwords (intersperse "," (map displayVal (content s)))


getStore: (store: Store) -> Integer -> Maybe (SchemaType (schema store))
getStore store i =
    let i = fromIntegerNat i in
        case inBounds i (content store) of
            (Yes _) => pure (index i (content store))
            _ => Nothing

{-
    if length (content store) < i
        then pure (index i (content store))
        else Nothing
-}

addToStore : (store: Store) -> SchemaType (schema store) -> Store
addToStore store val = MkStore (schema store) (val :: content store)


parseVal : String -> (schema: Schema) -> Maybe (SchemaType schema)
parseVal s schema = recur (words s) schema where
    recur [x] StrVal = pure x
    recur [x] IntVal =
        case all isDigit (unpack x) of
            True => pure (cast x)
            False => Nothing
    recur (x :: xs) (a .+. b) = do
        a <- recur [x] a
        b <- recur xs b
        pure (a, b)
    recur _ _ = Nothing


-------------------------------------------------------------------------------
-- Main loop
-------------------------------------------------------------------------------


storeRepl : Store -> IO Store
storeRepl store = do
    putStr "Add input: "
    val <- getLine
    case parseVal val (schema store) of  -- cannot use schema_!
        Nothing => do
            putStrLn "Bad parse"
            pure store
        Just val => do
            let store = addToStore store val
            putStrLn "Success"
            pure store


main : IO ()
main = do
    putStr "Enter schema: "
    schema_ <- getLine
    case parse schema_ of
        Nothing => putStrLn "Invalid schema"
        Just schema_ => do
            putStr "Valid schema: "
            printLn schema_
            let store = MkStore schema_ []
            store <- storeRepl store
            printLn store
            store <- storeRepl store
            printLn store
