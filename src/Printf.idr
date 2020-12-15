module Printf


data SchemaElem = IntVar | StrVar | StrCst String	


Schema : Type
Schema = List SchemaElem


parseSchema : String -> Schema
parseSchema s = recur "" (unpack s) where
    recur : String -> List Char -> Schema
    recur prefix (':' :: 'i' :: rest) = StrCst (reverse prefix) :: IntVar :: recur "" rest
    recur prefix (':' :: 's' :: rest) = StrCst (reverse prefix) :: StrVar :: recur "" rest
    recur prefix (x :: xs) = recur (strCons x prefix) xs
    recur prefix [] = [StrCst (reverse prefix)]


SchemaType : (schema: Schema) -> Type
SchemaType [] = String
SchemaType (IntVar :: xs) = Int -> SchemaType xs
SchemaType (StrVar :: xs) = String -> SchemaType xs
SchemaType (_ :: xs) = SchemaType xs


format : (s: String) -> SchemaType (parseSchema s)
format s = recur (parseSchema s) "" where
    recur : (schema: Schema) -> String -> SchemaType schema
    recur [] acc = acc
    recur (StrCst v :: schema) acc = recur schema (acc ++ v)
    recur (IntVar :: schema) acc = \i => recur schema (acc ++ show i)
    recur (StrVar :: schema) acc = \s => recur schema (acc ++ s)

