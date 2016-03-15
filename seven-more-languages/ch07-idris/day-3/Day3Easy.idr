module Day3Easy

-- take a name of type and return that type

-- here we wrap result in "Maybe" to avoid the problem of the function
-- being partial.
-- more complicated type might need some parsing work
-- which I don't know how to.
-- but I guess this is good enough.
total
typeNamed : String -> Maybe Type
typeNamed "Int" = Just Int
typeNamed "Char" = Just Char
typeNamed "String" = Just String
typeNamed _ = Nothing
