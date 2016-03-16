module Day3Hard

-- this module is just a placeholder,
-- see Proof.idr, which contains the complete proof.

-- some notes: keep holes in right hand side of a definition,
-- you'll see errors like the following:
{-
*Proof> :p plusZero
Declarations not solvable using prover
-}
-- I don't know the exact reason, but it seems toplevel holes cannot be used
-- as a proof goal.

-- Change the type definition to allow a custom "Time" to be passed in

-- the following shows a parsing error:
-- Trip : (Location : Type ** Float -> Location)
-- seems to be an Idris-side error: https://github.com/idris-lang/Idris-dev/issues/1965
-- so I'll just write down the idea: also keeping Time abstract, then
-- we can use the product of Location and Time in 1st position of dependent pair,
-- this is going to be:
-- Trip : ( (Location: Type, Time: Type) ** Time -> Location)
-- or an alternative way is to keep "Trip" as-is,
-- say explictly about the measurement of "Float", and have a "protocol"
-- "toFloat : Time -> Float" to convert from whatever time representation to
-- a standard one.
