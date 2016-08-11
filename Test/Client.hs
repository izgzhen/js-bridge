import Language.JS.Type
import Language.JS.Platform
import Control.JS.Utils.Client

domains :: Domains
domains =
    let x    = JEVar  (Name "x")
        zero = JEPrim (PNumber 0.0)
    in Domains [(PTyNumber, [ "x" .@ (x .> zero)
                            , "x" .@ (x .== zero)
                            , "x" .@ (x .< zero)])]

main :: IO ()
main = do
    idl <- readFile "prelude.webidl"
    startSession domains idl $ \handler -> do
        _ <- newCons handler (Name "Bar") []
        return ()
