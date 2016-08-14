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

caseSimpleEval :: IO ()
caseSimpleEval = do
    idl <- readFile "Test/prelude.webidl"
    startSession domains idl $ \handler -> do
        res <- newCons handler (Name "Bar") []
        print res
        return ()


main :: IO ()
main = caseSimpleEval

