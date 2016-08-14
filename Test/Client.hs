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

caseWrongNumArgs :: IO ()
caseWrongNumArgs = do
    idl <- readFile "Test/prelude.webidl"
    startSession domains idl $ \handler -> do
        res <- call handler (LInterface (Name "Foo")) (Name "pos") []
        print res

main :: IO ()
main = caseWrongNumArgs

