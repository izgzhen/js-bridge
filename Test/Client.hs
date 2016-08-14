import Language.JS.Type
import Language.JS.Platform
import Control.JS.Utils.Client

x    = JEVar  (Name "x")
zero = JEPrim (PNumber 0.0)

domains :: Domains
domains = Domains [(PTyNumber, [ "x" .@ (x .> zero)
                               , "x" .@ (x .== zero)
                               , "x" .@ (x .< zero)])]


onePointNine :: JsUnionVal
onePointNine = JsUnionVal [JVPrim PTyNumber ("x" .@ (x .== JEPrim (PNumber 1.9)))]

caseObjEval :: IO ()
caseObjEval = do
    idl <- readFile "Test/prelude.webidl"
    startSession domains idl $ \handler -> do
        res <- newCons handler (Name "Bar") []
        print res

casePrimEval :: IO ()
casePrimEval = do
    idl <- readFile "Test/prelude.webidl"
    startSession domains idl $ \handler -> do
        res <- call handler (LInterface (Name "Foo")) (Name "pos") []
        print res

caseWrongArgNum :: IO ()
caseWrongArgNum = do
    idl <- readFile "Test/prelude.webidl"
    startSession domains idl $ \handler -> do
        res <- call handler (LInterface (Name "Foo")) (Name "pos") [onePointNine]
        print res


main :: IO ()
main = caseWrongArgNum

