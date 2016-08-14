import Language.JS.Type
import Language.JS.Platform
import Control.JS.Utils.Client

x    = JEVar  (Name "x")
zero = JEPrim (PNumber 0.0)
zeroi = JEPrim (PInt 0)


domains :: Domains
domains = Domains [(PTyNumber, [ "x" .@ (x .> zero)
                               , "x" .@ (x .== zero)
                               , "x" .@ (x .< zero)]),
                   (PTyInt   , [ "x" .@ (x .> zeroi)
                               , "x" .@ (x .== zeroi)
                               , "x" .@ (x .< zeroi)]),
                   (PTyBool  , [ "x" .@ (x .== JEPrim (PBool True))
                               , "x" .@ (x .== JEPrim (PBool False))])]

onePointNine :: JsUnionVal
onePointNine = JsUnionVal [JVPrim PTyNumber ("x" .@ (x .== JEPrim (PNumber 1.9)))]

oneInt = JsUnionVal [JVPrim PTyInt ("x" .@ (x .== JEPrim (PInt 1)))]

dictA :: JsUnionVal
dictA = JsUnionVal [JVDict [ (Name "c", JVPrim PTyInt ("x" .@ (x .== JEPrim (PInt 11))))
                           , (Name "g", JVPrim PTyInt ("x" .@ (x .== JEPrim (PInt 1))))]]

testHarness f = do
    idl <- readFile "Test/prelude.webidl"
    startSession domains idl f

caseObjEval :: IO ()
caseObjEval = testHarness $ \handler -> do
    res <- newCons handler (Name "Bar") []
    print res

casePrimEval :: IO ()
casePrimEval = testHarness $ \handler -> do
    res <- call handler (LInterface (Name "Foo")) (Name "pos") []
    print res

caseWrongArgNum :: IO ()
caseWrongArgNum = testHarness $ \handler -> do
    res <- call handler (LInterface (Name "Foo")) (Name "pos")
                        [onePointNine]
    print res

caseCallbackEval :: IO ()
caseCallbackEval = testHarness $ \handler -> do
    res <- call handler (LInterface (Name "Foo")) (Name "async")
                        [oneInt, JsUnionVal [JVClos 1]]
    print res

caseUnionEval :: IO ()
caseUnionEval = testHarness $ \handler -> do
    res <- call handler (LInterface (Name "Foo")) (Name "use_union")
                        [oneInt]
    print res

-- FIXME: to pass this, we need effectful initialization
-- caseDictEval :: IO ()
-- caseDictEval = testHarness $ \handler -> do
--     res <- call handler (LInterface (Name "Foo")) (Name "use_dict")
--                         [dictA]
--     print res

caseNewDef :: IO ()
caseNewDef = testHarness $ \handler -> do
    res <- newDef handler (Name "HTMLBar")
    print res

caseAccessConst :: IO ()
caseAccessConst = testHarness $ \handler -> do
    res <- get handler (LInterface (Name "Foo")) (Name "SOME_CONST")
    print res

caseSetGetAttr :: IO ()
caseSetGetAttr = testHarness $ \handler -> do
    Sat (JVRRef r, _) <- newCons handler (Name "Bar") []
    res1 <- set handler (LRef r) (Name "myAttr") (JVPrim PTyInt ("x" .@ (x .== JEPrim (PInt 11))))
    print res1
    res <- get handler (LRef r) (Name "myAttr")
    print res

-- caseGhostAttr :: IO ()
-- caseGhostAttr = testHarness $ \handler -> do
--     Sat (JVRRef r, _) <- newCons handler (Name "Bar") []
--     res1 <- call handler (LRef r) (Name "bumpCounter") []
--     print res1
--     res <- call handler (LRef r) (Name "hasBumped") []
--     print res

main :: IO ()
main = caseSetGetAttr
