import Language.JS.Type
import Language.JS.Platform

domains :: Domains
domains = Domains [(PTyInt, [ JAssert (Name "x") (JRel GreaterThan (JVal (JVVar (Name "x"))) (JVal (JVPrim (PInt 0))))
                            , JAssert (Name "x") (JRel Equal       (JVal (JVVar (Name "x"))) (JVal (JVPrim (PInt 0))))
                            , JAssert (Name "x") (JRel LessThan    (JVal (JVVar (Name "x"))) (JVal (JVPrim (PInt 0))))])]

main = startSession domains $ \handler -> do
    -- _ <- eval handler (JNew (Name "Bar") [])
    r <- eval handler (JCall (LInterface (Name "Foo"))
                             (Name "async")
                             [ JVal (JVPrim (PInt 10))
                             , JEClos 1])
    print r
    return ()
