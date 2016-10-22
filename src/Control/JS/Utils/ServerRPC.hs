import Network.MessagePack.Server
import Language.JS.Type
import Language.JS.Platform

import Prelude hiding (div)
import Data.MessagePack
import Data.String.Utils
import Control.Monad.IO.Class (liftIO)

add, sub, mul :: Int -> Int -> Server Int
add x y = return $ x + y
sub x y = return $ x - y
mul x y = return $ x * y

div :: Int -> Int -> Server Double
div x y = return $ fromIntegral x / fromIntegral y

true :: JAssert
true = JAssert (Name "x") (JEPrim (PBool True))

parseSingleVal :: String -> Either String JsVal
parseSingleVal "Bool" = Right (JVPrim PTyBool true)
parseSingleVal "Num" = Right (JVPrim PTyNumber true)
parseSingleVal "Str" = Right (JVPrim PTyString true)
parseSingleVal "Null" = Right (JVPrim PTyNull true)
parseSingleVal "UInt" = Right (JVPrim PTyInt true)
parseSingleVal "NotUInt" = Right (JVPrim PTyNumber true) -- XXX: too coarse
parseSingleVal s = Left s


parseVal :: String -> Either String JsUnionVal
parseVal s = let vals = split "|" s in
             JsUnionVal <$> sequence (map parseSingleVal vals)

unknown :: JsValStr -> Server Int
unknown (JsValStr s) = do
    liftIO $ print (parseVal s)
    return 1

main = serve 8888 [ method "add" add
                  , method "sub" sub
                  , method "mul" mul
                  , method "div" div
                  , method "unknown" unknown ]

