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

parseSingleVal :: String -> Maybe JsVal
parseSingleVal "Bool" = Just (JVPrim PTyBool true)
parseSingleVal _ = Nothing

parseVal :: String -> Maybe JsUnionVal
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

