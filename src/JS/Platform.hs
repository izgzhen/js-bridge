{-# LANGUAGE DeriveGeneric #-}

module JS.Platform where

import JS.Type

import Network.Simple.TCP
import Network.Socket (socketToHandle)
import System.IO
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import GHC.Generics

data Command = CInvoke LVar Name [JsExpr]
             -- | Eval JsExpr JsVal
             -- | Assert JsExpr
             | CEnd
             deriving (Show, Generic)

data JsExpr = JVal JsVal
            | JCall LVar Name [JsExpr]
            | JAccess LVar Name
            | JRel RelBiOp JsExpr JsExpr
            | JNew Name [JsExpr]
            deriving (Show, Generic)

data LVar = LVal JsVal
          | LInterface Name
          deriving (Show, Generic)

data JsVal = JVRef JRef
           | JVPrim Prim
             deriving (Generic, Show)

data JsType = JTyObj Name
            | JTyPrim PrimType
            deriving (Show, Eq)

data RelBiOp = NotEqual | Equal deriving (Show, Generic)

startSession :: (PlatPort -> IO a) -> IO a
startSession m = connect "localhost" "8888" $ \(sock, addr) -> do
    putStrLn $ "Connection established to " ++ show addr
    handler <- socketToHandle sock ReadWriteMode
    ret <- m handler
    end handler
    return ret

data Reply = Sat | Unsat deriving (Show, Generic)

type PlatPort = Handle

invoke :: Handle -> LVar -> Name -> [JsExpr] -> IO Reply
invoke handler lvar name es = do
  let cmd = CInvoke lvar name es
  putStrLn $ "[SEND REQ] " ++ show cmd
  BS.hPut handler (BSL.toStrict (encode cmd) `BC.snoc` '\n')
  replyRaw <- BS.hGetLine handler
  let mReply = decode (BSL.fromStrict replyRaw) :: Maybe Reply
  case mReply of
      Just reply -> do
        putStrLn $ "[GET RES] " ++ show reply
        return reply
      Nothing -> error "Invalid reply"

end :: Handle -> IO ()
end handler = do
  let cmd = CEnd
  putStrLn $ "[SEND REQ] " ++ show cmd
  BS.hPut handler (BSL.toStrict (encode cmd) `BC.snoc` '\n')

instance ToJSON Command where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON JsExpr where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON LVar where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON JsVal where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON RelBiOp where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Reply where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Command
instance FromJSON JsExpr
instance FromJSON LVar
instance FromJSON JsVal
instance FromJSON RelBiOp
instance FromJSON Reply
