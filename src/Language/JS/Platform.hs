{-# LANGUAGE DeriveGeneric #-}

module Language.JS.Platform (
  JsExpr(..), LVar(..), JsVal(..), JsType(..),
  RelBiOp(..), Reply(..), PlatPort, Command(..),
  startSession, invoke, eval, assert, end
) where

import Language.JS.Type

import Network.Simple.TCP
import Network.Socket (socketToHandle)
import System.IO
import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import GHC.Generics

data Command = CInvoke LVar Name [JsExpr]
             | CEval JsExpr
                     (Maybe [JsExpr]) -- probing assertions
             | CAssert JsExpr
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

data Reply = Sat (Maybe JRef)
           | Unsat
           | Replies [Bool] -- True: Sat, False: Unsat
           | InvalidReqeust String
           deriving (Show, Generic)

type PlatPort = Handle

sendCmd :: Handle -> Command -> IO Reply
sendCmd handler cmd = do
  putStrLn $ "[SEND REQ] " ++ show cmd
  BS.hPut handler (BSL.toStrict (encode cmd) `BC.snoc` '\n')
  replyRaw <- BS.hGetLine handler
  let mReply = decode (BSL.fromStrict replyRaw) :: Maybe Reply
  case mReply of
      Just reply -> do
        putStrLn $ "[GET RES] " ++ show reply
        return reply
      Nothing -> return (InvalidReqeust "Invalid reply")

invoke :: Handle -> LVar -> Name -> [JsExpr] -> IO Reply
invoke handler lvar name es = sendCmd handler (CInvoke lvar name es)

eval :: Handle -> JsExpr -> Maybe [JsExpr] -> IO Reply
eval handler e mDomains = sendCmd handler (CEval e mDomains)

assert :: Handle -> JsExpr -> IO Reply
assert handler e = sendCmd handler (CAssert e)

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
