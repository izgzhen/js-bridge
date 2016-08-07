{-# LANGUAGE DeriveGeneric #-}

module Language.JS.Platform (
  JsExpr(..), LVar(..), JsVal(..), JsType(..), JAssert(..),
  RelBiOp(..), Reply(..), PlatPort, Command(..), Domains(..),
  startSession, invoke, eval, assert, end, eGetLine,
  ePutLine
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
             | CAssert JsExpr
             | CEnd
             deriving (Show, Generic)

data JsExpr = JVal JsVal
            | JInterface Name
            | JCall LVar Name [JsExpr]
            | JAccess LVar Name
            | JRel RelBiOp JsExpr JsExpr
            | JNew Name [JsExpr]
            | JEClos Int -- n-ary closure
            deriving (Show, Generic)

data JAssert = JAssert Name JsExpr
             deriving (Show, Generic)

data LVar = LVal JsVal
          | LInterface Name
          deriving (Show, Generic)

data JsVal = JVRef JRef
           | JVPrim Prim
           | JVVar Name
             deriving (Generic, Show)

data JsType = JTyObj Name
            | JTyPrim PrimType
            deriving (Show, Eq)

data RelBiOp = NotEqual
             | Equal
             | And
             | LessThan
             | LessEq
             | GreaterThan
             | GreaterEq
             deriving (Show, Generic)

data Reply = Sat (Maybe JRef)
           | Unsat
           | Replies PrimType [Bool] -- True: Sat, False: Unsat
           | InvalidReqeust String
           | ReplyCallback [Reply]
           deriving (Show, Generic)

type PlatPort = Handle

data Domains = Domains [(PrimType, [JAssert])] deriving (Show, Generic) -- Domain assertions

startSession :: Domains -> (PlatPort -> IO a) -> IO a
startSession domains f = connect "localhost" "8888" $ \(sock, addr) -> do
    putStrLn $ "Connection established to " ++ show addr
    handler <- socketToHandle sock ReadWriteMode
    putStrLn "[SEND DOMAINS]"
    ePutLine handler domains
    ret <- f handler
    end handler
    return ret

sendCmd :: Handle -> Command -> IO Reply
sendCmd handler cmd = do
  putStrLn $ "[SEND REQ] " ++ show cmd
  ePutLine handler cmd
  mReply <- eGetLine handler
  case mReply of
      Just reply -> do
        putStrLn $ "[GET RES] " ++ show reply
        return reply
      Nothing -> return (InvalidReqeust "Invalid reply")

invoke :: Handle -> LVar -> Name -> [JsExpr] -> IO Reply
invoke handler lvar name es = sendCmd handler (CInvoke lvar name es)

eval :: Handle -> JsExpr -> IO Reply
eval handler e = sendCmd handler (CEval e)

assert :: Handle -> JsExpr -> IO Reply
assert handler e = sendCmd handler (CAssert e)

end :: Handle -> IO ()
end handler = do
  let cmd = CEnd
  putStrLn $ "[SEND REQ] " ++ show cmd
  ePutLine handler cmd

eGetLine :: FromJSON a => Handle -> IO (Maybe a)
eGetLine handler = decode . BSL.fromStrict <$> BS.hGetLine handler

ePutLine :: ToJSON a => Handle -> a -> IO ()
ePutLine handler x = BS.hPut handler (BSL.toStrict (encode x) `BC.snoc` '\n')

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
instance ToJSON Domains where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON JAssert where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Command
instance FromJSON JsExpr
instance FromJSON LVar
instance FromJSON JsVal
instance FromJSON RelBiOp
instance FromJSON Reply
instance FromJSON Domains
instance FromJSON JAssert
