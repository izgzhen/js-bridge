{-# LANGUAGE DeriveGeneric #-}

module JS.Platform where

import JS.Type

import Network.Simple.TCP
import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
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

startSession :: (Socket -> IO a) -> IO a
startSession m = connect "localhost" "8888" $ \(sock, addr) -> do
    putStrLn $ "Connection established to " ++ show addr
    m sock

data Reply = Sat | Unsat deriving (Show, Generic)

invoke :: Socket -> LVar -> Name -> [JsExpr] -> IO Reply
invoke sock lvar name es = do
  let cmd = CInvoke lvar name es
  putStrLn $ "[SEND REQ] " ++ show cmd
  sendLazy sock (encode cmd)
  mReplyRaw <- recv sock 100
  case mReplyRaw of
    Just replyRaw -> do
      let mReply = decode (fromStrict replyRaw) :: Maybe Reply
      case mReply of
        Just reply -> return reply
        Nothing -> error "Invalid reply"
    Nothing -> error "Invalid reply"

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
