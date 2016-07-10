{-# LANGUAGE DeriveGeneric #-}

module JS.Platform where

import JS.Type

import Network.Simple.TCP
import Data.Aeson
import Data.ByteString.Lazy (fromStrict)
import GHC.Generics

type PlatPort = Socket

data Command = CInvoke LVar Name [PlatExpr]
             -- | Eval PlatExpr PlatVal
             -- | Assert PlatExpr
             | CEnd
             deriving (Show, Generic)
data PlatExpr = PVal PlatVal
              | PCall LVar Name [PlatExpr]
              | PAccess LVar Name
              | PRel RelBiOp PlatExpr PlatExpr
              | PNew Name [PlatExpr]
              deriving (Show, Generic)

data LVar = LVal PlatVal
          | LInterface Name
          deriving (Show, Generic)

newtype PRef = PRef { unPRef :: Int } deriving (Eq, Ord, Show, Generic)

data PlatVal = PVRef PRef
             | PVPrim Prim
             deriving (Generic, Show)

data RelBiOp = NotEqual | Equal deriving (Show, Generic)

startSession :: (PlatPort -> IO a) -> IO a
startSession m = connect "localhost" "8888" $ \(sock, addr) -> do
    putStrLn $ "Connection established to " ++ show addr
    m sock

data Reply = Sat | Unsat deriving (Show, Generic)

invoke :: PlatPort -> LVar -> Name -> [PlatExpr] -> IO Reply
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
instance ToJSON PlatExpr where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON LVar where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON PRef where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON PlatVal where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON RelBiOp where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Prim where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Name where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Reply where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Command
instance FromJSON PlatExpr
instance FromJSON LVar
instance FromJSON PRef
instance FromJSON PlatVal
instance FromJSON RelBiOp
instance FromJSON Prim
instance FromJSON Name
instance FromJSON Reply
