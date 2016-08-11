{-# LANGUAGE DeriveGeneric #-}

module Language.JS.Platform where

import Language.JS.Type

import System.IO

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import GHC.Generics

data Command = CBoot Domains String -- Bootstrapping: domain assertions map and idl prelude
             | CCall LVar Name [JsVal] -- Call
             | CGet LVar Name
             | CSet LVar Name JsVal
             | CNew Name (Maybe [JsVal]) -- Nothing means "by default"
             | CEnd -- End session
             deriving (Show, Generic)

data JsExpr = JEBinary RelBiOp JsExpr JsExpr
            -- | JEUnary UnaryOp JsExpr -- TODO
            | JEPrim Prim
            | JEVar Name
            deriving (Show, Generic)

data JAssert = JAssert Name JsExpr
             deriving (Show, Generic)

data LVar = LRef JRef
          | LInterface Name
          deriving (Show, Generic)

newtype JsUnionVal = JsUnionVal [JsVal] deriving (Show, Generic)

data JsVal = JVRef JRef
           | JVPrim PrimType JAssert
           | JVClos Int
           | JVDict [(Name, JsVal)]
           deriving (Generic, Show)

data JsType = JTyObj Name
            | JTyPrim PrimType
            deriving (Show, Eq)

data RelBiOp = NotEqual
             | Equal
             | And
             | Or
             | LessThan
             | LessEq
             | GreaterThan
             | GreaterEq
             deriving (Show, Generic)

data JsValResult = JVRRef JRef
                 | JVRPrim PrimType [Bool]
                 | JVRObj [(Name, JsValResult)]
                 deriving (Show, Generic)

data JsCallbackResult = JsCallbackResult [Maybe (JsValResult)] deriving (Show, Generic)

data Reply = Sat (Maybe JsValResult, Maybe JsCallbackResult) -- return values and callback feed
           | Unsat String -- Check failed and explaination
           | InvalidReqeust
           deriving (Show, Generic)

type PlatPort = Handle

data Domains = Domains [(PrimType, [JAssert])] deriving (Show, Generic) -- Domain assertions

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
instance ToJSON JsValResult where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON JsCallbackResult where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON JsUnionVal where
    toEncoding = genericToEncoding defaultOptions

instance FromJSON Command
instance FromJSON JsExpr
instance FromJSON LVar
instance FromJSON JsVal
instance FromJSON RelBiOp
instance FromJSON Reply
instance FromJSON Domains
instance FromJSON JAssert
instance FromJSON JsValResult
instance FromJSON JsCallbackResult
instance FromJSON JsUnionVal

(.>) :: JsExpr -> JsExpr -> JsExpr
(.>) = JEBinary GreaterThan

(.<) :: JsExpr -> JsExpr -> JsExpr
(.<) = JEBinary LessThan

(.==) :: JsExpr -> JsExpr -> JsExpr
(.==) = JEBinary Equal

(.@) :: String -> JsExpr -> JAssert
(.@) x e = JAssert (Name x) e
