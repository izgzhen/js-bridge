{-# LANGUAGE DeriveGeneric #-}

module Language.JS.Platform where

import Language.JS.Type
import Text.PrettyPrint.Leijen hiding ((<$>))

import System.IO

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC
import GHC.Generics

data Command = CBoot Domains String -- Bootstrapping: domain assertions map and idl prelude
             | CCall LVar Name [JsUnionVal] -- Call
             | CGet LVar Name
             | CSet LVar Name JsVal -- Property can't be union type
             | CNew Name (Maybe [JsUnionVal]) -- Nothing means "by default"
             | CEnd -- End session
             deriving (Show, Generic)

data JsExpr = JEBinary RelBiOp JsExpr JsExpr
            | JEPrim Prim
            | JEVar Name
            deriving (Show, Generic)

data JAssert = JAssert Name JsExpr
             deriving (Show, Generic)

data LVar = LRef JRef
          | LInterface Name
          deriving (Show, Generic)

newtype JsUnionVal = JsUnionVal [JsVal] deriving (Show, Generic)

data JsVal = JVRef JRef -- Interface
           | JVPrim PrimType JAssert -- Primitives
           | JVConst Prim -- Constant Primitives
           | JVClos Int -- Callback
           | JVDict [(Name, JsVal)] -- Dictionary
           deriving (Generic, Show)

newtype JsValStr = JsValStr String -- String representation
                 deriving (Show, Generic)

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
                 | JVRPrimPrecise Prim
                 | JVRObj [(Name, JsValResult)]
                 deriving (Show, Generic)

data JsCallbackResult = JsCallbackResult [Maybe [JsValResult]] deriving (Show, Generic)

data Reply = Sat (JsValResult, Maybe JsCallbackResult) -- return values and callback feed
           | Unsat String -- Check failed and explaination
           | InvalidReqeust String
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
instance ToJSON JsValStr where
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
instance FromJSON JsValStr

(.>) :: JsExpr -> JsExpr -> JsExpr
(.>) = JEBinary GreaterThan

(.<) :: JsExpr -> JsExpr -> JsExpr
(.<) = JEBinary LessThan

(.==) :: JsExpr -> JsExpr -> JsExpr
(.==) = JEBinary Equal

(.@) :: String -> JsExpr -> JAssert
(.@) x e = JAssert (Name x) e


app :: JAssert -> Name -> JsExpr
app jass@(JAssert x0 je) x = subst je
  where
    subst (JEBinary op e1 e2) = JEBinary op (subst e1) (subst e2)
    subst (JEPrim prim) = JEPrim prim
    subst (JEVar x0') | x0 == x0' = JEVar x
                      | otherwise = error $ "Non-closed assertion: " ++ show jass

instance Pretty JsExpr where
  pretty (JEBinary op e1 e2) = parens (pretty e1 <+> pretty op <+> pretty e2)
  pretty (JEPrim prim) = pretty prim
  pretty (JEVar (Name x)) = text x

instance Pretty RelBiOp where
    pretty = \case
        NotEqual -> text "!="
        Equal -> text "=="
        And -> text "&&"
        Or -> text "||"
        LessThan -> text "<"
        LessEq -> text "<="
        GreaterThan -> text ">"
        GreaterEq -> text ">="

