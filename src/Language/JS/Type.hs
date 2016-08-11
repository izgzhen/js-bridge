{-# LANGUAGE LambdaCase, DeriveGeneric #-}

module Language.JS.Type where

import GHC.Generics
import Data.Aeson
import Text.PrettyPrint.Leijen

data Prim = PNumber Double
          | PBool Bool
          | PString String
          | PNull
          deriving (Generic, Show, Eq)

data PrimType = PTyNull
              | PTyNumber
              | PTyString
              | PTyBool
              deriving (Generic, Ord, Show, Eq)

newtype Name = Name { unName :: String } deriving (Eq, Show, Generic, Ord)

newtype JRef = JRef { unJRef :: Int } deriving (Eq, Ord, Show, Generic)

inferPrimType :: Prim -> PrimType
inferPrimType = \case
  PNull -> PTyNull
  PNumber _ -> PTyNumber
  PString _ -> PTyString
  PBool _ -> PTyBool

defaultPrim :: PrimType -> Prim
defaultPrim = \case
  PTyNull -> PNull
  PTyNumber -> PNumber 0.0
  PTyString -> PString ""
  PTyBool -> PBool True

instance ToJSON Prim where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Name where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON JRef where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON PrimType where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Prim
instance FromJSON Name
instance FromJSON JRef
instance FromJSON PrimType

instance Pretty Prim where
    pretty PNull = text "null"
    pretty (PNumber i) = pretty i
    pretty (PString s) = text "\"" <> text (show s) <> text "\""
    pretty (PBool True) = text "true"
    pretty (PBool False) = text "false"
