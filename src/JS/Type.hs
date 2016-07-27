{-# LANGUAGE LambdaCase, DeriveGeneric #-}

module JS.Type where

import GHC.Generics
import Data.Aeson
import Text.PrettyPrint.Leijen

data Prim = PInt Int
          | PDouble Double
          | PBool Bool
          | PString String
          | PNull
          | PUndefined
          deriving (Generic, Show, Eq)

data PrimType = PTyNull
              | PTyUndefined
              | PTyInt
              | PTyDouble
              | PTyString
              | PTyBool
              deriving (Show, Eq)

newtype Name = Name { unName :: String } deriving (Eq, Show, Generic, Ord)

newtype JRef = JRef { unJRef :: Int } deriving (Eq, Ord, Show, Generic)

inferPrimType :: Prim -> PrimType
inferPrimType = \case
  PNull -> PTyNull
  PInt _ -> PTyInt
  PDouble _ -> PTyDouble
  PString _ -> PTyString
  PBool _ -> PTyBool
  PUndefined -> PTyUndefined

defaultPrim :: PrimType -> Prim
defaultPrim = \case
  PTyNull -> PNull
  PTyUndefined -> PUndefined
  PTyInt -> PInt 0
  PTyDouble -> PDouble 0.0
  PTyString -> PString ""
  PTyBool -> PBool True

instance ToJSON Prim where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON Name where
    toEncoding = genericToEncoding defaultOptions
instance ToJSON JRef where
    toEncoding = genericToEncoding defaultOptions
instance FromJSON Prim
instance FromJSON Name
instance FromJSON JRef

instance Pretty Prim where
    pretty PNull = text "null"
    pretty (PInt i) = pretty i
    pretty (PDouble d) = pretty d
    pretty (PString s) = text "\"" <> text (show s) <> text "\""
    pretty (PBool True) = text "true"
    pretty (PBool False) = text "false"
