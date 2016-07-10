{-# LANGUAGE DeriveGeneric #-}

module JS.Type where

import GHC.Generics

data Prim = PrimNum Double
          | PrimBool Bool
          | PrimStr String
          | PrimNull
          | PrimUndefined
          deriving (Generic, Eq)

instance Show Prim where
    show (PrimNum d)  = show d
    show (PrimBool b) = if b then "true" else "false"
    show (PrimStr s)  = show s
    show PrimNull     = "null"
    show PrimUndefined = "undefined"

newtype Name = Name String deriving (Eq, Generic, Ord)

instance Show Name where
    show (Name x) = x
