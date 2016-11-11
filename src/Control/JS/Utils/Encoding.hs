module Control.JS.Utils.Encoding where

import Data.MessagePack
import Data.MessagePack.Aeson

instance (FromJSON x, ToJSON x) => MessagePack x where
  toObject = fromAeson . toJSON
  fromObject o =
    case toAeson o of
      Nothing -> Nothing
      Just x ->
        case fromJSON x of
          Error _ -> Nothing
          Success y -> Just y

