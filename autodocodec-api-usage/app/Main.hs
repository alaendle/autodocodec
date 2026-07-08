{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import           GHC.TypeNats (Nat)
import Autodocodec (HasCodec)
import Autodocodec.Class (HasCodec(codec))
import Autodocodec.Codec
import Autodocodec
import Autodocodec.OpenAPI
import Data.Data
import Data.OpenApi.Declare
import Data.OpenApi
import Data.Aeson (toJSON)
import Data.Aeson.Encode.Pretty 
import qualified Data.ByteString.Lazy.Char8 as B

-- case study, what is needed to extend autodocodec with TtG

-- 1. Define own data structures

-- One newtype, bounds encoded at the type level = single source of truth.
newtype BoundedString (lo :: Nat) (hi :: Nat) = BoundedString { unBoundedString :: String }
  deriving stock (Show, Eq)

instance HasCodec (BoundedString lo hi) where
  codec = dimapCodec BoundedString unBoundedString codec

data Business 
  = Business
  { businessName :: BoundedString 1 100
  , businessAddress :: String
  , businessRevenue :: Integer
  } deriving stock (Show, Eq)

instance HasCodec Business where
  codec = object "Business" $
    Business
      <$> requiredField "name" "The name of the business" .= businessName
      <*> requiredField "address" "The address of the business" .= businessAddress
      <*> requiredField "revenue" "The revenue of the business" .= businessRevenue

main :: IO ()
main = do
    let (_, (NamedSchema _ s)) = flip runDeclare mempty $ declareNamedSchemaViaCodec @Business Proxy
    B.putStrLn $ encodePretty s
