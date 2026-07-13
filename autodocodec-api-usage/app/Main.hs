{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Autodocodec
import Autodocodec.OpenAPI
import Data.Aeson hiding (object, (.=))
import Data.Aeson.Encode.Pretty
import Data.Aeson.KeyMap (singleton)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Data
import Data.OpenApi
import Data.OpenApi.Declare (runDeclare)
import Data.Text (pack)
import GHC.Generics
import GHC.TypeNats (KnownNat, Nat, natVal)

-- case study, what is needed to extend autodocodec with TtG

-- 1. Define own data structures

-- One newtype, bounds encoded at the type level = single source of truth.
newtype BoundedString (lo :: Nat) (hi :: Nat) = BoundedString {unBoundedString :: String}
  deriving stock (Show, Eq)

-- instance HasCodec (BoundedString lo hi) where
--   codec = undefined -- obviously this couldn't work since if this would be possible we won't need an extension at all.

toJSONExt :: ToJSONExt MyExt
toJSONExt =
  ToJSONExt
    (\_ t -> toJSON ("Bounded: " <> t))
    (\_ t -> case t of Address s z -> singleton "address" (String $ pack $ s <> "-" <> z))

data Address
  = Address
  { -- addressStreet :: String,
    addressCity :: String,
    addressZipCode :: String
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSchema)

data Business
  = Business
  { businessName :: BoundedString 10 100,
    businessAddress :: Address,
    businessRevenue :: Integer
  }
  deriving stock (Show, Eq)

--    deriving (FromJSON, ToJSON) via (Autodocodec Business)

-- instance HasCodec Business where
--   codec = object "Business" $
--     Business
--       <$> requiredField "name" "The name of the business" .= businessName
--       <*> requiredField "address" "The address of the business" .= businessAddress
--       <*> requiredFieldWith "revenue" integerCodec "The revenue of the business" .= businessRevenue

-- 2. Lets build our "own" tree

data MyExt -- tree index

type instance XXValCodec MyExt = BoundedStringCodec

type instance XVal MyExt = String

type instance XXObjCodec MyExt = AddressCodec

type instance XObj MyExt = Address

type instance XObjectOfCodec MyExt = NoExtField

type instance XBimapCodec MyExt = NoExtField

type instance XRequiredKeyCodec MyExt = NoExtField

type instance XIntegerCodec MyExt = NoExtField

type instance XApCodec MyExt = NoExtField

type instance XPureCodec MyExt = NoExtField

type instance XReferenceCodec MyExt = NoExtField

type instance XOptionalKeyCodec MyExt = NoExtField

type instance XOptionalKeyWithDefaultCodec MyExt = NoExtField

type instance XOptionalKeyWithOmittedDefaultCodec MyExt = NoExtField

type instance XDiscriminatedUnionCodec MyExt = NoExtField

type instance XEitherCodec MyExt = NoExtField

type instance XCommentCodec MyExt = NoExtField

type instance XArrayOfCodec MyExt = NoExtField

type instance XMapCodec MyExt = NoExtField

type instance XHashMapCodec MyExt = NoExtField

type instance XEqCodec MyExt = NoExtField

type instance XStringCodec MyExt = NoExtField

type instance XBoolCodec MyExt = NoExtField

type instance XNullCodec MyExt = NoExtField

data BoundedStringCodec = BoundedStringCodec Nat Nat deriving stock (Show, Eq)

data AddressCodec = AddressCodec Schema deriving stock (Show, Eq)

boundedStringCodec :: (KnownNat lo, KnownNat hi) => JSONCodecAt MyExt (BoundedString lo hi)
boundedStringCodec = boundedStringCodec' Proxy Proxy

boundedStringCodec' :: (KnownNat lo, KnownNat hi) => Proxy lo -> Proxy hi -> JSONCodecAt MyExt (BoundedString lo hi)
boundedStringCodec' pl ph = XValCodec $ BoundedStringCodec (natVal pl) (natVal ph)

addressCodec :: ObjectCodecAt MyExt Address Address
addressCodec = XObjCodec $ AddressCodec $ toSchema (Proxy @Address)

businessCodec :: JSONCodecAt MyExt Business
businessCodec =
  object "Business" $
    Business
      <$> requiredFieldWith "name" boundedStringCodec "The name of the business" .= businessName
      <*> lmapCodec businessAddress addressCodec -- requiredFieldWith "address" addressCodec "The address of the business" .= businessAddress
      <*> requiredFieldWith "revenue" integerCodec "The revenue of the business" .= businessRevenue

main :: IO ()
main = do
  -- let (_, (NamedSchema _ s)) = flip runDeclare mempty $ declareNamedSchemaViaCodec @Business Proxy
  -- B.putStrLn $ encodePretty s
  let b = Business (BoundedString "My Business") (Address "Anytown" "12345") 1000000
  B.putStrLn $ encodePretty $ toJSONViaExt toJSONExt businessCodec b
  print $ showCodecABitAt show show businessCodec

  let (_, NamedSchema _ s) = flip runDeclare mempty $ declareNamedSchemaViaAt businessCodec toJSONExt (\(BoundedStringCodec lo hi) -> pure $ NamedSchema Nothing (mempty {_schemaMinLength = Just (fromIntegral lo), _schemaMaxLength = Just (fromIntegral hi)})) (\(AddressCodec as) -> pure . pure $ as)
  B.putStrLn $ encodePretty s
