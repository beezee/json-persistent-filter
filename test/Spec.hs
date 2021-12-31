{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

import Data.Aeson
import Database.Persist.Sql
import Database.Persist.TH
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS8
import Data.Fixed
import Data.Functor.Const
import Data.Int
import qualified Data.Map as M
import Data.List (partition)
import Data.Maybe
import Data.Proxy
import qualified Data.Text as T
import Data.Time (Day, secondsToDiffTime, TimeOfDay, TimeOfDay(..), UTCTime, UTCTime(..))
import Data.Time.Calendar (Day(..))
import Lib
import Generic.Random
import GHC.Generics
import Test.QuickCheck hiding (reason)
import Test.QuickCheck.Instances.ByteString()
import Test.QuickCheck.Instances.Text()
import Test.QuickCheck.Monadic
import Test.QuickCheck.Property

share [mkPersist sqlSettings] [persistLowerCase|
KitchenSink
    name T.Text
    bs ByteString
    dbl Double
    rt Rational
    i64 Int64
    bool Bool
    day Day
    timeOfDay TimeOfDay
    utcTime UTCTime
    list [PersistValue]
    map (M.Map T.Text PersistValue)
    age Int Maybe
    deriving Generic Show
|]

instance Arbitrary KitchenSink where
  arbitrary = genericArbitrary uniform

instance Arbitrary (Entity KitchenSink) where
  arbitrary = Entity 
    <$> (toSqlKey <$> arbitrary)
    <*> arbitrary

instance ParseEntityField KitchenSink where
  parseEntityField def = case unFieldNameHS . fieldHaskell $ def of
    "name" -> Just . ParsedEntityField $ KitchenSinkName
    "age" -> Just . ParsedEntityField $ KitchenSinkAge
    "bs" -> Just . ParsedEntityField $ KitchenSinkBs
    "dbl" ->  Just . ParsedEntityField $ KitchenSinkDbl
    "rt" ->  Just . ParsedEntityField $ KitchenSinkRt
    "i64" ->  Just . ParsedEntityField $ KitchenSinkI64
    "bool" ->  Just . ParsedEntityField $ KitchenSinkBool
    "day" ->  Just . ParsedEntityField $ KitchenSinkDay
    "timeOfDay" ->  Just . ParsedEntityField $ KitchenSinkTimeOfDay
    "utcTime" ->  Just . ParsedEntityField $ KitchenSinkUtcTime
    "list" ->  Just . ParsedEntityField $ KitchenSinkList
    "map" ->  Just . ParsedEntityField $ KitchenSinkMap
    _ -> Nothing

data EF where
  EF :: a -> EntityField KitchenSink a -> EF

kitchenSinkChecksAllFieldTypes :: PersistValue -> EF
kitchenSinkChecksAllFieldTypes (PersistText t) = EF t KitchenSinkName
kitchenSinkChecksAllFieldTypes (PersistByteString t) = EF t KitchenSinkBs
kitchenSinkChecksAllFieldTypes (PersistDouble t) = EF t KitchenSinkDbl
kitchenSinkChecksAllFieldTypes (PersistRational t) = EF t KitchenSinkRt
kitchenSinkChecksAllFieldTypes (PersistInt64 t) = EF t KitchenSinkI64
kitchenSinkChecksAllFieldTypes (PersistBool t) = EF t KitchenSinkBool
kitchenSinkChecksAllFieldTypes (PersistDay t) = EF t KitchenSinkDay
kitchenSinkChecksAllFieldTypes (PersistTimeOfDay t) = EF t KitchenSinkTimeOfDay
kitchenSinkChecksAllFieldTypes (PersistUTCTime t) = EF t KitchenSinkUtcTime
kitchenSinkChecksAllFieldTypes PersistNull = EF Nothing KitchenSinkAge
-- non-normal fields are eyed for removal
-- https://github.com/yesodweb/persistent/issues/1254
kitchenSinkChecksAllFieldTypes (PersistList t) = EF t KitchenSinkList
kitchenSinkChecksAllFieldTypes (PersistMap t) = EF (M.fromList t) KitchenSinkMap
kitchenSinkChecksAllFieldTypes (PersistObjectId t) = EF t KitchenSinkBs
kitchenSinkChecksAllFieldTypes (PersistArray t) = EF t KitchenSinkList
kitchenSinkChecksAllFieldTypes (PersistLiteral_ _ t) = EF t KitchenSinkBs

newtype Arb a = Arb a

instance Show a => Show (Arb a) where
  show (Arb a) = show a

unarb :: Arb a -> a
unarb (Arb a) = a

instance Arbitrary (Arb PersistFilter) where
  arbitrary = Arb <$> elements [Eq, Ne, Gt, Lt, Ge, Le, In, NotIn]

instance Arbitrary Day where
  arbitrary = ModifiedJulianDay <$> arbitrary

instance Arbitrary TimeOfDay where
  arbitrary = TimeOfDay
    <$> choose (0, 23)
    <*> choose (0, 59)
    <*> (fmap MkFixed (choose (0, x)))
    where
    x :: Integer
    x = 61*(10::Integer)^(12::Integer)-1

instance Arbitrary UTCTime where
  arbitrary = UTCTime <$> arbitrary <*> (secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary PersistValue where
  arbitrary = oneof [
    PersistText <$> arbitrary,
    PersistByteString <$> arbitrary,
    PersistDouble <$> arbitrary,
    PersistRational <$> arbitrary,
    PersistInt64 <$> arbitrary,
    PersistBool <$> arbitrary,
    PersistDay <$> arbitrary,
    PersistTimeOfDay <$> arbitrary,
    PersistUTCTime <$> arbitrary,
    return PersistNull,
    PersistList <$> arbitrary,
    PersistMap <$> arbitrary,
    PersistObjectId <$> arbitrary,
    PersistArray <$> arbitrary]

instance Arbitrary (SerializeFilter (Filter KitchenSink)) where
  arbitrary = do
    topAndOr <- arbitrary @Bool
    sink <- arbitrary @(Entity KitchenSink)
    sinks <- resize 5 $ listOf $ arbitrary @(Entity KitchenSink)
    fields <- sublistOf . catMaybes . (parseEntityField @KitchenSink <$>) 
      . getEntityFields . entityDef $ Proxy @KitchenSink
    fieldOps <- (partition (\(_, _, t) -> t)) <$> (traverse 
      (\f -> ((,,) f) 
        <$> (unarb <$> arbitrary @(Arb PersistFilter))
        <*> arbitrary @Bool)
      fields)
    let ors = FilterOr $ mkFilter sink sinks <$> fst fieldOps
    let ands = FilterAnd $ mkFilter sink sinks <$> snd fieldOps
    return . SerializeFilter 
      $ if topAndOr then FilterAnd [ors, ands] else FilterOr [ors, ands]
    where
    mkFilterVal :: Entity KitchenSink -> [Entity KitchenSink]
                -> EntityField KitchenSink typ
                -> PersistFilter -> FilterValue typ
    -- TODO - fix type of filtersArray, dont do dumb crap for the type inference
    mkFilterVal sink sinks field o = case filtersArray . SerializeFilter $ o of
      True -> FilterValues . (getConst . fieldLens field Const <$>) $ sink:sinks
      False -> FilterValue . getConst . fieldLens field Const $ sink
    mkFilter :: Entity KitchenSink -> [Entity KitchenSink] 
             -> (ParsedEntityField KitchenSink, PersistFilter, Bool)
             -> Filter KitchenSink
    mkFilter sink sinks (ParsedEntityField field, o, _) =
      (Filter field (mkFilterVal sink sinks field o) o)

instance Show (SerializeFilter (Filter KitchenSink)) where
  show = BS8.unpack . encode

prop_jsonIsIso
  :: SerializeFilter (Filter KitchenSink) -> Property
prop_jsonIsIso fltr = monadicIO $ do
  ser <- return $ encode fltr
  fltr2 <- unSerializedFilter <$> 
    (either fail return $ eitherDecode @(SerializeFilter (Filter KitchenSink)) $ ser)
  ser2 <- return . encode . SerializeFilter $ fltr2
  return $
    if ser == ser2 then succeeded else failed { 
      reason = BS8.unpack ser <> " /= " <> BS8.unpack ser2 }

main :: IO ()
main = 
  let _ = kitchenSinkChecksAllFieldTypes
  in
    quickCheck prop_jsonIsIso
