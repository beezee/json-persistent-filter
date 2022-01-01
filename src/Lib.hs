{-# LANGUAGE GADTs, FlexibleContexts, FlexibleInstances, LambdaCase #-}
{-# LANGUAGE OverloadedStrings, TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib where

import Control.Arrow
import Data.Aeson
import qualified Data.HashMap.Strict as HM
import Data.Aeson.Types
import qualified Data.Aeson as A
import Data.Bits (shiftL)
import Data.ByteString (ByteString, foldl')
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy.Char8 as LBS8
import Data.List (find)
import qualified Data.List.NonEmpty as NEL
import Data.Proxy
import qualified Data.Scientific as S
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time
import Database.Persist
import qualified Data.Vector as V
import Numeric (showHex)

newtype SerializeFilter a = SerializeFilter a

unSerializedFilter :: SerializeFilter a -> a
unSerializedFilter (SerializeFilter a) = a

array :: [Value] -> Value
array = Array . V.fromList

mergeArrays :: Value -> Value -> Value
mergeArrays (Array a) (Array b) = Array $ a <> b
mergeArrays _ _ = array []

op :: PersistFilter -> String
op Eq = "=="
op Ne = "/="
op Gt = ">"
op Lt = "<"
op Ge = ">="
op Le = "<="
op In = "IN"
op NotIn = "NOT IN"
op _ = "Unserializable backend specific filter"

instance A.ToJSON (SerializeFilter PersistValue) where
  toJSON (SerializeFilter (PersistText t)) = A.String $ t
  toJSON (SerializeFilter (PersistByteString b)) = A.String $ TE.decodeUtf8 $ B64.encode b
  toJSON (SerializeFilter (PersistInt64 i)) = A.Number $ fromIntegral i
  toJSON (SerializeFilter (PersistDouble d)) = A.Number $ S.fromFloatDigits d
  toJSON (SerializeFilter (PersistRational r)) = A.String $ T.pack $ show r
  toJSON (SerializeFilter (PersistBool b)) = A.Bool b
  toJSON (SerializeFilter (PersistTimeOfDay t)) = A.String $ T.pack $ show t
  toJSON (SerializeFilter (PersistUTCTime u)) = A.String $ T.pack $ show u
  toJSON (SerializeFilter (PersistDay d)) = A.String $ T.pack $ show d
  toJSON (SerializeFilter PersistNull) = A.Null
  toJSON (SerializeFilter (PersistList l)) = A.Array $ V.fromList $ map A.toJSON l
  toJSON (SerializeFilter (PersistMap m)) = A.object $ map (second A.toJSON) m
  toJSON (SerializeFilter (PersistLiteral_ _ b)) = 
    A.String $ TE.decodeUtf8 $ B64.encode b
  toJSON (SerializeFilter (PersistArray a)) = A.Array $ V.fromList $ map A.toJSON a
  toJSON (SerializeFilter (PersistObjectId o)) =
    A.toJSON $ showChar 'o' $ showHexLen 8 (bs2i four) $ showHexLen 16 (bs2i eight) ""
    where
     (four, eight) = BS8.splitAt 4 o

     -- taken from crypto-api
     bs2i :: ByteString -> Integer
     bs2i bs = foldl' (\i b -> (i `shiftL` 8) + fromIntegral b) 0 bs
     {-# INLINE bs2i #-}

     -- showHex of n padded with leading zeros if necessary to fill d digits
     -- taken from Data.BSON
     showHexLen :: (Show n, Integral n) => Int -> n -> ShowS
     showHexLen d n = showString (replicate (d - sigDigits n) '0') . showHex n  where
       sigDigits 0 = 1
       sigDigits n' = truncate (logBase (16 :: Double) $ fromIntegral n') + 1

instance PersistEntity a => ToJSON (SerializeFilter (Filter a)) where
  toJSON (SerializeFilter (Filter field (FilterValue val) cmp)) = array [
    object [
      (unFieldNameHS . fieldHaskell . persistFieldDef $ field) .= 
        object [(T.pack . op $ cmp) .= (SerializeFilter . toPersistValue $ val)]]]
  toJSON (SerializeFilter (Filter field (FilterValues vals) cmp)) = array [
    object [(unFieldNameHS . fieldHaskell . persistFieldDef $ field) .= 
      object [(T.pack . op $ cmp) .=
        (array $ (toJSON . SerializeFilter . toPersistValue) <$> vals)]]]
  toJSON (SerializeFilter (Filter field (UnsafeValue _) _)) = object [
    (unFieldNameHS . fieldHaskell . persistFieldDef $ field) .= 
      (toJSON @String "Cannot serialize UnsafeValue")]
  toJSON (SerializeFilter (FilterAnd fs)) = array [
    object [ "AND" .=
      foldl (\a e -> mergeArrays a (toJSON . SerializeFilter $ e)) (array []) fs]]
  toJSON (SerializeFilter (FilterOr fs)) = array [
    object [ "OR" .=
      foldl (\a e -> mergeArrays a (toJSON . SerializeFilter $ e)) (array []) fs]]
  toJSON (SerializeFilter (BackendFilter _)) = array [
    object [ "Unknown" .= (toJSON @String "Cannot serialize BackendFilter")]]

invalidFilterArr :: String -> Value -> Parser (SerializeFilter (Filter a))
invalidFilterArr msg v = fail $ msg <> (LBS8.unpack . encode $ v)

data ParsedEntityField a where
  ParsedEntityField 
    :: (PersistField typ, FromJSON (SerializeFilter typ))
    => EntityField a typ -> ParsedEntityField a

class PersistEntity a => ParseEntityField a where
  parseEntityField :: FieldDef -> Maybe (ParsedEntityField a)

filtersArray :: SerializeFilter PersistFilter -> Bool
filtersArray (SerializeFilter In) = True
filtersArray (SerializeFilter NotIn) = True
filtersArray _ = False

instance Eq (SerializeFilter PersistFilter) where
  (SerializeFilter Eq) == (SerializeFilter Eq) = True
  (SerializeFilter Ne) == (SerializeFilter Ne) = True
  (SerializeFilter Gt) == (SerializeFilter Gt) = True
  (SerializeFilter Lt) == (SerializeFilter Lt) = True
  (SerializeFilter Ge) == (SerializeFilter Ge) = True
  (SerializeFilter Le) == (SerializeFilter Le) = True
  (SerializeFilter In) == (SerializeFilter In) = True
  (SerializeFilter NotIn) == (SerializeFilter NotIn) = True
  _ == _ = False

instance FromJSON (SerializeFilter a) => FromJSON (SerializeFilter (Maybe a)) where
  parseJSON Null = return . SerializeFilter $ Nothing
  parseJSON x = SerializeFilter . Just . unSerializedFilter <$> parseJSON x

parsePersistVal 
  :: PersistField b => (a -> Value) -> (a -> a) -> a -> Parser (SerializeFilter b)
parsePersistVal v m = 
  (either (fail . T.unpack) (return . SerializeFilter) . fromPersistValue =<<) 
    . parseJSON . v . m

instance {-# OVERLAPPABLE #-} (PersistField a, FromJSON a) 
  =>  FromJSON (SerializeFilter a) where
    parseJSON = parsePersistVal id id

instance FromJSON (SerializeFilter ByteString) where
  parseJSON (String s) = parsePersistVal String (T.cons 'b') s
  parseJSON x = parsePersistVal id id x

instance FromJSON (SerializeFilter T.Text) where
  parseJSON (String s) = parsePersistVal String (T.cons 's') s
  parseJSON x = parsePersistVal id id x

instance FromJSON (SerializeFilter String) where
  parseJSON (String s) = parsePersistVal String (T.cons 's') s
  parseJSON x = parsePersistVal id id x

instance FromJSON (SerializeFilter Rational) where
  parseJSON (String s) = parsePersistVal String (T.cons 'r') s
  parseJSON x = parsePersistVal id id x

instance FromJSON (SerializeFilter Day) where
  parseJSON (String s) = parsePersistVal String (T.cons 'd') s
  parseJSON x = parsePersistVal id id x

instance FromJSON (SerializeFilter TimeOfDay) where
  parseJSON (String s) = parsePersistVal String (T.cons 't') s
  parseJSON x = parsePersistVal id id x

instance FromJSON (SerializeFilter UTCTime) where
  parseJSON (String s) = parsePersistVal String (T.cons 'u') s
  parseJSON x = parsePersistVal id id x

instance FromJSON (SerializeFilter PersistFilter) where
  parseJSON (String s) = SerializeFilter <$> case s of
    "==" -> return Eq
    "/=" -> return Ne
    ">" -> return Gt
    "<" -> return Lt
    ">=" -> return Ge
    "<=" -> return Le
    "IN" -> return In
    "NOT IN" -> return NotIn
    _ -> fail . T.unpack $ "Unrecognized persist filter - " <> s
  parseJSON _ = fail "Not a persist filter"

instance (PersistEntity a, ParseEntityField a) => FromJSON (SerializeFilter (Filter a)) where
  parseJSON (Array vs) = V.toList <$> traverse parseJSON vs >>= \case 
    (h:[]) -> return h
    _ -> invalidFilterArr "Cannot parse non-singleton-array value as filter - " $ Array vs
  parseJSON (Object v) = case HM.toList v of
    [("AND", vs)] -> SerializeFilter . FilterAnd 
      . (unSerializedFilter <$>) <$> parseJSON vs
    [("OR", vs)] -> SerializeFilter . FilterOr 
      . (unSerializedFilter <$>) <$> parseJSON vs
    [(key, o)] -> case (find ((== key) . snd) knownFields) of
      (Just (fd, fn)) -> case parseEntityField @a fd of
        (Just ef) -> case ef of
          ParsedEntityField f -> case o of
            (Object ov) -> case HM.toList ov of
              [(cmp, val)] -> do 
                cmpp <- parseJSON . String $ cmp
                pv <- if filtersArray cmpp
                  then FilterValues . (unSerializedFilter <$>)
                    <$> parseJSON val <?> (Key (key <> "." <> cmp))
                  else FilterValue . unSerializedFilter 
                    <$> parseJSON val <?> (Key (key <> "." <> cmp))
                return . SerializeFilter $ Filter f pv (unSerializedFilter cmpp)
              _ -> (invalidFilterArr 
                "Invalid filter object serialization format - " $ (Object ov))
            _ -> (invalidFilterArr 
              "Invalid filter object serialization format - " $ o)
        Nothing ->
          invalidFilterArr
            ("Persistent error - Specified field name " <> T.unpack fn 
              <> " does not map to an EntityField. "
              <> "Check your PersistEntity implementation") (Object v)
      Nothing -> (invalidFilterArr 
        ("Unknown key " <> T.unpack key <> " from known keys " 
          <> (show . (snd <$>) $ knownFields))
        o)
    _ -> invalidFilterArr "Invalid filter object serialization format - " (Object v)
    where
      knownFields :: [(FieldDef, T.Text)]
      knownFields = 
        NEL.toList 
          $ (id &&& unFieldNameHS . fieldHaskell) 
          <$> (keyAndEntityFields $ entityDef (Proxy @a))
  parseJSON v = invalidFilterArr "Cannot parse non-singleton-array value as filter - " v

