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
module Main where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Database.Persist
import Database.Persist.Sql
import Database.Persist.TH
import Lib

share [mkPersist sqlSettings] [persistLowerCase|
Person
    name String
    age Int Maybe
    deriving Show
|]

instance ParseEntityField Person where
  parseEntityField def = case unFieldNameHS . fieldHaskell $ def of
    "name" -> Just . ParsedEntityField $ PersonName
    "age" -> Just . ParsedEntityField $ PersonAge
    _ -> Nothing

main :: IO ()
main = do
  let 
    fltr = FilterOr [
      PersonName /<-. ["foo", "bar"],
      --PersonName ==. "foo",
      FilterAnd [PersonAge <. Just 40, PersonAge >. Just 30]]
    encoded = encode . toJSON . SerializeFilter $ fltr
  _ <- putStrLn . BS.unpack $ encoded
  putStrLn . either id id . (BS.unpack . encode . toJSON 
    . SerializeFilter
    . unSerializedFilter <$>)
    . eitherDecode @(SerializeFilter (Filter Person)) $ encoded
