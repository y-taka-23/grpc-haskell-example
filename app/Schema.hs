{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Schema where

import Data.Aeson   ( FromJSON )
import Data.Text    as T
import GHC.Generics ( Generic )
import GHC.Int      ( Int32 )

import Mu.Quasi.GRpc ( grpc )
import Mu.Schema     ( FromSchema, ToSchema )

grpc "TheSchema" id "route_guide.proto"

data Point = Point {
      latitude  :: Int32
    , longitude :: Int32
    } deriving (
      Eq, Show, Generic
    , ToSchema   TheSchema "Point"
    , FromSchema TheSchema "Point"
    )

instance FromJSON Point

data Feature = Feature {
      name     :: T.Text
    , location :: Maybe Point
    } deriving (
      Eq, Show, Generic
    , ToSchema   TheSchema "Feature"
    , FromSchema TheSchema "Feature"
    )

instance FromJSON Feature
