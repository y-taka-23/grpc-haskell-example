{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE DerivingVia     #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Schema where

import Data.Aeson   ( FromJSON )
import Data.Text    as T
import GHC.Generics ( Generic )
import GHC.Int      ( Int32 )

import Mu.Quasi.GRpc ( grpc )
import Mu.Schema
    ( CustomFieldMapping(CustomFieldMapping)
    , FromSchema
    , Mapping((:->))
    , ToSchema
    )

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

data Rectangle = Rectangle {
      lo :: Maybe Point
    , hi :: Maybe Point
    } deriving (
      Eq, Show, Generic
    , ToSchema   TheSchema "Rectangle"
    , FromSchema TheSchema "Rectangle"
    )

data Feature = Feature {
      name     :: T.Text
    , location :: Maybe Point
    } deriving (
      Eq, Show, Generic
    , ToSchema   TheSchema "Feature"
    , FromSchema TheSchema "Feature"
    )

instance FromJSON Feature

data RouteSummary = RouteSummary {
      pointCount   :: Int32
    , featureCount :: Int32
    , distance     :: Int32
    , elapsedTime  :: Int32
    } deriving (
      Eq, Show, Generic
    ) deriving (
      ToSchema   TheSchema "RouteSummary"
    , FromSchema TheSchema "RouteSummary"
    ) via (
      CustomFieldMapping "RouteSummary" RouteSummaryFieldMapping RouteSummary
    )

type RouteSummaryFieldMapping = '[
      "pointCount" ':-> "point_count"
    , "featureCount" ':-> "feature_count"
    , "elapsedTime" ':-> "elapsed_time"
    ]
