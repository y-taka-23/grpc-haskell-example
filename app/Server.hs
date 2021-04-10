{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main (main) where

import           Control.Exception.Safe
    ( MonadThrow
    , SomeException
    , catch
    , displayException
    , throwString
    )
import           Control.Monad            ( when )
import           Control.Monad.IO.Class   ( MonadIO, liftIO )
import           Control.Monad.Reader     ( MonadReader, ask, runReaderT )
import           Data.Aeson               ( eitherDecodeStrict' )
import qualified Data.ByteString          as B
import           Data.Conduit             ( ConduitT, Void, runConduit, (.|) )
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List        as CL
import           Data.List                ( find )
import           Data.Maybe               ( isJust )
import           Data.Time
    ( diffUTCTime
    , getCurrentTime
    , nominalDiffTimeToSeconds
    )
import           Mu.GRpc.Server           ( msgProtoBuf, runGRpcAppTrans )
import           Mu.Server
    ( MonadServer
    , SingleServerT
    , method
    , singleService
    )
import           Prelude                  hiding ( readFile )
import           System.Directory         ( doesFileExist )
import           System.Exit              ( die )

import qualified Schema as S

main :: IO ()
main = do
    savedFeatures <- loadFeatures "route_guide_db.json" `catch` fatal
    runGRpcAppTrans msgProtoBuf 10000 (flip runReaderT savedFeatures) server
        where
            fatal = (\(e :: SomeException) -> die $ displayException e)

server
    :: (MonadServer m, MonadReader [S.Feature] m)
    => SingleServerT i S.RouteGuide m _
server = singleService (
      method @"GetFeature" getFeature
    , method @"ListFeatures" listFeatures
    , method @"RecordRoute" recordRoute
    )

getFeature
    :: (MonadServer m, MonadReader [S.Feature] m)
    => S.Point -> m S.Feature
getFeature p = do
    fs <- ask
    case filter ((== Just p) . S.location) fs of
        []  -> pure $ S.Feature "" (Just p)
        f:_ -> pure f

listFeatures
    :: (MonadServer m, MonadReader [S.Feature] m)
    => S.Rectangle -> ConduitT S.Feature Void m () -> m ()
listFeatures r sink = do
    fs <- ask
    runConduit $ CL.sourceList fs .| CL.filter inRectangle .| sink
        where
            inRectangle f = case S.location f of
                Nothing -> False
                Just p  -> inRange r p

recordRoute
    :: (MonadServer m, MonadReader [S.Feature] m, MonadIO m)
    => ConduitT () S.Point m () -> m S.RouteSummary
recordRoute src = do
    t0 <- liftIO getCurrentTime
    fs <- ask
    let start = S.Point 0 0
        countFc p = if inFeatures fs p then 1 else 0
        record (pc, fc, d, lastP) p =
            (pc + 1, fc + countFc p, d + calcDistance lastP p, p)
    (pc, fc, d, _) <- runConduit $ src .| CC.foldl record (0, 0, 0, start)
    t1 <- liftIO getCurrentTime
    let et = round $ nominalDiffTimeToSeconds $ diffUTCTime t1 t0
    pure $ S.RouteSummary pc fc d et

loadFeatures
    :: (MonadThrow m, MonadIO m)
    => FilePath -> m [S.Feature]
loadFeatures fp = readFile fp >>= decodeFeatures

readFile
    :: (MonadThrow m, MonadIO m)
    => FilePath -> m B.ByteString
readFile fp = do
    ok <- liftIO $ doesFileExist fp
    when (not ok) $ throwString $ "failed to load default features: " ++ fp
    liftIO $ B.readFile fp

decodeFeatures
    :: (MonadThrow m)
    => B.ByteString -> m [S.Feature]
decodeFeatures bs = case eitherDecodeStrict' bs of
    Left err -> throwString $ "failed to load default features: " ++ err
    Right fs -> pure fs

inRange :: S.Rectangle -> S.Point -> Bool
inRange r p = case (S.lo r, S.hi r) of
    (Just l, Just h) ->
        left <= S.longitude p && S.longitude p <= right &&
        bottom <= S.latitude p  && S.latitude p <= top
            where
                left = S.longitude l `min` S.longitude h
                right = S.longitude l `max` S.longitude h
                bottom = S.latitude l `min` S.latitude h
                top = S.latitude l `max` S.latitude h
    (_, _) -> False

inFeatures :: [S.Feature] -> S.Point -> Bool
inFeatures fs p = isJust $ find ((==) (Just p) . S.location) fs

calcDistance :: (Integral i) => S.Point -> S.Point -> i
calcDistance lastP p = round $ radius * central
    where
        coordToRadian d = (fromIntegral d * pi) / (180 * 10000000) :: Double
        lat1 = coordToRadian $ S.latitude lastP
        lat2 = coordToRadian $ S.latitude p
        lng1 = coordToRadian $ S.longitude lastP
        lng2 = coordToRadian $ S.longitude p
        (dlat, dlng) = (lat2 - lat1, lng2 - lng1)
        central = 2 * (asin $ sqrt $
            sin (dlat/2) * sin (dlat/2) +
            cos lat1 * cos lat2 * sin (dlng/2) * sin (dlng/2))
        radius = 6371000
