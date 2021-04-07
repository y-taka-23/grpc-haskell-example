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
import           Control.Monad          ( when )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import           Control.Monad.Reader   ( MonadReader, ask, runReaderT )
import           Data.Aeson             ( eitherDecodeStrict' )
import qualified Data.ByteString        as B
import           Data.Conduit           ( ConduitT, Void, runConduit, (.|) )
import qualified Data.Conduit.List      as CL
import           Mu.GRpc.Server         ( msgProtoBuf, runGRpcAppTrans )
import           Mu.Server
    ( MonadServer
    , SingleServerT
    , method
    , singleService
    )
import           Prelude                hiding ( readFile )
import           System.Directory       ( doesFileExist )
import           System.Exit            ( die )

import Schema as S

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
    )

getFeature
    :: (MonadServer m, MonadReader [S.Feature] m)
    => S.Point -> m S.Feature
getFeature p = do
    fs <- ask
    case filter ((== Just p) . location) fs of
        []  -> pure $ Feature "" (Just p)
        f:_ -> pure f

listFeatures
    :: (MonadServer m, MonadReader [S.Feature] m)
    => S.Rectangle -> ConduitT Feature Void m () -> m ()
listFeatures r sink = do
    fs <- ask
    runConduit $ CL.sourceList fs .| CL.filter inRectangle .| sink
        where
            inRectangle f = case location f of
                Nothing -> False
                Just p  -> inRange r p

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
inRange r p = case (lo r, hi r) of
    (Just l, Just h) ->
        left <= longitude p && longitude p <= right &&
        bottom <= latitude p  && latitude p <= top
            where
                left = longitude l `min` longitude h
                right = longitude l `max` longitude h
                bottom = latitude l `min` latitude h
                top = latitude l `max` latitude h
    (_, _) -> False
