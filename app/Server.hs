{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main (main) where

import           Control.Concurrent.STM
    ( TVar
    , atomically
    , newTVar
    , readTVar
    , writeTVar
    )
import           Control.Exception.Safe
    ( MonadThrow
    , SomeException
    , bracket
    , catch
    , displayException
    , throwString
    )
import           Control.Monad               ( when )
import           Control.Monad.IO.Class      ( liftIO )
import           Control.Monad.Reader        ( MonadReader, ask, runReaderT )
import           Data.Aeson                  ( eitherDecodeStrict' )
import qualified Data.ByteString             as B
import           Data.Conduit
    ( ConduitT
    , Void
    , runConduit
    , (.|)
    )
import qualified Data.Conduit.Combinators    as CC
import qualified Data.Conduit.List           as CL
import           Data.List                   ( find )
import qualified Data.Map                    as M
import           Data.Maybe                  ( isJust )
import           Data.String                 ( fromString )
import           Data.Time
    ( diffUTCTime
    , getCurrentTime
    , nominalDiffTimeToSeconds
    )
import           Mu.GRpc.Server
    ( gRpcAppTrans
    , msgProtoBuf
    , runGRpcAppSettings
    , runGRpcAppTLS
    )
import           Mu.Server
    ( MonadServer
    , SingleServerT
    , method
    , singleService
    )
import           Network.Socket
    ( Family(AF_UNIX)
    , SockAddr(SockAddrUnix)
    , Socket
    , SocketType(Stream)
    , bind
    , close
    , getSocketName
    , listen
    , maxListenQueue
    , socket
    )
import           Network.Wai.Handler.Warp
    ( defaultSettings
    , runSettingsSocket
    , setHost
    , setPort
    )
import           Network.Wai.Handler.WarpTLS ( TLSSettings, tlsSettingsMemory )
import           Options.Applicative
    ( Parser
    , auto
    , execParser
    , help
    , helper
    , idm
    , info
    , long
    , metavar
    , option
    , strOption
    , switch
    , value
    )
import           Prelude                     hiding ( readFile )
import           System.Exit                 ( die )
import           System.Posix                ( fileExist, removeLink )

import qualified RouteGuide.Schema as S

main :: IO ()
main = runServer =<< execParser parser
    where
        parser = info (helper <*> options) idm

runServer :: Options -> IO ()
runServer opts = do
    let fatal =  \(e :: SomeException) -> die $ displayException e
    fs <- loadFeatures (jsonDBFile opts) `catch` fatal
    nsv <- atomically $ newTVar M.empty
    let settings = setHost (fromString $ host opts) $ setPort (port opts) $ defaultSettings
    let trans = flip runReaderT (Server fs nsv)
    if unix opts /= "" then do
        let runSocket st app sock = runSettingsSocket st sock app
        bracket (createSocket $ unix opts) removeSocket $
            runSocket settings (gRpcAppTrans msgProtoBuf trans server)
    else if tls opts then do
        cred <- loadTLSSettings (certFile opts) (keyFile opts) `catch` fatal
        runGRpcAppTLS msgProtoBuf cred settings trans server
    else do
        runGRpcAppSettings msgProtoBuf settings trans server

data Options = Options {
      tls        :: Bool
    , certFile   :: FilePath
    , keyFile    :: FilePath
    , jsonDBFile :: FilePath
    , host       :: String
    , port       :: Int
    , unix       :: FilePath
    }

options :: Parser Options
options = Options <$>
        switch (
               long "tls"
            <> help "Connection uses TLS if true, else plain TCP")
    <*> strOption (
               long "cert_file" <> metavar "string" <> value ""
            <> help "The TLS cert file")
    <*> strOption (
               long "key_file" <> metavar "string" <> value ""
            <> help "The TLS key file")
    <*> strOption (
               long "json_db_file" <>  metavar "string" <> value "route_guide_db.json"
            <> help "A json file containing a list of features")
    <*> strOption (
               long "host" <>  metavar "string" <> value "localhost"
            <> help "The server host")
    <*> option auto (
               long "port" <>  metavar "int" <> value 10000
            <> help "The server port")
    <*> strOption (
               long "unix" <>  metavar "string" <> value ""
            <> help "The unix domain socket")

server
    :: (MonadServer m, MonadReader Server m)
    => SingleServerT i S.RouteGuide m _
server = singleService (
      method @"GetFeature" getFeature
    , method @"ListFeatures" listFeatures
    , method @"RecordRoute" recordRoute
    , method @"RouteChat" routeChat
    )

type Features = [S.Feature]
type RouteNotes = M.Map S.Point [S.RouteNote]

data Server = Server {
      savedFeatures :: Features
    , routeNotesVar :: TVar RouteNotes
    }

getFeature
    :: (MonadServer m, MonadReader Server m)
    => S.Point -> m S.Feature
getFeature p = do
    fs <- savedFeatures <$> ask
    case filter ((== Just p) . S.location) fs of
        []  -> pure $ S.Feature (fromString "") (Just p)
        f:_ -> pure f

listFeatures
    :: (MonadServer m, MonadReader Server m)
    => S.Rectangle -> ConduitT S.Feature Void m () -> m ()
listFeatures r sink = do
    fs <- savedFeatures <$> ask
    runConduit $ CL.sourceList fs .| CL.filter inRectangle .| sink
        where
            inRectangle f = case S.location f of
                Nothing -> False
                Just p  -> inRange r p

recordRoute
    :: (MonadServer m, MonadReader Server m)
    => ConduitT () S.Point m () -> m S.RouteSummary
recordRoute src = do
    fs <- savedFeatures <$> ask
    let start = S.Point 0 0
        countFc p = if inFeatures fs p then 1 else 0
        record (pc, fc, d, lastP) p =
            (pc + 1, fc + countFc p, d + calcDistance lastP p, p)
    t0 <- liftIO getCurrentTime
    (pc, fc, d, _) <- runConduit $ src .| CC.foldl record (0, 0, 0, start)
    t1 <- liftIO getCurrentTime
    let et = round $ nominalDiffTimeToSeconds $ diffUTCTime t1 t0
    pure $ S.RouteSummary pc fc d et

routeChat
    :: (MonadServer m, MonadReader Server m)
    => ConduitT () S.RouteNote m () -> ConduitT S.RouteNote Void m () -> m ()
routeChat src sink = do
    nsv <- routeNotesVar <$> ask
    runConduit $ src .| CL.concatMapM (checkin nsv) .| sink
        where
            checkin nsv n = do
                appended <- liftIO $ atomically $ do
                    prev <- readTVar nsv
                    let (ns, updated) = appendNote n prev
                    writeTVar nsv updated
                    pure ns
                pure $ reverse appended

createSocket :: FilePath -> IO Socket
createSocket fp = do
    removeIfExists fp
    sock <- socket AF_UNIX Stream 0
    bind sock $ SockAddrUnix fp
    listen sock maxListenQueue
    pure sock

removeSocket :: Socket -> IO ()
removeSocket sock = do
    nm <- getSocketName sock
    close sock
    case nm of
        SockAddrUnix fp -> removeIfExists fp
        _               -> pure ()

removeIfExists :: FilePath -> IO ()
removeIfExists fp = do
    exists <- fileExist fp
    when exists $ removeLink fp

loadFeatures :: FilePath -> IO Features
loadFeatures fp = do
    readFile "json db file" fp >>= decodeFeatures

loadTLSSettings :: FilePath -> FilePath -> IO TLSSettings
loadTLSSettings cert key =
    tlsSettingsMemory <$> readFile "cert file" cert <*> readFile "key file" key

readFile :: String -> FilePath -> IO B.ByteString
readFile desc fp = do
    exists <- fileExist fp
    when (not exists) $ throwString $ desc ++ " not found: " ++ show fp
    B.readFile fp

decodeFeatures
    :: (MonadThrow m)
    => B.ByteString -> m Features
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

inFeatures :: Features -> S.Point -> Bool
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

appendNote :: S.RouteNote -> RouteNotes -> ([S.RouteNote], RouteNotes)
appendNote n prev = case S.noteLocation n of
    Nothing -> ([], prev)
    Just p  -> case M.lookup p prev of
        Nothing -> ([n], M.insert p [n] prev)
        Just ns -> (n:ns, M.insert p (n:ns) prev)
