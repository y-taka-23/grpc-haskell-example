{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Data.Conduit             ( ConduitT, Void, runConduit, (.|) )
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List        as CL
import qualified Data.List                as L
import           Mu.GRpc.Client.TyApps
    ( CompressMode(Compressed)
    , GRpcMessageProtocol(MsgProtoBuf)
    , GRpcReply(GRpcOk)
    , GrpcClient
    , gRpcCall
    , grpcClientConfigSimple
    , setupGrpcClient'
    )
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
import           System.Exit              ( die )
import           System.Random
    ( RandomGen
    , getStdGen
    , getStdRandom
    , randomR
    )

import qualified RouteGuide.Schema as S

main :: IO ()
main = do
    runClient =<< execParser parser
        where
            parser = info (helper <*> options) idm

runClient :: Options -> IO ()
runClient opts = do
    let config = grpcClientConfigSimple (host opts) (fromIntegral $ port opts) (tls opts)
    setup <- setupGrpcClient' config
    case setup of
        Left err -> die $ show err
        Right client -> do
            printFeature client $ S.Point 409146138 (-746188906)
            printFeature client $ S.Point 0 0
            printFeatures client $ S.Rectangle
                (Just (S.Point 400000000 (-750000000)))
                (Just (S.Point 420000000 (-730000000)))
            runRecordRoute client

data Options = Options {
      tls  :: Bool
    , host :: String
    , port :: Int
    }

options :: Parser Options
options = Options <$>
        switch (
               long "tls"
            <> help "Connection uses TLS if true, else plain TCP")
    <*> strOption (
               long "host" <>  metavar "string" <> value "localhost"
            <> help "The server host")
    <*> option auto (
               long "port" <>  metavar "int" <> value 10000
            <> help "The server port")

printFeature :: GrpcClient -> S.Point -> IO ()
printFeature client p = do
    putStrLn $ "Getting feature for " ++ show p
    printGRpcReply =<< getFeature client p

printFeatures :: GrpcClient -> S.Rectangle -> IO ()
printFeatures client r = do
    putStrLn $ "Looking for features within " ++ show r
    src <- listFeatures client r
    runConduit $ src .| CC.mapM_ printGRpcReply

runRecordRoute :: GrpcClient -> IO ()
runRecordRoute client = do
    pointCount <- getStdRandom $ randomR (2, 101)
    ps <- take pointCount <$> randomPoints <$> getStdGen
    putStrLn $ "Traversing " ++ show (length ps) ++ " points."
    sink <- recordRoute client
    reply <- runConduit $ CL.sourceList ps .| sink
    putStr "Route Summary: " >> printGRpcReply reply

getFeature :: GrpcClient -> S.Point -> IO (GRpcReply S.Feature)
getFeature = gRpcCall @'MsgProtoBuf @S.RouteGuide @"RouteGuide" @"GetFeature"

listFeatures :: GrpcClient -> S.Rectangle -> IO (ConduitT () (GRpcReply S.Feature) IO ())
listFeatures = gRpcCall @'MsgProtoBuf @S.RouteGuide @"RouteGuide" @"ListFeatures"

recordRoute :: GrpcClient -> IO (ConduitT S.Point Void IO (GRpcReply S.RouteSummary))
recordRoute client =
    gRpcCall @'MsgProtoBuf @S.RouteGuide @"RouteGuide" @"RecordRoute" client Compressed

printGRpcReply :: (Show a) => GRpcReply a -> IO ()
printGRpcReply reply = case reply of
    GRpcOk item -> putStrLn $ show item
    err         -> die $ show err

randomPoint :: (RandomGen g) => g -> (S.Point, g)
randomPoint gen =
    let (lat, gen1) = randomR (0, 179) gen
        (lng, gen2) = randomR (0, 359) gen1
    in (S.Point ((lat - 90) * 10000000) ((lng - 180) * 10000000), gen2)

randomPoints :: (RandomGen g) => g -> [S.Point]
randomPoints = L.unfoldr (Just . randomPoint)
