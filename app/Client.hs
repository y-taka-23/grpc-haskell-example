{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import           Data.Conduit             ( ConduitT, runConduit, (.|) )
import qualified Data.Conduit.Combinators as CC
import           Mu.GRpc.Client.TyApps
    ( GRpcMessageProtocol(MsgProtoBuf)
    , GRpcReply(GRpcOk)
    , GrpcClient
    , gRpcCall
    , grpcClientConfigSimple
    , setupGrpcClient'
    )
import           System.Exit              ( die )

import qualified RouteGuide.Schema as S

main :: IO ()
main = do
    let config = grpcClientConfigSimple "127.0.0.1" 10000 False
    setup <- setupGrpcClient' config
    case setup of
        Left err -> die $ show err
        Right client -> do
            printFeature client $ S.Point 409146138 (-746188906)
            printFeature client $ S.Point 0 0
            printFeatures client $ S.Rectangle
                (Just (S.Point 400000000 (-750000000)))
                (Just (S.Point 420000000 (-730000000)))

printFeature :: GrpcClient -> S.Point -> IO ()
printFeature client p = do
    putStrLn $ "Getting feature for " ++ show p
    printGRpcReply =<< getFeature client p

printFeatures :: GrpcClient -> S.Rectangle -> IO ()
printFeatures client r = do
    putStrLn $ "Looking for features within " ++ show r
    responses <- listFeatures client r
    runConduit $ responses .| CC.mapM_ printGRpcReply

getFeature :: GrpcClient -> S.Point -> IO (GRpcReply S.Feature)
getFeature = gRpcCall @'MsgProtoBuf @S.RouteGuide @"RouteGuide" @"GetFeature"

listFeatures :: GrpcClient -> S.Rectangle -> IO (ConduitT () (GRpcReply S.Feature) IO ())
listFeatures = gRpcCall @'MsgProtoBuf @S.RouteGuide @"RouteGuide" @"ListFeatures"

printGRpcReply :: (Show a) => GRpcReply a -> IO ()
printGRpcReply reply = case reply of
    GRpcOk item -> putStrLn $ show item
    err         -> die $ show err
