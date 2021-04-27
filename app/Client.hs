{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Mu.GRpc.Client.TyApps
    ( GRpcMessageProtocol(MsgProtoBuf)
    , GRpcReply(GRpcOk)
    , GrpcClient
    , gRpcCall
    , grpcClientConfigSimple
    , setupGrpcClient'
    )
import System.Exit           ( die )

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

printFeature :: GrpcClient -> S.Point -> IO ()
printFeature client p = do
    putStrLn $ "Getting feature for " ++ show p
    response <- getFeature client p
    case response of
        GRpcOk feature -> putStrLn $ show feature
        err            -> die $ show err

getFeature :: GrpcClient -> S.Point -> IO (GRpcReply S.Feature)
getFeature = gRpcCall @'MsgProtoBuf @S.RouteGuide @"RouteGuide" @"GetFeature"
