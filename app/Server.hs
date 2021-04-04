{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module Main where

import Mu.GRpc.Server
import Mu.Server

import Schema

main :: IO ()
main = runGRpcApp msgProtoBuf 8080 server

server :: MonadServer m => SingleServerT i RouteGuide m _
server = singleService ()
