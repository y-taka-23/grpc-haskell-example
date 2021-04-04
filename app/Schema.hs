{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE PolyKinds       #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Schema where

import Mu.Quasi.GRpc

grpc "TheSchema" id "route_guide.proto"
