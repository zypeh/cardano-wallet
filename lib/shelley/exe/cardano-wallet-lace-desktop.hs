{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Copyright: © 2018-2020 IOHK
-- License: Apache-2.0
--
-- This module parses command line arguments for the wallet and executes
-- corresponding commands.
--
-- In essence, it's a proxy to the wallet server, which is required for most
-- commands. Commands are turned into corresponding API calls, and submitted
-- to an up-and-running server. Some commands do not require an active server
-- and can be run "offline".

module Main where

import Pinch
import Prelude

import Cardano.Wallet.Primitive.Types
    ( BlockHeader (..), SlotNo (..) )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Control.Concurrent
    ( forkFinally )
import Control.Exception
    ( bracket, bracketOnError )
import Control.Monad
    ( forever )
import Data.Bits
    ( Bits (shiftR, xor, (.&.)) )
import Data.Functor
    ( void )
import Data.Int
    ( Int64 )
import Data.IntCast
    ( intCast )
import Data.Quantity
    ( Quantity (Quantity) )
import Data.Word
    ( Word32, Word64 )
import Network.Socket
    ( AddrInfo (addrAddress, addrFlags, addrSocketType)
    , AddrInfoFlag (AI_PASSIVE)
    , SocketOption (ReuseAddr)
    , SocketType (Stream)
    , accept
    , bind
    , close
    , defaultHints
    , getAddrInfo
    , gracefulClose
    , listen
    , openSocket
    , setCloseOnExecIfNeeded
    , setSocketOption
    , withFdSocket
    )
import Pinch.Client
    ( ThriftCall (..), call, client )
import Pinch.Server
    ( Handler (..), ThriftServer, createChannel, createServer, runConnection )
import Pinch.Transport
    ( framedTransport )


main :: IO ()
main = do
    addr <- resolve
    print addr
    bracket (open addr) close loop
  where
    resolve = do
        let hints = defaultHints
                { addrFlags = [AI_PASSIVE]
                , addrSocketType = Stream
                }
        head <$> getAddrInfo (Just hints) (Just "localhost") (Just "8888")
    open addr = bracketOnError (openSocket addr) close $ \sock -> do
        setSocketOption sock ReuseAddr 1
        withFdSocket sock setCloseOnExecIfNeeded
        bind sock $ addrAddress addr
        listen sock 1024
        pure sock
    loop sock = forever $ bracketOnError (accept sock) (close . fst)
        $ \(conn, _peer) -> void $
            forkFinally (server conn) (const $ gracefulClose conn 5000)
    server so = do
        let context = mempty
        channel <- createChannel so framedTransport binaryProtocol
        runConnection context thriftServer channel
        let cl = client channel
        blockHeader :: BlockHeader <- call cl (TCall "latestBlockHeader" NoArgs)
        print blockHeader

data NoArgs = NoArgs

instance Pinchable NoArgs where
   type Tag NoArgs = TStruct
   pinch _ = struct []
   unpinch _ = pure NoArgs

thriftServer :: ThriftServer
thriftServer = createServer \case
    "latestBlockHeader" ->
        Just $ CallHandler \_ctx (arg :: Value TStruct) -> do
            putStrLn "returning latest block header"
            pure mockBlockHeader
    _ -> Nothing
  where
    mockBlockHeader =
        BlockHeader
            { slotNo =
                SlotNo 158228
            , blockHeight =
                Quantity 7609464
            , headerHash =
                Hash "751ecc1a7fd63a508fe3d4d8b2872e93\
                        \d70404cc3c8b5bd7ddcdb30549885b48"
            , parentHeaderHash =
                Just $ Hash "f41a8508fc2c95712be2bd56ed343c73\
                            \1b405ee28da3b4740c8f59a5369bf5a8"
            }

instance Pinchable BlockHeader where
    type Tag BlockHeader = TStruct
    pinch BlockHeader{..} =
        struct
            [ 1 .= slotNo
            , 2 .= blockHeight
            , 3 .= headerHash
            , 4 ?= parentHeaderHash
            ]
    unpinch value =
        BlockHeader
            <$> value .: 1
            <*> value .: 2
            <*> value .: 3
            <*> value .:? 4

instance Pinchable SlotNo where
    type Tag SlotNo = TInt64
    pinch (SlotNo no) = pinch $ wordToSignedInt64 no
      where
        wordToSignedInt64 :: Word64 -> Int64
        wordToSignedInt64 n
            = fromIntegral (shiftR n 1) `xor` negate (fromIntegral $ n .&. 1)
    unpinch value = SlotNo . fromIntegral @Int64 <$> unpinch value

instance Pinchable (Quantity tag Word32) where
    type Tag (Quantity tag Word32) = TInt64
    pinch (Quantity n) = pinch (intCast @Word32 @Int64 n)
    unpinch value = Quantity . fromIntegral @Int64 <$> unpinch value

instance Pinchable (Hash "BlockHeader") where
    type Tag (Hash "BlockHeader") = TBinary
    pinch (Hash bs) = pinch bs
    unpinch value = Hash <$> unpinch value
