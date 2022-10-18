{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Primitive.Tx.Byron
    ( fromTxAux
    , fromTxIn
    , fromTxOut
    )
where

import Cardano.Binary
    ( serialize'
    )
import Cardano.Chain.Common
    ( unsafeGetLovelace
    )
import Cardano.Chain.UTxO
    ( ATxAux (..)
    , Tx (..)
    , TxIn (..)
    , TxOut (..)
    , taTx
    )
import Cardano.Crypto.Hashing qualified as CC
import Cardano.Wallet.Primitive.Types.Address qualified as W
import Cardano.Wallet.Primitive.Types.Coin qualified as Coin
import Cardano.Wallet.Primitive.Types.Coin qualified as W
import Cardano.Wallet.Primitive.Types.Hash qualified as W
import Cardano.Wallet.Primitive.Types.TokenBundle qualified as TokenBundle
import Cardano.Wallet.Primitive.Types.Tx qualified as W
import Cardano.Wallet.Read.Eras
    ( byron
    , inject
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR
    )
import Cardano.Wallet.Read.Tx.Hash
    ( byronTxHash
    )
import Data.List.NonEmpty qualified as NE
import Prelude

fromTxAux :: ATxAux a -> W.Tx
fromTxAux txAux = case taTx txAux of
    UnsafeTx inputs outputs _attributes ->
        W.Tx
            { txId = W.Hash $ byronTxHash txAux
            , txCBOR = Just $ renderTxToCBOR $ inject byron $ Tx $ () <$ txAux
            , fee = Nothing
            , -- TODO: Review 'W.Tx' to not require resolved inputs but only inputs
              resolvedInputs =
                (,W.Coin 0) . fromTxIn <$> NE.toList inputs
            , resolvedCollateralInputs = []
            , outputs =
                fromTxOut <$> NE.toList outputs
            , collateralOutput =
                Nothing
            , withdrawals =
                mempty
            , metadata =
                Nothing
            , scriptValidity =
                Nothing
            }

fromTxIn :: TxIn -> W.TxIn
fromTxIn (TxInUtxo id_ ix) =
    W.TxIn
        { inputId = W.Hash $ CC.hashToBytes id_
        , inputIx = fromIntegral ix
        }

fromTxOut :: TxOut -> W.TxOut
fromTxOut (TxOut addr coin) =
    W.TxOut
        { address = W.Address (serialize' addr)
        , tokens = TokenBundle.fromCoin $ Coin.fromWord64 $ unsafeGetLovelace coin
        }
