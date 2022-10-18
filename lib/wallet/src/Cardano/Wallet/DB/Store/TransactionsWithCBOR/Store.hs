{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- |
--Copyright: 2022 IOHK
--License: Apache-2.0
--
--Implementation of a 'Store' for 'TxHistoryWithCBOR'.
module Cardano.Wallet.DB.Store.TransactionsWithCBOR.Store where

import Cardano.Wallet.DB.Store.CBOR.Model qualified as CBOR
import Cardano.Wallet.DB.Store.CBOR.Store
    ( mkStoreCBOR
    )
import Cardano.Wallet.DB.Store.Transactions.Model qualified as Txs
import Cardano.Wallet.DB.Store.Transactions.Store
    ( mkStoreTransactions
    )
import Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model
    ( DeltaTx (Append, DeleteTx)
    , TxHistoryWithCBOR (TxHistoryWithCBOR)
    )
import Cardano.Wallet.DB.Store.TransactionsWithCBOR.Model qualified as WithCBOR
import Control.Monad.Except
    ( ExceptT (..)
    , runExceptT
    )
import Data.DBVar
    ( Store (..)
    )
import Database.Persist.Sql
    ( SqlPersistT
    )
import Prelude

mkStoreTransactionsWithCBOR
    :: Store (SqlPersistT IO) WithCBOR.DeltaTx
mkStoreTransactionsWithCBOR =
    Store
        { loadS =
            runExceptT $
                TxHistoryWithCBOR
                    <$> ExceptT (loadS mkStoreTransactions)
                    <*> ExceptT (loadS mkStoreCBOR)
        , writeS = \(TxHistoryWithCBOR txs cbors) -> do
            writeS mkStoreTransactions txs
            writeS mkStoreCBOR cbors
        , updateS = \(TxHistoryWithCBOR oldtxs oldcbors) -> \case
            Append (TxHistoryWithCBOR newtxs newcbors) -> do
                updateS mkStoreTransactions oldtxs (Txs.Append newtxs)
                updateS mkStoreCBOR oldcbors (CBOR.Append newcbors)
            DeleteTx tid -> do
                updateS mkStoreTransactions oldtxs (Txs.DeleteTx tid)
                updateS mkStoreCBOR oldcbors (CBOR.DeleteTx tid)
        }
