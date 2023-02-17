{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Access to the submissions store.

module Cardano.Wallet.DB.Store.Submissions.Layer
    ( mkLocalTxSubmission
    , emptyTxSubmissions_
    , resubmitTx_
    , getInSubmissionTransactions_
    , readLocalTxSubmissionPending_
    , rollForwardTxSubmissions_
    , removePendingOrExpiredTx_
    , rollBackSubmissions_
    , pruneByFinality_
    , addTxSubmission_
    , getInSubmissionTransaction_
    )
    where

import Prelude hiding
    ( (.) )

import Cardano.Wallet.DB.Errors
    ( ErrNoSuchTransaction (..), ErrRemoveTx (..) )
import Cardano.Wallet.DB.Sqlite.Types
    ( TxId (..) )
import Cardano.Wallet.DB.Store.Submissions.Operations
    ( DeltaTxSubmissions
    , SubmissionMeta (SubmissionMeta, submissionMetaResubmitted)
    , TxSubmissions
    , TxSubmissionsStatus
    , submissionMetaFromTxMeta
    )
import Cardano.Wallet.Primitive.Types
    ( SlotNo (SlotNo), WalletId )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( LocalTxSubmissionStatus (LocalTxSubmissionStatus), SealedTx )
import Cardano.Wallet.Submissions.Operations
    ( Operation (..) )
import Cardano.Wallet.Submissions.Submissions
    ( TxStatusMeta (..), mkEmpty, transactions, transactionsL )
import Cardano.Wallet.Submissions.TxStatus
    ( TxStatus (..), expirySlot, getTx, status )
import Cardano.Wallet.Transaction.Built
    ( BuiltTx (..) )
import Control.Category
    ( (.) )
import Control.Lens
    ( ix, (^.), (^..), (^?) )
import Data.Bifunctor
    ( second )
import Data.Maybe
    ( fromMaybe )

import qualified Data.Map.Strict as Map

emptyTxSubmissions_ :: TxSubmissions
emptyTxSubmissions_ = mkEmpty 0

addTxSubmission_
    :: BuiltTx
    -> SlotNo
    -> DeltaTxSubmissions
addTxSubmission_ BuiltTx{..} resubmitted =
    let txId = TxId $ builtTx ^. #txId
        expiry = case builtTxMeta ^. #expiry of
            Nothing -> SlotNo maxBound
            Just slot -> slot
    in AddSubmission expiry (txId, builtSealedTx)
        $ submissionMetaFromTxMeta builtTxMeta resubmitted

resubmitTx_
    :: Hash "Tx"
    -> SlotNo
    -> TxSubmissions
    -> [DeltaTxSubmissions]
resubmitTx_ (TxId -> txId) resubmitted walletSubmissions
    = fromMaybe [] $ do
        (TxStatusMeta datas meta) <-
            Map.lookup txId $ walletSubmissions ^. transactionsL
        (_, sealed) <- getTx datas
        pure $ case expirySlot datas of
            Nothing -> []
            Just expiry ->
                [ AddSubmission expiry (txId, sealed)
                    $ meta{submissionMetaResubmitted = resubmitted}
                , Forget txId
                ]


getInSubmissionTransactions_ :: TxSubmissions -> [TxSubmissionsStatus]
getInSubmissionTransactions_ submissions
    = submissions ^.. transactionsL . traverse

getInSubmissionTransaction_ :: Hash "Tx" -> TxSubmissions -> Maybe TxSubmissionsStatus
getInSubmissionTransaction_ txId submissions
    = submissions ^? transactionsL . ix (TxId txId)

readLocalTxSubmissionPending_
    :: TxSubmissions -> [LocalTxSubmissionStatus SealedTx]
readLocalTxSubmissionPending_ walletSubmissions = do
    (_k, x) <- Map.assocs $ walletSubmissions ^. transactionsL
    mkLocalTxSubmission x

rollForwardTxSubmissions_
    :: SlotNo -> [(SlotNo, Hash "Tx")] -> DeltaTxSubmissions
rollForwardTxSubmissions_ tip txs = RollForward tip (second TxId <$> txs)

removePendingOrExpiredTx_ :: TxSubmissions -> WalletId -> Hash "Tx"
    -> Either ErrRemoveTx DeltaTxSubmissions
removePendingOrExpiredTx_ walletSubmissions wid txId = do
    let
        errNoTx = ErrRemoveTxNoSuchTransaction $ ErrNoSuchTransaction wid txId
        errInLedger = ErrRemoveTxAlreadyInLedger txId
    case status (TxId txId) (transactions walletSubmissions) of
        Unknown -> Left errNoTx
        InLedger{} -> Left errInLedger
        _ -> Right $ Forget (TxId txId)

rollBackSubmissions_ :: SlotNo -> DeltaTxSubmissions
rollBackSubmissions_ = RollBack

pruneByFinality_ :: SlotNo -> DeltaTxSubmissions
pruneByFinality_ = Prune

mkLocalTxSubmission
    :: TxSubmissionsStatus
    -> [LocalTxSubmissionStatus SealedTx]
mkLocalTxSubmission (TxStatusMeta status' SubmissionMeta{..})
    = maybe
        []
        (\(TxId txId, sealed) -> pure $
            LocalTxSubmissionStatus (txId) sealed submissionMetaResubmitted
        )
        $ getTx status'
