{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Cardano.Wallet.Read.Tx.Hash
    ( byronTxHash
    , alonzoTxHash
    , shelleyTxHash
    , fromShelleyTxId
    , getEraTxHash
    )
where

import Cardano.Binary
    ( ToCBOR (..)
    )
import Cardano.Chain.UTxO
    ( ATxAux
    , taTx
    )
import Cardano.Crypto
    ( serializeCborHash
    )
import Cardano.Crypto qualified as CryptoC
import Cardano.Crypto.Hash qualified as Crypto
import Cardano.Ledger.Alonzo.Tx qualified as Alonzo
import Cardano.Ledger.Babbage.Tx qualified as Babbage hiding
    ( ScriptIntegrityHash
    , TxBody
    )
import Cardano.Ledger.Core
    ( AuxiliaryData
    )
import Cardano.Ledger.Core qualified as SL.Core
import Cardano.Ledger.Crypto qualified as SL
import Cardano.Ledger.Era
    ( Era (..)
    )
import Cardano.Ledger.SafeHash qualified as SafeHash
import Cardano.Ledger.Shelley.API qualified as SL
import Cardano.Ledger.Shelley.TxBody
    ( EraIndependentTxBody
    )
import Cardano.Ledger.ShelleyMA qualified as MA
import Cardano.Ledger.TxIn qualified as TxIn
import Cardano.Wallet.Read
    ( Tx
    )
import Cardano.Wallet.Read.Eras
    ( EraFun (..)
    , K (..)
    )
import Cardano.Wallet.Read.Tx.Eras
    ( onTx
    )
import Prelude

getEraTxHash :: EraFun Tx (K Crypto.ByteString)
getEraTxHash =
    EraFun
        { byronFun = onTx $ K . byronTxHash
        , shelleyFun = onTx $ K . shelleyTxHash
        , allegraFun = onTx $ K . shelleyTxHash
        , maryFun = onTx $ K . shelleyTxHash
        , alonzoFun = onTx $ K . alonzoTxHash
        , babbageFun = onTx $ K . alonzoTxHash
        }

byronTxHash :: ATxAux a -> Crypto.ByteString
byronTxHash = CryptoC.hashToBytes . serializeCborHash . taTx

alonzoTxHash
    :: ( Crypto.HashAlgorithm (SL.HASH crypto)
       , SafeHash.HashAnnotated
            (SL.Core.TxBody era)
            EraIndependentTxBody
            crypto
       )
    => Babbage.ValidatedTx era
    -> Crypto.ByteString
alonzoTxHash (Alonzo.ValidatedTx bod _ _ _) = fromShelleyTxId $ TxIn.txid bod

shelleyTxHash
    :: ( Era x
       , ToCBOR (AuxiliaryData x)
       , ToCBOR (SL.Core.TxBody x)
       , ToCBOR (SL.Core.Witnesses x)
       )
    => MA.Tx x
    -> Crypto.ByteString
shelleyTxHash
    (SL.Tx bod _ _) = fromShelleyTxId $ TxIn.txid bod

fromShelleyTxId :: SL.TxId crypto -> Crypto.ByteString
fromShelleyTxId (SL.TxId h) =
    Crypto.hashToBytes $ SafeHash.extractHash h
