{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Primitive.Tx.Allegra
    ( fromAllegraTx
    , fromLedgerTxValidity
    )
where

import Cardano.Api
    ( AllegraEra
    )
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Ledger.BaseTypes qualified as SL
import Cardano.Ledger.Shelley.API qualified as SL
import Cardano.Ledger.Shelley.API qualified as SLAPI
import Cardano.Ledger.ShelleyMA.AuxiliaryData qualified as MA
import Cardano.Ledger.ShelleyMA.TxBody qualified as MA
import Cardano.Wallet.Primitive.Types qualified as W
import Cardano.Wallet.Primitive.Types.Coin qualified as W
import Cardano.Wallet.Primitive.Types.Hash qualified as W
import Cardano.Wallet.Primitive.Types.Tx qualified as W
import Cardano.Wallet.Read.Eras
    ( allegra
    , inject
    )
import Cardano.Wallet.Read.Primitive.Tx.Features.Certificates
    ( anyEraCerts
    )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyCoin
    , fromShelleyMD
    , fromShelleyTxIn
    , fromShelleyTxOut
    , fromShelleyWdrl
    )
import Cardano.Wallet.Read.Tx
    ( Tx (..)
    )
import Cardano.Wallet.Read.Tx.CBOR
    ( renderTxToCBOR
    )
import Cardano.Wallet.Read.Tx.Hash
    ( shelleyTxHash
    )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , emptyTokenMapWithScripts
    )
import Data.Foldable
    ( toList
    )
import Data.Quantity
    ( Quantity (..)
    )
import Ouroboros.Network.Block qualified as O
import Prelude

-- NOTE: For resolved inputs we have to pass in a dummy value of 0.

fromAllegraTx
    :: SLAPI.Tx (Cardano.ShelleyLedgerEra AllegraEra)
    -> ( W.Tx
       , [W.Certificate]
       , TokenMapWithScripts
       , TokenMapWithScripts
       , Maybe ValidityIntervalExplicit
       )
fromAllegraTx tx =
    ( W.Tx
        { txId =
            W.Hash $ shelleyTxHash tx
        , txCBOR =
            Just $ renderTxToCBOR $ inject allegra $ Tx tx
        , fee =
            Just $ fromShelleyCoin fee
        , resolvedInputs =
            map ((,W.Coin 0) . fromShelleyTxIn) (toList ins)
        , resolvedCollateralInputs =
            -- TODO: (ADP-957)
            []
        , outputs =
            map fromShelleyTxOut (toList outs)
        , collateralOutput =
            -- Collateral outputs are not supported in Allegra.
            Nothing
        , withdrawals =
            fromShelleyWdrl wdrls
        , metadata =
            fromShelleyMD . toSLMetadata <$> SL.strictMaybeToMaybe mmd
        , scriptValidity =
            Nothing
        }
    , anyEraCerts certs
    , emptyTokenMapWithScripts
    , emptyTokenMapWithScripts
    , Just (fromLedgerTxValidity ttl)
    )
  where
    SL.Tx (MA.TxBody ins outs certs wdrls fee ttl _ _ _) _ mmd = tx

    -- fixme: [ADP-525] It is fine for now since we do not look at script
    -- pre-images. But this is precisely what we want as part of the
    -- multisig/script balance reporting.
    toSLMetadata (MA.AuxiliaryData blob _scripts) = SL.Metadata blob

fromLedgerTxValidity
    :: MA.ValidityInterval
    -> ValidityIntervalExplicit
fromLedgerTxValidity (MA.ValidityInterval from to) =
    case (from, to) of
        (MA.SNothing, MA.SJust (O.SlotNo s)) ->
            ValidityIntervalExplicit (Quantity 0) (Quantity s)
        (MA.SNothing, MA.SNothing) ->
            ValidityIntervalExplicit (Quantity 0) (Quantity maxBound)
        (MA.SJust (O.SlotNo s1), MA.SJust (O.SlotNo s2)) ->
            ValidityIntervalExplicit (Quantity s1) (Quantity s2)
        (MA.SJust (O.SlotNo s1), MA.SNothing) ->
            ValidityIntervalExplicit (Quantity s1) (Quantity maxBound)
