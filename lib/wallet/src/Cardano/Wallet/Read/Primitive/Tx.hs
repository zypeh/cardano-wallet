{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
module Cardano.Wallet.Read.Primitive.Tx
    ( fromCardanoTx
    )
where

import Cardano.Api qualified as Cardano
import Cardano.Api.Byron qualified as Cardano
    ( Tx (ByronTx)
    )
import Cardano.Api.Shelley qualified as Cardano
import Cardano.Wallet.Primitive.Types qualified as W
import Cardano.Wallet.Primitive.Types.Tx qualified as W
import Cardano.Wallet.Read.Primitive.Tx.Allegra
    ( fromAllegraTx
    )
import Cardano.Wallet.Read.Primitive.Tx.Alonzo
    ( fromAlonzoTx
    )
import Cardano.Wallet.Read.Primitive.Tx.Babbage
    ( fromBabbageTx
    )
import Cardano.Wallet.Read.Primitive.Tx.Byron
    ( fromTxAux
    )
import Cardano.Wallet.Read.Primitive.Tx.Mary
    ( fromMaryTx
    )
import Cardano.Wallet.Read.Primitive.Tx.Shelley
    ( fromShelleyTx
    )
import Cardano.Wallet.Transaction
    ( TokenMapWithScripts (..)
    , ValidityIntervalExplicit (..)
    , emptyTokenMapWithScripts
    )
import Prelude

fromCardanoTx
    :: Cardano.Tx era
    -> ( W.Tx
       , TokenMapWithScripts
       , TokenMapWithScripts
       , [W.Certificate]
       , Maybe ValidityIntervalExplicit
       )
fromCardanoTx = \case
    Cardano.ShelleyTx era tx -> case era of
        Cardano.ShelleyBasedEraShelley ->
            extract $ fromShelleyTx tx
        Cardano.ShelleyBasedEraAllegra ->
            extract $ fromAllegraTx tx
        Cardano.ShelleyBasedEraMary ->
            extract $ fromMaryTx tx
        Cardano.ShelleyBasedEraAlonzo ->
            extract $ fromAlonzoTx tx
        Cardano.ShelleyBasedEraBabbage ->
            extract $ fromBabbageTx tx
    Cardano.ByronTx tx ->
        ( fromTxAux tx
        , emptyTokenMapWithScripts
        , emptyTokenMapWithScripts
        , []
        , Nothing
        )
  where
    extract (tx, certs, mint, burn, validity) =
        (tx, mint, burn, certs, validity)
