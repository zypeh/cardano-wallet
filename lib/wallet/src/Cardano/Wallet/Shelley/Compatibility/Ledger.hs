{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}

-- |
-- Copyright: © 2020 IOHK
-- License: Apache-2.0
--
-- Exposes a wallet-friendly interface to types and functions exported by the
-- ledger specification.
module Cardano.Wallet.Shelley.Compatibility.Ledger
    ( -- * Conversions from wallet types to ledger specification types
      toLedgerCoin
    , toLedgerTokenBundle
    , toLedgerTokenPolicyId
    , toLedgerTokenName
    , toLedgerTokenQuantity

      -- * Conversions from ledger specification types to wallet types
    , toWalletCoin
    , toWalletTokenBundle
    , toWalletTokenPolicyId
    , toWalletTokenName
    , toWalletTokenQuantity
    , toWalletScript

      -- * Roundtrip conversion between wallet types and ledger specification

    --   types
    , Convert (..)

      -- * Conversions for transaction outputs
    , toShelleyTxOut
    , toAllegraTxOut
    , toMaryTxOut
    , toAlonzoTxOut
    , toBabbageTxOut
    )
where

import Cardano.Address.Script
    ( KeyHash (..)
    , KeyRole (..)
    , Script (..)
    )
import Cardano.Crypto.Hash
    ( hashFromBytes
    , hashToBytes
    )
import Cardano.Crypto.Hash.Class qualified as Crypto
import Cardano.Ledger.Address qualified as Ledger
import Cardano.Ledger.Alonzo qualified as Alonzo
import Cardano.Ledger.Alonzo.TxBody qualified as Alonzo
import Cardano.Ledger.Babbage qualified as Babbage
import Cardano.Ledger.Babbage.TxBody qualified as Babbage
import Cardano.Ledger.Crypto qualified as Ledger
import Cardano.Ledger.Keys qualified as Ledger
import Cardano.Ledger.Mary.Value qualified as Ledger
import Cardano.Ledger.SafeHash
    ( unsafeMakeSafeHash
    )
import Cardano.Ledger.Shelley.API qualified as Ledger
import Cardano.Ledger.Shelley.TxBody qualified as Shelley
import Cardano.Ledger.ShelleyMA.Timelocks qualified as MA
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..)
    )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin (..)
    )
import Cardano.Wallet.Primitive.Types.Coin qualified as Coin
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle
    ( TokenBundle (..)
    )
import Cardano.Wallet.Primitive.Types.TokenBundle qualified as TokenBundle
import Cardano.Wallet.Primitive.Types.TokenMap qualified as TokenMap
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..)
    , TokenPolicyId (..)
    )
import Cardano.Wallet.Primitive.Types.TokenQuantity
    ( TokenQuantity (..)
    )
import Cardano.Wallet.Primitive.Types.Tx
    ( TxOut (..)
    )
import Data.ByteString.Short
    ( fromShort
    , toShort
    )
import Data.Foldable
    ( toList
    )
import Data.Function
    ( (&)
    )
import Data.Generics.Internal.VL.Lens
    ( view
    )
import Data.Generics.Labels
    (
    )
import Data.IntCast
    ( intCast
    )
import Data.Map.Strict qualified as Map
import Data.Map.Strict.NonEmptyMap qualified as NonEmptyMap
import Fmt
    ( pretty
    )
import GHC.Stack
    ( HasCallStack
    )
import Numeric.Natural
    ( Natural
    )
import Ouroboros.Consensus.Shelley.Eras
    ( StandardAllegra
    , StandardAlonzo
    , StandardBabbage
    , StandardCrypto
    , StandardMary
    , StandardShelley
    )
import Ouroboros.Network.Block qualified as O
import Prelude

--------------------------------------------------------------------------------
-- Roundtrip conversion between wallet types and ledger specification types
--------------------------------------------------------------------------------

-- | Connects a wallet type with its equivalent ledger specification type.
--
-- Instances of this class should satisfy the following laws:
--
-- >>> toLedger . toWallet == id
-- >>> toWallet . toLedger == id
class Convert wallet ledger | wallet -> ledger where
    -- | Converts a value from a wallet type to the equivalent ledger
    --   specification type.
    toLedger
        :: HasCallStack => wallet -> ledger

    -- | Converts a value from a ledger specification type to the equivalent
    --   wallet type.
    toWallet
        :: HasCallStack => ledger -> wallet

--------------------------------------------------------------------------------
-- Conversions for 'Coin'
--------------------------------------------------------------------------------

instance Convert Coin Ledger.Coin where
    toLedger = toLedgerCoin
    toWallet = toWalletCoin

toLedgerCoin :: Coin -> Ledger.Coin
toLedgerCoin (Coin c) = Ledger.Coin $ intCast @Natural @Integer c

toWalletCoin :: Ledger.Coin -> Coin
toWalletCoin (Ledger.Coin c) = Coin.unsafeFromIntegral c

--------------------------------------------------------------------------------
-- Conversions for 'TokenBundle'
--------------------------------------------------------------------------------

-- Values of the ledger specification's 'Value' type are constructed in a way
-- that is similar to the wallet's 'TokenBundle' type. The ada quantity is
-- stored as a separate value, and asset quantities are stored in a nested map.

instance Convert TokenBundle (Ledger.Value StandardCrypto) where
    toLedger = toLedgerTokenBundle
    toWallet = toWalletTokenBundle

toLedgerTokenBundle :: TokenBundle -> Ledger.Value StandardCrypto
toLedgerTokenBundle bundle =
    Ledger.Value ledgerAda ledgerTokens
  where
    (Ledger.Coin ledgerAda) = toLedgerCoin $ TokenBundle.getCoin bundle
    ledgerTokens =
        bundle
            & view #tokens
            & TokenMap.toNestedMap
            & Map.mapKeys toLedgerTokenPolicyId
            & Map.map mapInner
    mapInner inner =
        inner
            & NonEmptyMap.toMap
            & Map.mapKeys toLedgerTokenName
            & Map.map toLedgerTokenQuantity

toWalletTokenBundle :: Ledger.Value StandardCrypto -> TokenBundle
toWalletTokenBundle (Ledger.Value ledgerAda ledgerTokens) =
    TokenBundle.fromNestedMap (walletAda, walletTokens)
  where
    walletAda = toWalletCoin $ Ledger.Coin ledgerAda
    walletTokens =
        ledgerTokens
            & Map.mapKeys toWalletTokenPolicyId
            & Map.map mapInner
            & Map.mapMaybe NonEmptyMap.fromMap
    mapInner inner =
        inner
            & Map.mapKeys toWalletTokenName
            & Map.map toWalletTokenQuantity

--------------------------------------------------------------------------------
-- Conversions for 'TokenName'
--------------------------------------------------------------------------------

instance Convert TokenName Ledger.AssetName where
    toLedger = toLedgerTokenName
    toWallet = toWalletTokenName

toLedgerTokenName :: TokenName -> Ledger.AssetName
toLedgerTokenName (UnsafeTokenName bytes) =
    Ledger.AssetName $ toShort bytes

toWalletTokenName :: Ledger.AssetName -> TokenName
toWalletTokenName (Ledger.AssetName bytes) =
    UnsafeTokenName $ fromShort bytes

--------------------------------------------------------------------------------
-- Conversions for 'TokenPolicyId'
--------------------------------------------------------------------------------

instance Convert TokenPolicyId (Ledger.PolicyID StandardCrypto) where
    toLedger = toLedgerTokenPolicyId
    toWallet = toWalletTokenPolicyId

toLedgerTokenPolicyId :: TokenPolicyId -> Ledger.PolicyID StandardCrypto
toLedgerTokenPolicyId p@(UnsafeTokenPolicyId (Hash bytes)) =
    case hashFromBytes bytes of
        Just hash ->
            Ledger.PolicyID (Ledger.ScriptHash hash)
        Nothing ->
            error $
                unwords
                    [ "Ledger.toLedgerTokenPolicyId"
                    , "Unable to construct hash for token policy:"
                    , pretty p
                    ]

toWalletTokenPolicyId :: Ledger.PolicyID StandardCrypto -> TokenPolicyId
toWalletTokenPolicyId (Ledger.PolicyID (Ledger.ScriptHash hash)) =
    UnsafeTokenPolicyId (Hash (hashToBytes hash))

--------------------------------------------------------------------------------
-- Conversions for 'TokenQuantity'
--------------------------------------------------------------------------------

instance Convert TokenQuantity Integer where
    toLedger = toLedgerTokenQuantity
    toWallet = toWalletTokenQuantity

toLedgerTokenQuantity :: TokenQuantity -> Integer
toLedgerTokenQuantity (TokenQuantity q) = fromIntegral q

toWalletTokenQuantity :: Integer -> TokenQuantity
toWalletTokenQuantity q
    | q >= 0 =
        TokenQuantity $ fromIntegral q
    | otherwise =
        error $
            unwords
                [ "Ledger.toWalletTokenQuantity:"
                , "Unexpected negative value:"
                , pretty q
                ]

--------------------------------------------------------------------------------
-- Conversions for 'Address'
--------------------------------------------------------------------------------

instance Convert Address (Ledger.Addr StandardCrypto) where
    toLedger (Address bytes) = case Ledger.deserialiseAddr bytes of
        Just addr -> addr
        Nothing ->
            error $
                unwords
                    [ "toLedger @Address: Invalid address:"
                    , pretty (Address bytes)
                    ]
    toWallet = Address . Ledger.serialiseAddr

--------------------------------------------------------------------------------
-- Conversions for 'TxOut'
--------------------------------------------------------------------------------

toShelleyTxOut
    :: TxOut
    -> Shelley.TxOut StandardShelley
toShelleyTxOut (TxOut addr bundle) =
    Shelley.TxOut (toLedger addr) (toLedger (TokenBundle.coin bundle))

toAllegraTxOut
    :: TxOut
    -> Shelley.TxOut StandardAllegra
toAllegraTxOut (TxOut addr bundle) =
    Shelley.TxOut (toLedger addr) (toLedger (TokenBundle.coin bundle))

toMaryTxOut
    :: TxOut
    -> Shelley.TxOut StandardMary
toMaryTxOut (TxOut addr bundle) =
    Shelley.TxOut (toLedger addr) (toLedger bundle)

toAlonzoTxOut
    :: TxOut
    -> Maybe (Hash "Datum")
    -> Alonzo.TxOut StandardAlonzo
toAlonzoTxOut (TxOut addr bundle) = \case
    Nothing ->
        Alonzo.TxOut
            (toLedger addr)
            (toLedger bundle)
            Ledger.SNothing
    Just (Hash bytes) ->
        Alonzo.TxOut
            (toLedger addr)
            (toLedger bundle)
            ( Ledger.SJust $
                unsafeMakeSafeHash $
                    Crypto.UnsafeHash $
                        toShort bytes
            )

toBabbageTxOut
    :: TxOut
    -> Maybe (Hash "Datum")
    -> Babbage.TxOut StandardBabbage
toBabbageTxOut (TxOut addr bundle) = \case
    Nothing ->
        Babbage.TxOut
            (toLedger addr)
            (toLedger bundle)
            Babbage.NoDatum
            Ledger.SNothing
    Just (Hash bytes) ->
        Babbage.TxOut
            (toLedger addr)
            (toLedger bundle)
            ( Babbage.DatumHash $
                unsafeMakeSafeHash $
                    Crypto.UnsafeHash $
                        toShort bytes
            )
            Ledger.SNothing

toWalletScript
    :: Ledger.Crypto crypto
    => KeyRole
    -> MA.Timelock crypto
    -> Script KeyHash
toWalletScript keyrole = fromLedgerScript
  where
    fromLedgerScript (MA.RequireSignature (Ledger.KeyHash h)) =
        RequireSignatureOf (KeyHash keyrole (hashToBytes h))
    fromLedgerScript (MA.RequireAllOf contents) =
        RequireAllOf $ map fromLedgerScript $ toList contents
    fromLedgerScript (MA.RequireAnyOf contents) =
        RequireAnyOf $ map fromLedgerScript $ toList contents
    fromLedgerScript (MA.RequireMOf num contents) =
        RequireSomeOf (fromIntegral num) $ fromLedgerScript <$> toList contents
    fromLedgerScript (MA.RequireTimeExpire (O.SlotNo slot)) =
        ActiveUntilSlot $ fromIntegral slot
    fromLedgerScript (MA.RequireTimeStart (O.SlotNo slot)) =
        ActiveFromSlot $ fromIntegral slot
