{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.Primitive.Types.TxSeq
    (
    -- * Types
      TxSeq (..)

    -- * Constructors
    , empty
    , fromUTxO

    -- * Measurements
    , length
    , txCount
    , txGroupCount
    , txGroupBoundaryCount

    -- * Indicators
    , isValid

    -- * Conversions
    , toTxGroupList
    , toTxList
    , toTransitionList

    -- * Views
    , assetIds
    , headUTxO
    , lastUTxO
    , txIds

    -- * Maps
    , mapAssetIds
    , mapTxs
    , mapTxIds
    , mapUTxOs

    -- * Traversals
    , foldUTxO

    -- * Extensions
    , appendTx
    , appendTxGroupBoundary

    -- * Reductions
    , dropHeadTx
    , dropHeadTxs
    , dropLastTx
    , dropLastTxs
    , dropNullTx
    , dropNullTxs
    , dropGroupBoundary
    , dropGroupBoundaries
    , removeAssetId
    , removeAssets
    , shrinkAssetIds
    , shrinkTxIds
    ) where

import Prelude hiding
    ( length, seq )

import Cardano.Wallet.Primitive.Model
    ( applyTxToUTxO )
import Cardano.Wallet.Primitive.Types.Coin
    ( Coin )
import Cardano.Wallet.Primitive.Types.Hash
    ( Hash (..) )
import Cardano.Wallet.Primitive.Types.StateDeltaSeq
    ( StateDeltaSeq )
import Cardano.Wallet.Primitive.Types.TokenMap
    ( AssetId (..) )
import Cardano.Wallet.Primitive.Types.TokenPolicy
    ( TokenName (..), TokenPolicyId (..) )
import Cardano.Wallet.Primitive.Types.Tx
    ( Tx (..), TxIn, txAssetIds, txMapAssetIds, txMapTxIds, txRemoveAssetId )
import Cardano.Wallet.Primitive.Types.UTxO
    ( UTxO )
import Data.Bifoldable
    ( Bifoldable (..) )
import Data.Bifunctor
    ( bimap, first, second )
import Data.Either
    ( isLeft, isRight, lefts, rights )
import Data.Either.Extra
    ( mapRight )
import Data.Function
    ( (&) )
import Data.List.NonEmpty
    ( NonEmpty (..) )
import Data.Map.Strict
    ( Map )
import Data.Maybe
    ( mapMaybe )
import Data.Set
    ( Set )

import qualified Cardano.Wallet.Primitive.Types.StateDeltaSeq as Seq
import qualified Cardano.Wallet.Primitive.Types.Tx as Tx
import qualified Cardano.Wallet.Primitive.Types.UTxO as UTxO
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NE
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

--------------------------------------------------------------------------------
-- Public interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

newtype TxSeq = TxSeq
    {unTxSeq :: StateDeltaSeq UTxO (Either TxSeqGroupBoundary Tx)}
    deriving (Eq, Show)

data TxSeqGroupBoundary = TxSeqGroupBoundary
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Constructors
--------------------------------------------------------------------------------

empty :: TxSeq
empty = fromUTxO mempty

fromUTxO :: UTxO -> TxSeq
fromUTxO = TxSeq . Seq.fromState

--------------------------------------------------------------------------------
-- Measurements
--------------------------------------------------------------------------------

length :: TxSeq -> Int
length = F.length . unTxSeq

txCount :: TxSeq -> Int
txCount = F.length . toTxList

txGroupCount :: TxSeq -> Int
txGroupCount = succ . txGroupBoundaryCount

txGroupBoundaryCount :: TxSeq -> Int
txGroupBoundaryCount = F.length . lefts . Seq.toDeltaList . unTxSeq

--------------------------------------------------------------------------------
-- Indicators
--------------------------------------------------------------------------------

isValid :: TxSeq -> Bool
isValid = (Just True ==) . Seq.isValidM safeAppendTxM . unTxSeq

--------------------------------------------------------------------------------
-- Conversions
--------------------------------------------------------------------------------

toTxList :: TxSeq -> [Tx]
toTxList = rights . Seq.toDeltaList . unTxSeq

toTxGroupList :: TxSeq -> NonEmpty [Tx]
toTxGroupList = F.foldr acc (pure []) . unTxSeq
  where
    acc :: Either TxSeqGroupBoundary Tx -> NonEmpty [Tx] -> NonEmpty [Tx]
    acc delta groups@(h :| t) = case delta of
        Left TxSeqGroupBoundary ->
            [] `NE.cons` groups
        Right tx ->
            (tx : h) :| t

toTransitionList :: TxSeq -> [(UTxO, Tx, UTxO)]
toTransitionList (TxSeq s) =
    mapMaybe maybeTxTransition (Seq.toTransitionList s)
  where
    maybeTxTransition :: (u, Either a Tx, u) -> Maybe (u, Tx, u)
    maybeTxTransition (u0, e, u1) = e & either
        (const Nothing)
        (\tx -> Just (u0, tx, u1))

--------------------------------------------------------------------------------
-- Views
--------------------------------------------------------------------------------

assetIds :: TxSeq -> Set AssetId
assetIds = bifoldMap UTxO.assetIds (either (const mempty) txAssetIds) . unTxSeq

headUTxO :: TxSeq -> UTxO
headUTxO = Seq.headState . unTxSeq

lastUTxO :: TxSeq -> UTxO
lastUTxO = Seq.lastState . unTxSeq

txIds :: TxSeq -> Set (Hash "Tx")
txIds
    = bifoldMap UTxO.txIds (either (const mempty) (Set.singleton . txId))
    . unTxSeq

--------------------------------------------------------------------------------
-- Maps
--------------------------------------------------------------------------------

mapAssetIds :: (AssetId -> AssetId) -> TxSeq -> TxSeq
mapAssetIds f =
    TxSeq . bimap (UTxO.mapAssetIds f) (fmap (txMapAssetIds f)) . unTxSeq

mapTxs :: (Tx -> Tx) -> TxSeq -> TxSeq
mapTxs f = TxSeq . second (mapRight f) . unTxSeq

mapTxIds :: (Hash "Tx" -> Hash "Tx") -> TxSeq -> TxSeq
mapTxIds f =
    TxSeq . bimap (UTxO.mapTxIds f) (fmap (txMapTxIds f)) . unTxSeq

mapUTxOs :: (UTxO -> UTxO) -> TxSeq -> TxSeq
mapUTxOs f = TxSeq . first f . unTxSeq

--------------------------------------------------------------------------------
-- Traversals
--------------------------------------------------------------------------------

foldUTxO :: TxSeq -> UTxO
foldUTxO = bifoldMap id (const mempty) . unTxSeq

--------------------------------------------------------------------------------
-- Extensions
--------------------------------------------------------------------------------

appendTx :: Tx -> TxSeq -> Maybe TxSeq
appendTx tx =
    fmap TxSeq . Seq.applyDeltaM safeAppendTxM (Right tx) . unTxSeq

appendTxGroupBoundary :: TxSeq -> TxSeq
appendTxGroupBoundary =
    TxSeq . Seq.applyDelta const (Left TxSeqGroupBoundary) . unTxSeq

--------------------------------------------------------------------------------
-- Reductions
--------------------------------------------------------------------------------

dropHeadTx :: TxSeq -> Maybe TxSeq
dropHeadTx = fmap TxSeq . Seq.dropHead . unTxSeq

dropHeadTxs :: TxSeq -> [TxSeq]
dropHeadTxs = fmap TxSeq . Seq.suffixes . unTxSeq

dropLastTx :: TxSeq -> Maybe TxSeq
dropLastTx = fmap TxSeq . Seq.dropLast . unTxSeq

dropLastTxs :: TxSeq -> [TxSeq]
dropLastTxs = fmap TxSeq . Seq.prefixes . unTxSeq

dropGroupBoundary :: TxSeq -> [TxSeq]
dropGroupBoundary (TxSeq s) = TxSeq <$> Seq.dropEmptyTransitionWhere isLeft s

dropGroupBoundaries :: TxSeq -> TxSeq
dropGroupBoundaries (TxSeq s) = TxSeq $ Seq.dropEmptyTransitionsWhere isLeft s

dropNullTx :: TxSeq -> [TxSeq]
dropNullTx (TxSeq s) = TxSeq <$> Seq.dropEmptyTransitionWhere isRight s

dropNullTxs :: TxSeq -> TxSeq
dropNullTxs (TxSeq s) = TxSeq $ Seq.dropEmptyTransitionsWhere isRight s

removeAssetId :: TxSeq -> AssetId -> TxSeq
removeAssetId (TxSeq s) a = TxSeq $
    bimap (`UTxO.removeAssetId` a) (fmap (`txRemoveAssetId` a)) s

removeAssets :: TxSeq -> TxSeq
removeAssets s0 = F.foldl' removeAssetId s0 (assetIds s0)

shrinkAssetIds :: TxSeq -> TxSeq
shrinkAssetIds s = mapAssetIds toSimpleAssetId s
  where
    toSimpleAssetId :: AssetId -> AssetId
    toSimpleAssetId = mapToFunction
        (head simpleAssetIds)
        (Map.fromList $ F.toList (assetIds s) `zip` simpleAssetIds)

shrinkTxIds :: TxSeq -> TxSeq
shrinkTxIds s = mapTxIds toSimpleTxId s
  where
    toSimpleTxId :: Hash "Tx" -> Hash "Tx"
    toSimpleTxId = mapToFunction
        (head simpleTxIds)
        (Map.fromList $ F.toList (txIds s) `zip` simpleTxIds)

--------------------------------------------------------------------------------
-- Internal interface
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- Domain-specific constants
--------------------------------------------------------------------------------

simpleAssetIds :: [AssetId]
simpleAssetIds
    = AssetId (UnsafeTokenPolicyId $ Hash mempty)
    . UnsafeTokenName
    . T.encodeUtf8
    . T.pack
    . show <$> [0 :: Integer ..]

simpleTxIds :: [Hash "Tx"]
simpleTxIds = Hash . T.encodeUtf8 . T.pack . show <$> [0 :: Integer ..]

--------------------------------------------------------------------------------
-- Domain-specific functions
--------------------------------------------------------------------------------

canApplyTxToUTxO :: Tx -> UTxO -> Bool
canApplyTxToUTxO tx u =  (&&)
    (all inputRefIsValid (tx & Tx.resolvedInputs))
    (all inputRefIsValid (tx & Tx.resolvedCollateralInputs))
  where
    inputRefIsValid :: (TxIn, Coin) -> Bool
    inputRefIsValid (ti, c) = case UTxO.lookup ti u of
        Nothing -> False
        Just to -> Tx.txOutCoin to == c

safeAppendTx :: MonadFail m => UTxO -> Tx -> m UTxO
safeAppendTx = flip safeApplyTxToUTxO

safeAppendTxM :: MonadFail m => UTxO -> Either TxSeqGroupBoundary Tx -> m UTxO
safeAppendTxM u = either (const (pure u)) (safeAppendTx u)

safeApplyTxToUTxO :: MonadFail m => Tx -> UTxO -> m UTxO
safeApplyTxToUTxO tx u
    | tx `canApplyTxToUTxO` u =
        pure $ tx `applyTxToUTxO` u
    | otherwise = fail
        "cannot spend an input that does not refer to a known UTxO"

--------------------------------------------------------------------------------
-- Utility functions
--------------------------------------------------------------------------------

mapToFunction :: Ord k => v -> Map k v -> (k -> v)
mapToFunction = flip . Map.findWithDefault
