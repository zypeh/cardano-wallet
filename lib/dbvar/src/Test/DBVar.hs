{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Support for testing DBVar stores

module Test.DBVar
    ( genUpdates
    , prop_StoreUpdates
    , GenDelta
    , Updates (..)
    , runStoreLaw
    , applyS
    , checkLaw
    , reset
    , context
    , observe
    , ignore
    ) where

import Prelude

import Control.Exception.Safe
    ( impureThrow )
import Control.Monad.Reader
    ( MonadReader (ask) )
import Control.Monad.RWS
    ( MonadWriter (tell), RWST, censor, evalRWST, listen )
import Control.Monad.State
    ( MonadState (get, put), MonadTrans (lift), forM_ )
import Data.DBVar
    ( Store (loadS, updateS, writeS) )
import Data.Delta
    ( Delta (..) )
import Data.Either
    ( isRight )
import Fmt
    ( Buildable, listF, pretty )
import Test.QuickCheck
    ( Blind (Blind), Gen, Property, conjoin, counterexample, sized, (===) )
import Test.QuickCheck.Monadic
    ( PropertyM, assert, monitor, pick )


-- | Given a value, generate a random delta starting from this value.
type GenDelta da = Base da -> Gen da

-- | A sequence of updates and values after updating.
-- The update that is applied *last* appears in the list *first*.
newtype Updates da = Updates [(Base da, da)]

instance Show da => Show (Updates da) where
    show (Updates xs) = show . map snd $ xs

-- | Randomly generate a sequence of updates
genUpdates :: Delta da => Gen (Base da) -> GenDelta da -> Gen (Updates da)
genUpdates gen0 more = sized $ \n -> go n [] =<< gen0
  where
    go 0 das _  = pure $ Updates das
    go n das a0 = do
        da <- more a0
        let a1 = apply da a0
        go (n-1) ((a1,da):das) a1

-- | Test whether 'updateS' and 'loadS' behave as expected.
--
-- TODO: Shrinking of the update sequence.
prop_StoreUpdates
    :: ( Monad m, Delta da, Eq (Base da), Buildable da, Show (Base da))
    => (forall b. m b -> PropertyM IO b)
    -- ^ Function to embed the monad in 'IO'
    -> Store m da
    -- ^ Store that is to be tested.
    -> Gen (Base da)
    -- ^ Generator for the initial value.
    -> GenDelta da
    -- ^ Generator for deltas.
    -> PropertyM IO ()
prop_StoreUpdates toPropertyM store gen0 more = do

    -- randomly generate a sequence of updates
    Blind a0 <- pick $ Blind <$> gen0
    Blind (Updates adas) <- pick $ Blind <$> genUpdates (pure a0) more
    let as  = map fst adas ++ [a0]
        das = map snd adas

    monitor $ counterexample $
        "\nUpdates applied:\n" <> pretty (listF das)

    -- apply those updates
    ea <- toPropertyM $ do
        writeS store a0
        -- first update is applied last!
        let updates = reverse $ zip das (drop 1 as)
        forM_ updates $ \(da,a) -> updateS store a da
        loadS store

    -- check whether the last value is correct
    case ea of
        Left err -> impureThrow err
        Right a  -> do
            monitor $ counterexample $ "\nExpected:\n" <> show (head as)
            monitor $ counterexample $ "\nGot:\n" <> show a
            assert $ a == head as

type StoreLaw m da = RWST (Store m da) [Property] (Base da, Base da, [da]) m

applyS :: (Monad m, Delta da) => da -> StoreLaw m da ()
applyS r = do
    s <- ask
    (q, x, ds) <- get
    put (q, apply r x , r : ds)
    lift $ updateS s x r

checkLaw
    :: (Monad m, Eq (Base da), Show (Base da), Show da)
    => StoreLaw m da ()
checkLaw = do
    (_, x, reverse -> ds) <- get
    x' <- ask >>= lift . loadS
    tell
        [ counterexample (show (ds, leftOf x')) (isRight x')
        , counterexample (show ds) $ rightOf x' === x
        ]
    where
    leftOf (Left x) = x
    leftOf _ = undefined
    rightOf (Right x) = x
    rightOf _ = undefined


reset :: Monad m => StoreLaw m da ()
reset = do
    s <- ask
    (q,_,_) <- get
    lift $ writeS s q
    put (q, q,[])

runStoreLaw
    :: (Monad m, Eq (Base da), Show (Base da), Show da)
    => Store m da
    -> StoreLaw m da a
    -> m Property
runStoreLaw s f = do
    mx <- loadS s
    w <- case mx of
        Right x -> snd <$> evalRWST (f >> checkLaw) s (x, x, [])
        Left e -> pure [counterexample (show e) (isRight mx)]
    pure $ conjoin w

context :: Monad m => (Property -> Property) -> StoreLaw m da x -> StoreLaw m da x
context d f = do
    (x,w) <- listen f
    tell $ fmap d w
    pure x

observe :: Monad m => (Base da -> Property) ->  StoreLaw m da ()
observe f = do
    (_,s,_) <- get
    tell [f s]

ignore :: Monad m =>   StoreLaw m da x  -> StoreLaw m da x
ignore = censor (const [])
