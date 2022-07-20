{-# LANGUAGE DataKinds #-}

-- |
-- Copyright: © 2022 IOHK
-- License: Apache-2.0
--
-- Provides various 'Address' constants used by the wallet.
--
module Cardano.Wallet.Primitive.Types.Address.Constants
    ( maxLengthAddress
    , maxLengthAddressByron
    , maxLengthAddressShelley
    ) where

import Prelude

import Cardano.Wallet.Primitive.Passphrase
    ( Passphrase (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )
import Cardano.Wallet.Primitive.Types.ProtocolMagic
    ( ProtocolMagic (..) )
import Data.Function
    ( on )

import qualified Cardano.Byron.Codec.Cbor as Byron
import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS
import qualified Data.List as L

-- | The longest 'Address' that the wallet can generate.
--
-- Please note that this address should:
--
--  - never be used for anything besides its length and validity properties.
--  - never be used as a payment target within a real transaction.
--
maxLengthAddress :: Address
maxLengthAddress = L.maximumBy (compare `on` (BS.length . unAddress))
    [ maxLengthAddressByron
    , maxLengthAddressShelley
    ]

-- | The longest Byron-style 'Address' that the wallet can generate.
--
-- Please note that this address should:
--
--  - never be used for anything besides its length and validity properties.
--  - never be used as a payment target within a real transaction.
--
maxLengthAddressByron :: Address
maxLengthAddressByron = Address
    $ CBOR.toStrictByteString
    $ Byron.encodeAddress xpub
        [ Byron.encodeDerivationPathAttr passphrase maxBound maxBound
        , Byron.encodeProtocolMagicAttr (ProtocolMagic maxBound)
        ]
  where
    -- Must apparently always be 32 bytes:
    passphrase :: Passphrase "addr-derivation-payload"
    passphrase = Passphrase $ BA.convert $ BS.replicate 32 0

    xpub :: CC.XPub
    xpub = CC.toXPub $ CC.generate (BS.replicate 32 0) xprvPass
      where
        xprvPass = mempty :: BS.ByteString

-- | The longest Shelley-style 'Address' that the wallet can generate.
--
-- See: https://cips.cardano.org/cips/cip19/#binaryformat
--
-- Please note that this address should:
--
--  - never be used for anything besides its length and validity properties.
--  - never be used as a payment target within a real transaction.
--
maxLengthAddressShelley :: Address
maxLengthAddressShelley = Address $ BS.replicate 57 0
