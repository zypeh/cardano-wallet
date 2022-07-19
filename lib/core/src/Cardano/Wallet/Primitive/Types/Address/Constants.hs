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
import Cardano.Wallet.Primitive.Types
    ( ProtocolMagic (..) )
import Cardano.Wallet.Primitive.Types.Address
    ( Address (..) )

import qualified Cardano.Byron.Codec.Cbor as Byron
import qualified Cardano.Crypto.Wallet as CC
import qualified Codec.CBOR.Write as CBOR
import qualified Data.ByteArray as BA
import qualified Data.ByteString as BS

-- | The longest 'Address' that the wallet can generate.
--
maxLengthAddress :: Address
maxLengthAddress = maxLengthAddressByron

-- | The longest Byron-style 'Address' that the wallet can generate.
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
maxLengthAddressShelley :: Address
maxLengthAddressShelley = Address $ BS.replicate 57 0
