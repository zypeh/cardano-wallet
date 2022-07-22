module Cardano.Wallet.Primitive.Types.AddressContext
    ( AddressEra (..)
    , AddressContext (..)
    )
    where

import Cardano.Wallet.Primitive.Types.Address
    ( Address )

--------------------------------------------------------------------------------
-- The 'AddressEra' type
--------------------------------------------------------------------------------

-- | Represents an era for 'Address' values.
--
data AddressEra
    = AddressEraByron
    -- ^ Represents the Byron address era.
    | AddressEraShelley
    -- ^ Represents the Shelley address era.

--------------------------------------------------------------------------------
-- The 'AddressContext' type
--------------------------------------------------------------------------------

-- | An address context that can be used to estimate address lengths.
--
-- To construct an 'AddressContext', please choose the constructor that is most
-- appropriate for your use case:
--
--  - Use 'AddressContextForAddress':
--    in situations where an exact address is available.
--
--  - Use 'AddressContextForEra':
--    in situations where an exact address is not available, but where an
--    address era is available.
--
--  - Use 'AddressContextDefault':
--    in situations where neither an exact address nor an address era are
--    available.
--
data AddressContext
    = AddressContextDefault
    -- ^ A default specification.
    | AddressContextForAddress Address
    -- ^ A specification based on an exact address.
    | AddressContextForEra AddressEra
    -- ^ A specification based on an address era.
