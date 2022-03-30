-------------------------------------------------------------------------------
-- time-units-types
-- Copyright 2022 Michael B. Gale (github@michael-gale.co.uk)
-------------------------------------------------------------------------------

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Exports types which can be used to describe time periods at
-- the type-level. Use the `durationVal` function to reify them as the
-- corresponding value-level descriptions from "Data.Time.Units" or the
-- `durationMicroseconds` function to reify them as microseconds straight away.
module Data.Time.TypeLevel (
    KnownDuration(..),
    durationMicroseconds,

    Attosecond,
    Femtosecond,
    Picosecond,
    Nanosecond,
    Microsecond,
    Millisecond,
    Second,
    Minute,
    Hour,
    Day,
    Week,
    Fortnight
) where

-------------------------------------------------------------------------------

import GHC.TypeLits

import Data.Time.Units qualified as Units
import Data.Proxy
import Data.Kind

-------------------------------------------------------------------------------

-- | Represents @n@-many attoseconds.
data Attosecond (n :: Nat)

-- | Represents @n@-many femtoseconds.
data Femtosecond (n :: Nat)

-- | Represents @n@-many picoseconds.
data Picosecond (n :: Nat)

-- | Represents @n@-many nanoseconds.
data Nanosecond (n :: Nat)

-- | Represents @n@-many microseconds.
data Microsecond (n :: Nat)

-- | Represents @n@-many milliseconds.
data Millisecond (n :: Nat)

-- | Represents @n@-many seconds.
data Second (n :: Nat)

-- | Represents @n@-many minutes.
data Minute (n :: Nat)

-- | Represents @n@-many hours.
data Hour (n :: Nat)

-- | Represents @n@-many days.
data Day (n :: Nat)

-- | Represents @n@-many weeks.
data Week (n :: Nat)

-- | Represents @n@-many fortnights.
data Fortnight (n :: Nat)

-- | A class of types which can be reified as value-level descriptions of
-- time periods.
class KnownDuration k where
    -- | The type representing value-level descriptions of the time period
    -- corresponding to @k@.
    type DurationUnit k :: Type

    -- | `durationVal` reifies the duration as the corresponding value-level
    -- type. Intended to be used with @TypeApplications@.
    --
    -- >>> durationVal @(Second 10)
    -- 10s :: Data.Time.Units.Second
    durationVal :: DurationUnit k

instance KnownNat n => KnownDuration (Attosecond n) where
    type DurationUnit (Attosecond n) = Units.Attosecond

    durationVal =
        fromInteger @Units.Attosecond $
        natVal (Proxy :: Proxy n)

-- | `durationMicroseconds` is a convenience function which reifies a
-- type-level duration as microseconds on the value level. Intended to be
-- used with @TypeApplications@.
--
-- >>> durationMicroseconds @(Second 10)
-- 10000000 :: Integer
durationMicroseconds
    :: forall d . (KnownDuration d, Units.TimeUnit (DurationUnit d))
    => Integer
durationMicroseconds = Units.toMicroseconds $ durationVal @d

instance KnownNat n => KnownDuration (Femtosecond n) where
    type DurationUnit (Femtosecond n) = Units.Femtosecond

    durationVal =
        fromInteger @Units.Femtosecond $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration (Picosecond n) where
    type DurationUnit (Picosecond n) = Units.Picosecond

    durationVal =
        fromInteger @Units.Picosecond $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration (Nanosecond n) where
    type DurationUnit (Nanosecond n) = Units.Nanosecond

    durationVal =
        fromInteger @Units.Nanosecond $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration (Microsecond n) where
    type DurationUnit (Microsecond n) = Units.Microsecond

    durationVal =
        fromInteger @Units.Microsecond $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration (Millisecond n) where
    type DurationUnit (Millisecond n) = Units.Millisecond

    durationVal =
        fromInteger @Units.Millisecond $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration (Second n) where
    type DurationUnit (Second n) = Units.Second

    durationVal =
        fromInteger @Units.Second $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration (Minute n) where
    type DurationUnit (Minute n) = Units.Minute

    durationVal =
        fromInteger @Units.Minute $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration (Hour n) where
    type DurationUnit (Hour n) = Units.Hour

    durationVal =
        fromInteger @Units.Hour $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration (Day n) where
    type DurationUnit (Day n) = Units.Day

    durationVal =
        fromInteger @Units.Day $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration (Week n) where
    type DurationUnit (Week n) = Units.Week

    durationVal =
        fromInteger @Units.Week $
        natVal (Proxy :: Proxy n)

instance KnownNat n => KnownDuration (Fortnight n) where
    type DurationUnit (Fortnight n) = Units.Fortnight

    durationVal =
        fromInteger @Units.Fortnight $
        natVal (Proxy :: Proxy n)

-------------------------------------------------------------------------------
