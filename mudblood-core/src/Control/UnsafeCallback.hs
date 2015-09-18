-- | The same as DynCallback, but without the Typeable constraint and thus, less safe.

module Control.UnsafeCallback
    ( UnsafeCallback
    , unsafeCallback, runUnsafeCallback

    ) where

import Unsafe.Any

newtype UnsafeCallback a = UnsafeCallback Any

unsafeCallback :: (a -> b) -> UnsafeCallback a
unsafeCallback f = UnsafeCallback $ unsafeCoerce f

runUnsafeCallback :: UnsafeCallback a -> Maybe (a -> b)
runUnsafeCallback (UnsafeCallback cb) = Just $ unsafeCoerce cb
