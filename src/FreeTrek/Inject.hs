{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances     #-}

module FreeTrek.Inject
    ( (:<:)(..)
    , injectFree
    ) where

import Data.Functor.Sum
import Control.Monad.Free

class (Functor f, Functor g) => f :<: g where
    inject :: f a -> g a

instance {-# OVERLAPPABLE #-} (Functor f, Functor g) => f :<: Sum f g where
    inject = InL

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, g :<: h) => g :<: Sum f h where
    inject = InR . inject

instance {-# OVERLAPS #-} (Functor f) => f :<: f where
    inject = id

injectFree :: (Functor f, f :<: g) => f a -> Free g a
injectFree = liftF . inject
