{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module FreeTrek.Pairing
    ( type (<->)(..)
    ) where

import Data.Functor.Sum
import Data.Functor.Product
import Control.Monad.Free
import Control.Monad.Identity
import Control.Comonad.Cofree

class (Functor f, Functor g) => f <-> g where
    pair :: (a -> b -> r) -> f a -> g b -> r

instance Identity <-> Identity where
    pair f (Identity a) (Identity b) = f a b

instance (f <-> g) => (Cofree f) <-> (Free g) where
    pair f (a :< _) (Pure x) = f a x
    pair f (_ :< fs) (Free gs) = pair (pair f) fs gs

instance (g <-> f, k <-> h) => (Product g k) <-> (Sum f h) where
    pair f (Pair g _) (InL x) = pair f g x
    pair f (Pair _ k) (InR x) = pair f k x
