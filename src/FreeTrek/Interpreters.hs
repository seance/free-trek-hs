{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

module FreeTrek.Interpreters
    ( Interpreters
    , traceInterpreter
    ) where

import FreeTrek.Domain
import FreeTrek.Algebras
import FreeTrek.Inject
import FreeTrek.Pairing
import Data.Functor (void)
import Data.Functor.Product
import Control.Comonad.Cofree
import Control.Monad.Writer.Class
import Control.Monad.Writer (Writer)

type P0 = Product CoWarpCoreOps CoWarpDriveOps
type P1 = Product CoPhasersOps P0
type P2 = Product CoDeflectorShieldsOps P1

type Interpreters = P2

data CoWarpCoreOps a = CoWarpCoreOps {
    coWarpCoreOps :: forall b. WarpCoreOps b -> a
} deriving Functor

data CoWarpDriveOps a = CoWarpDriveOps {
    coWarpDriveOps :: forall b. WarpDriveOps b -> a
} deriving Functor

data CoPhasersOps a = CoPhasersOps {
    coPhasersOps :: forall b. PhasersOps b -> a
} deriving Functor

data CoDeflectorShieldsOps a = CoDeflectorShieldsOps {
    coDeflectorShieldsOps :: forall b. DeflectorShieldsOps b -> a
} deriving Functor

trace :: (MonadWriter [SystemsOps ()] m, f :<: SystemsOps) => m () -> f a -> m ()
trace m fa = m >> tell [inject $ void fa]

traceInterpreter :: Cofree Interpreters (Writer [SystemsOps ()] ())
traceInterpreter = coiter next start
    where
        start = tell []
        next w =
            Pair (CoDeflectorShieldsOps $ trace w)
                 (Pair (CoPhasersOps $ trace w)
                       (Pair (CoWarpCoreOps $ trace w)
                             (CoWarpDriveOps $ trace w)))

instance CoWarpCoreOps <-> WarpCoreOps where
    pair f (CoWarpCoreOps g) p@(TurnCrankShaft k) = f (g p) k
    pair f (CoWarpCoreOps g) p@(EngageAntimatterReactionAssembly k) = f (g p) k
    pair f (CoWarpCoreOps g) p@(ConvertDilithiumCrystals _ k) = f (g p) k

instance CoWarpDriveOps <-> WarpDriveOps where
    pair f (CoWarpDriveOps g) p@(ReattenuatePlasmaConduit k) = f (g p) k
    pair f (CoWarpDriveOps g) p@(TuneWarpFieldCoil _ k) = f (g p) k
    pair f (CoWarpDriveOps g) p@(DisentangleElectroPlasmaSystem k) = f (g p) k

instance CoPhasersOps <-> PhasersOps where
    pair f (CoPhasersOps g) p@(FocusOptronics _ k) = f (g p) k
    pair f (CoPhasersOps g) p@(RewirePowerCellMatrix k) = f (g p) k
    pair f (CoPhasersOps g) p@(PurgePrefireChamber k) = f (g p) k

instance CoDeflectorShieldsOps <-> DeflectorShieldsOps where
    pair f (CoDeflectorShieldsOps g) p@(DecompressGravitonEmitter k) = f (g p) k
    pair f (CoDeflectorShieldsOps g) p@(RepolarizeFrequencyModulator _ k) = f (g p) k
