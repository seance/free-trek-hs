{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TemplateHaskell       #-}

module FreeTrek.Algebras
    ( SystemsOps
    , WarpCoreOps(..)
    , WarpDriveOps(..)
    , PhasersOps(..)
    , DeflectorShieldsOps(..)
    , turnCrankShaft
    , engageAntimatterReactionAssembly
    , convertDilithiumCrystals
    , reattenuatePlasmaConduit
    , tuneWarpFieldCoil
    , disentangleElectroPlasmaSystem
    , focusOptronics
    , rewirePowerCellMatrix
    , purgePrefireChamber
    , decompressGravitonEmitter
    , repolarizeFrequencyModulator
    ) where

import FreeTrek.Domain
import FreeTrek.Inject
import Data.Functor.Sum
import Control.Monad.Free
import Data.Eq.Deriving
import Text.Show.Deriving

type S0 = Sum WarpCoreOps WarpDriveOps
type S1 = Sum PhasersOps S0
type S2 = Sum DeflectorShieldsOps S1

type SystemsOps = S2

data WarpCoreOps a
    = TurnCrankShaft a
    | EngageAntimatterReactionAssembly a
    | ConvertDilithiumCrystals Dilithium a
    deriving (Eq, Show, Functor)

data WarpDriveOps a
    = ReattenuatePlasmaConduit a
    | TuneWarpFieldCoil WarpSpeed a
    | DisentangleElectroPlasmaSystem a
    deriving (Eq, Show, Functor)

data PhasersOps a
    = FocusOptronics PhaserFrequency a
    | RewirePowerCellMatrix a
    | PurgePrefireChamber a
    deriving (Eq, Show, Functor)

data DeflectorShieldsOps a
    = DecompressGravitonEmitter a
    | RepolarizeFrequencyModulator DeflectorPolarity a
    deriving (Eq, Show, Functor)

turnCrankShaft :: (WarpCoreOps :<: f) => Free f ()
turnCrankShaft = injectFree (TurnCrankShaft ())

engageAntimatterReactionAssembly :: (WarpCoreOps :<: f) => Free f ()
engageAntimatterReactionAssembly = injectFree (EngageAntimatterReactionAssembly ())

convertDilithiumCrystals :: (WarpCoreOps :<: f) => Dilithium -> Free f ()
convertDilithiumCrystals dilithium = injectFree (ConvertDilithiumCrystals dilithium ())

reattenuatePlasmaConduit :: (WarpDriveOps :<: f) => Free f ()
reattenuatePlasmaConduit = injectFree (ReattenuatePlasmaConduit ())

tuneWarpFieldCoil :: (WarpDriveOps :<: f) => WarpSpeed -> Free f ()
tuneWarpFieldCoil warpSpeed = injectFree (TuneWarpFieldCoil warpSpeed ())

disentangleElectroPlasmaSystem :: (WarpDriveOps :<: f) => Free f ()
disentangleElectroPlasmaSystem = injectFree (DisentangleElectroPlasmaSystem ())

focusOptronics :: (PhasersOps :<: f) => PhaserFrequency -> Free f ()
focusOptronics freq = injectFree (FocusOptronics freq ())

rewirePowerCellMatrix :: (PhasersOps :<: f) => Free f ()
rewirePowerCellMatrix = injectFree (RewirePowerCellMatrix ())

purgePrefireChamber :: (PhasersOps :<: f) => Free f ()
purgePrefireChamber = injectFree (PurgePrefireChamber ())

decompressGravitonEmitter :: (DeflectorShieldsOps :<: f) => Free f ()
decompressGravitonEmitter = injectFree (DecompressGravitonEmitter ())

repolarizeFrequencyModulator :: (DeflectorShieldsOps :<: f) => DeflectorPolarity -> Free f ()
repolarizeFrequencyModulator polarity = injectFree (RepolarizeFrequencyModulator polarity ())

deriveEq1 ''WarpCoreOps
deriveEq1 ''WarpDriveOps
deriveEq1 ''PhasersOps
deriveEq1 ''DeflectorShieldsOps

deriveShow1 ''WarpCoreOps
deriveShow1 ''WarpDriveOps
deriveShow1 ''PhasersOps
deriveShow1 ''DeflectorShieldsOps
