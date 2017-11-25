{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}

module FreeTrek.Planning
    ( Plan
    , PlanConfig(..)
    , PlanState(..)
    , planExpedition
    ) where

import FreeTrek.Domain
import FreeTrek.Algebras
import Control.Monad.Reader.Class
import Control.Monad.State.Class
import Control.Monad.Free
import Control.Monad.Extra

type MonadPlan m = (MonadReader PlanConfig m, MonadState PlanState m)
type Plan = Free SystemsOps ()

data PlanConfig = PlanConfig {
    warpSpeed :: WarpSpeed
} deriving Show

data PlanState = PlanState {
    dilithium :: Dilithium,
    expedition :: Expedition
} deriving Show

markExpendedDilithium :: (MonadPlan m) => Dilithium -> m ()
markExpendedDilithium expended =
    modify $ \s -> s { dilithium = dilithium s - expended }

popFirstEncounter :: (MonadPlan m) => m Encounter
popFirstEncounter = do
    e <- gets $ head . expedition
    modify $ \s -> s { expedition = tail $ expedition s }
    return e

startIgnition :: (MonadPlan m) => m Plan
startIgnition = return turnCrankShaft

planForEmptySpace :: (MonadPlan m) => m Plan
planForEmptySpace = return reattenuatePlasmaConduit

planForNebula :: (MonadPlan m) => m Plan
planForNebula = return disentangleElectroPlasmaSystem

planForAsteroidField :: (MonadPlan m) => m Plan
planForAsteroidField = return $ do
    engageAntimatterReactionAssembly
    repolarizeFrequencyModulator Antiparallel

planForSpaceCrab :: (MonadPlan m) => m Plan
planForSpaceCrab = return $ do
    repolarizeFrequencyModulator Parallel
    focusOptronics NinetyNineTHz
    purgePrefireChamber

planForSpaceTimeAnomaly :: (MonadPlan m) => m Plan
planForSpaceTimeAnomaly = return $ do
    rewirePowerCellMatrix
    decompressGravitonEmitter

planEncounter :: (MonadPlan m) => Encounter -> m Plan
planEncounter = \case
    EmptySpace -> planForEmptySpace
    Nebula -> planForNebula
    AsteroidField -> planForAsteroidField
    HostileSpaceCrab -> planForSpaceCrab
    SpaceTimeAnomaly -> planForSpaceTimeAnomaly

planWarpJump :: (MonadPlan m) => m Plan
planWarpJump = do
    warpSpeed <- asks warpSpeed
    dilithium <- gets dilithium
    let w = min warpSpeed dilithium
    markExpendedDilithium w
    return $ do
        convertDilithiumCrystals w
        tuneWarpFieldCoil w

planWarpToEncounter :: (MonadPlan m) => Encounter -> m Plan
planWarpToEncounter e = do
    warpJump <- planWarpJump
    encounter <- planEncounter e
    return $ do
        warpJump
        encounter

planExpeditionStages :: (MonadPlan m) => m [Plan]
planExpeditionStages =
    ifM (gets $ null . expedition)
        (return [])
        (do
            e <- popFirstEncounter
            h <- planWarpToEncounter e
            t <- planExpeditionStages
            return $ h : t)

planExpedition :: (MonadPlan m) => m [Plan]
planExpedition = do
    ignition <- startIgnition
    stages <- planExpeditionStages
    return $ ignition : stages
