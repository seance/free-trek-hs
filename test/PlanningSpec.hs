{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import Test.Tasty
import Test.Tasty.HUnit

import FreeTrek.Domain
import FreeTrek.Algebras
import FreeTrek.Interpreters
import FreeTrek.Inject
import FreeTrek.Pairing
import FreeTrek.Planning
import Control.Monad.Writer (execWriter)
import Control.Monad.RWS (runRWS)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Expedition planning"
    [
      testCase "At least start up the Enterprise" $
        checkExpeditionPlan 0 10 [] $ \dilithium stages -> do
            stages @?= [ignition]

    , testCase "Track dilithium usage in a single encounter" $
        checkExpeditionPlan 5 10 [EmptySpace] $ \dilithium stages -> do
            dilithium @?= 5

    , testCase "Track dilithium usage across multiple encounters" $
        checkExpeditionPlan 3 10 [EmptySpace, EmptySpace] $ \dilithium stages -> do
            dilithium @?= 4

    , testCase "Lower warp speed when low on dilithium" $
        checkExpeditionPlan 6 10 [EmptySpace, EmptySpace] $ \dilithium stages -> do
            dilithium @?= 0
            stages @?=
                [ ignition
                , emptySpace 6
                , emptySpace 4
                ]

    , testCase "Take the chance to reattenuate the plasma conduit in empty space" $
        checkExpeditionPlan 6 10 [EmptySpace] $ \dilithium stages -> do
            dilithium @?= 4
            stages @?=
                [ ignition
                , emptySpace 6
                ]

    , testCase "Disentangle the electro-plasma system in nebulas" $
        checkExpeditionPlan 6 10 [Nebula] $ \dilithium stages -> do
            dilithium @?= 4
            stages @?=
                [ ignition
                , nebula 6
                ]

    , testCase "Setup deflector polarity, phaser frequency and focus optronics when encountering space crabs" $
        checkExpeditionPlan 6 10 [HostileSpaceCrab] $ \dilithium stages -> do
            dilithium @?= 4
            stages @?=
                [ ignition
                , spaceCrab 6
                ]

    , testCase "Engage the antimatter reaction assembly and repolarize shields in asteroid fields" $
        checkExpeditionPlan 6 10 [AsteroidField] $ \dilithium stages -> do
            dilithium @?= 4
            stages @?=
                [ ignition
                , asteroidField 6
                ]

    , testCase "Take precaution by rewiring phaser power cell matrix and decompress the deflector graviton emitter near space-time anomalies" $
        checkExpeditionPlan 6 10 [SpaceTimeAnomaly] $ \dilithium stages -> do
            dilithium @?= 4
            stages @?=
                [ ignition
                , spaceTimeAnomaly 6
                ]

    , testCase "Correctly plan for long and varied space expeditions" $
        let expedition = [EmptySpace, Nebula, HostileSpaceCrab, AsteroidField, SpaceTimeAnomaly]
        in checkExpeditionPlan 7 30 expedition $ \dilithium stages -> do
            dilithium @?= 0
            stages @?=
                [ ignition
                , emptySpace 7
                , nebula 7
                , spaceCrab 7
                , asteroidField 7
                , spaceTimeAnomaly 2
                ]
    ]

type Checks = Dilithium -> [[SystemsOps ()]] -> Assertion

checkExpeditionPlan :: WarpSpeed -> Dilithium -> Expedition -> Checks -> Assertion
checkExpeditionPlan warpSpeed' dilithium' expedition checks =
    checks (dilithium s1) ops
        where
            (ps, s1) = plan warpSpeed' dilithium' expedition
            ops = interpret ps

plan :: WarpSpeed -> Dilithium -> Expedition -> ([Plan], PlanState)
plan warpSpeed' dilithium' expedition = (ps, s1)
    where
        (ps, s1, ()) = runRWS planExpedition cfg s0
        cfg = PlanConfig { warpSpeed = warpSpeed' }
        s0 = PlanState { dilithium = dilithium', expedition = expedition }

interpret :: [Plan] -> [[SystemsOps ()]]
interpret ps = interpret' <$> ps
    where
        interpret' p = execWriter $ pair const traceInterpreter p

ignition :: [SystemsOps ()]
ignition = [inject $ TurnCrankShaft ()]

warpJump :: WarpSpeed -> [SystemsOps ()]
warpJump w =
    [ inject $ ConvertDilithiumCrystals w ()
    , inject $ TuneWarpFieldCoil w ()
    ]

emptySpace :: WarpSpeed -> [SystemsOps ()]
emptySpace w = warpJump w ++
    [ inject $ ReattenuatePlasmaConduit ()
    ]

nebula :: WarpSpeed -> [SystemsOps ()]
nebula w = warpJump w ++
    [ inject $ DisentangleElectroPlasmaSystem ()
    ]

asteroidField :: WarpSpeed -> [SystemsOps ()]
asteroidField w = warpJump w ++
    [ inject $ EngageAntimatterReactionAssembly ()
    , inject $ RepolarizeFrequencyModulator Antiparallel ()
    ]

spaceCrab :: WarpSpeed -> [SystemsOps ()]
spaceCrab w = warpJump w ++
    [ inject $ RepolarizeFrequencyModulator Parallel ()
    , inject $ FocusOptronics NinetyNineTHz ()
    , inject $ PurgePrefireChamber ()
    ]

spaceTimeAnomaly :: WarpSpeed -> [SystemsOps ()]
spaceTimeAnomaly w = warpJump w ++
    [ inject $ RewirePowerCellMatrix ()
    , inject $ DecompressGravitonEmitter ()
    ]
