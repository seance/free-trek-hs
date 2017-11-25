{-# LANGUAGE FlexibleContexts      #-}

module Main where

import FreeTrek.Domain
import FreeTrek.Algebras
import FreeTrek.Interpreters
import FreeTrek.Pairing
import FreeTrek.Planning
import Control.Monad.Writer (execWriter)
import Control.Monad.RWS (runRWS)

main :: IO ()
main = do
    print s1
    mapM_ (mapM_ print) ops
    where
        (ps, s1) = plan' 9 10 [EmptySpace]
        ops = interpret' ps

plan' :: WarpSpeed -> Dilithium -> Expedition -> ([Plan], PlanState)
plan' warpSpeed dilithium expedition = (ps, s1)
    where
        (ps, s1, ()) = runRWS planExpedition cfg s0
        cfg = PlanConfig { warpSpeed = warpSpeed }
        s0 = PlanState { dilithium = dilithium, expedition = expedition }

interpret' :: [Plan] -> [[SystemsOps ()]]
interpret' ps = f <$> ps
    where
        f p = execWriter $ pair const traceInterpreter p
