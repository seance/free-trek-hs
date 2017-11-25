module FreeTrek.Domain where

type Dilithium = Int
type WarpSpeed = Int

data Encounter
    = EmptySpace
    | Nebula
    | AsteroidField
    | HostileSpaceCrab
    | SpaceTimeAnomaly
    deriving Show

type Expedition = [Encounter]

data PhaserFrequency
    = NinetyNineTHz
    | TwoKHz
    deriving (Eq, Show)

data DeflectorPolarity
    = Parallel
    | Antiparallel
    deriving (Eq, Show)
