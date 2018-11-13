{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}

module RPS.TypesInstances where

import           RPS.Types

import           Control.Lens
import           Control.Monad.State
import qualified Data.Aeson          as A
import qualified Data.Aeson.Diff     as AD
import           Data.Maybe
import           Data.Monoid
import           GHC.Generics

import           WEGO

--
-- GHC Generic autoderivation
--
deriving instance Generic Decision
deriving instance Generic Outcome
deriving instance Generic Game

--
-- JSON default instances
--
instance A.ToJSON Decision
instance A.ToJSON Outcome
instance A.ToJSON Game

instance A.FromJSON Decision
instance A.FromJSON Outcome
instance A.FromJSON Game


--
-- WEGO
--
instance (a ~ Patch) => Diffable Game a where
  diff g1 g2 = AD.diff (A.toJSON g1) (A.toJSON g2)
  patch p g =
    case AD.patch p (A.toJSON g) of
      A.Success v ->
        case A.fromJSON v of
          A.Success g' -> Just g'
          A.Error _    -> Nothing
      A.Error _ -> Nothing


instance Serializable AD.Patch where
  encode = A.encode
  decode = A.decode


instance SimulationState Game where
  initial = Game Nothing Nothing Draw 0 0 0
  ready g =
    isJust (_gamePlayer1 g) && isJust (_gamePlayer2 g)

  simulate = Endo sim


sim :: Game -> Game
sim g = flip execState g $ do
  p1d <- use gamePlayer1
  p2d <- use gamePlayer2
  let o = decideOutcome (fromJust p1d) (fromJust p2d)
  gameOutcome .= o

  case o of
    WinPlayer1 -> gamePlayer1Wins += 1
    WinPlayer2 -> gamePlayer2Wins += 1
    _          -> return ()

  gamePlayer1 .= Nothing
  gamePlayer2 .= Nothing
  gameRound += 1



decideOutcome :: Decision -> Decision -> Outcome
decideOutcome p1d p2d        | p1d == p2d = Draw
decideOutcome Rock Paper     = WinPlayer2
decideOutcome Paper Rock     = WinPlayer1
decideOutcome Rock Scissors  = WinPlayer2
decideOutcome Scissors Rock  = WinPlayer1
decideOutcome Paper Scissors = WinPlayer2
decideOutcome Scissors Paper = WinPlayer1
