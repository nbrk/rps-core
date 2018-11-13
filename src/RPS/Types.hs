{-# LANGUAGE TemplateHaskell #-}
module RPS.Types where

import           Control.Lens
import qualified Data.Aeson.Diff as AD


data Decision = Rock | Paper | Scissors
  deriving (Show, Eq)

data Outcome = Draw | WinPlayer1 | WinPlayer2
  deriving (Show, Eq)

data Game = Game
  { _gamePlayer1     :: Maybe Decision
  , _gamePlayer2     :: Maybe Decision
  , _gameOutcome     :: Outcome
  , _gameRound       :: Int
  , _gamePlayer1Wins :: Int
  , _gamePlayer2Wins :: Int
  }
  deriving (Show, Eq)
makeLenses ''Game

type Patch = AD.Patch

