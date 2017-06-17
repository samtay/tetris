module Main where

import Tetris (Game)

import UI.PickLevel (pickLevel)
import UI.Game (playGame)

-- | TODO possibly allow a small number of CLI args,
-- like tetris --high-score
main :: IO ()
main = pickLevel >>= playGame >>= handleEndGame

-- | TODO possibly save high score (with 3 initials? kick it real old school?) to ~/.tetris
handleEndGame :: Game -> IO ()
handleEndGame = const $ return ()
