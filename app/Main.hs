module Main where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)

import Tetris

import Brick
import Brick.BChan
import qualified Graphics.Vty as V

-- | Ticks mark passing of time
data Tick = Tick

-- | Named resources
type Name = ()

app :: App Game Tick Name
app = undefined

main :: IO ()
main = pickLevel >>= playGame

pickLevel :: IO Int
pickLevel = undefined

playGame :: Int -> IO ()
playGame lvl = do
  let delay = levelToDelay lvl
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  initialGame <- initGame lvl
  customMain (V.mkVty V.defaultConfig) (Just chan) app initialGame >>= handleEndGame

levelToDelay :: Int -> Int
levelToDelay = undefined

-- | TODO possibly save high score (with 3 initials? kick it real old school?) to ~/.tetris
handleEndGame :: Game -> IO ()
handleEndGame = const $ return ()
