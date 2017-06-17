{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module UI.Game
  ( playGame
  ) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (Left, Right)

import Tetris

import Brick hiding (Down)
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V
import Data.Map (Map)
import qualified Data.Map as M
import Lens.Micro

-- | Ticks mark passing of time
data Tick = Tick

-- | Named resources
type Name = ()

-- App definition and execution

app :: App Game Tick Name
app = App { appDraw = drawUI
          , appChooseCursor = neverShowCursor
          , appHandleEvent = handleEvent
          , appStartEvent = return
          , appAttrMap = const theMap
          }

playGame :: Int -> IO Game
playGame lvl = do
  let delay = levelToDelay lvl
  chan <- newBChan 10
  forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  initialGame <- initGame lvl
  customMain (V.mkVty V.defaultConfig) (Just chan) app initialGame

levelToDelay :: Int -> Int
levelToDelay n = 1000000 -- floor $ fromIntegral $ 72500 * 0.85 ^ n + n

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = liftIO (timeStep g) >>= continue
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ shift Right g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ shift Left g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ shift Down g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ rotate g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ hBox
     [ drawScore (g ^. score)
     , drawGrid g
     , drawNextShape (g ^. nextShape)
     ]
  ]

drawGrid :: Game -> Widget Name
drawGrid g = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Tetris")
  $ C.center
  $ str $ show $ blkMap
  -- $ foldr (<=>) emptyWidget rows
  where
    rows = [foldr (<+>) emptyWidget $ M.filterWithKey (inRow r) gmap
             | r <- [boardHeight,boardHeight-1..1]
           ]
    inRow r (_,y) _ = r == y
    gmap = drawMCell <$> mconcat [brdMap, blkMap, emptyMap]
    brdMap = Just <$> g ^. board
    blkMap = M.fromList [(c, Just $ g ^. block ^. shape) | c <- g ^. block ^. to coords]

emptyMap :: Map Coord (Maybe a)
emptyMap = M.fromList [((x,y), Nothing) | x <- [1..boardWidth], y <- [1,boardHeight]]

drawMCell :: Maybe Tetrimino -> Widget Name
drawMCell Nothing = withAttr emptyAttr cw
drawMCell (Just t) = drawCell t

drawCell :: Tetrimino -> Widget Name
drawCell t = withAttr (tToAttr t) cw
  where tToAttr I = iAttr
        tToAttr O = oAttr
        tToAttr T = tAttr
        tToAttr S = sAttr
        tToAttr Z = zAttr
        tToAttr J = jAttr
        tToAttr L = lAttr

cw :: Widget Name
cw = str "  "

drawScore :: Int -> Widget Name
drawScore n = vBox [ C.vCenter $ str "Score"
                   , C.center $ str $ show n
                   ]

drawNextShape :: Tetrimino -> Widget Name
-- TODO try vbox and see if different than foldr
drawNextShape t = padAll 1
  $ foldr (<=>) emptyWidget $ mkRow <$> [0,-1]
  where
    mkRow y = foldr (<+>) emptyWidget $ drawMCell . cellAt . (,y) <$> [-2..1]
    cellAt (x,y) = if (x,y) `elem` cs then Just t else Nothing
    blk = Block t (0,0) (relCells t)
    cs = blk ^. to coords

-- TODO test on mac terminal defAttr vs (bg black)
theMap = attrMap V.defAttr
  [ (iAttr, bg V.cyan)
  , (oAttr, bg V.yellow)
  , (tAttr, bg V.magenta)
  , (sAttr, bg V.green)
  , (zAttr, bg V.red)
  , (jAttr, bg V.blue)
  , (lAttr, bg V.white) -- damn no orange in ANSI
  ]

iAttr, oAttr, tAttr, sAttr, zAttr, jAttr, lAttr :: AttrName
iAttr = "I"
oAttr = "O"
tAttr = "T"
sAttr = "S"
zAttr = "Z"
jAttr = "J"
lAttr = "L"

emptyAttr :: AttrName
emptyAttr = "empty"
