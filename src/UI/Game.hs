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

data CellLocation = InGrid | InNextShape

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
levelToDelay n = floor $ 400000 * 0.85 ^ (2 * n)

-- Handling events

handleEvent :: Game -> BrickEvent Name Tick -> EventM Name (Next Game)
handleEvent g (AppEvent Tick)                       = handleTick g
handleEvent g (VtyEvent (V.EvKey V.KRight []))      = continue $ shift Right g
handleEvent g (VtyEvent (V.EvKey V.KLeft []))       = continue $ shift Left g
handleEvent g (VtyEvent (V.EvKey V.KDown []))       = continue $ shift Down g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'l') [])) = continue $ shift Right g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'h') [])) = continue $ shift Left g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'j') [])) = continue $ shift Down g
handleEvent g (VtyEvent (V.EvKey (V.KChar ' ') [])) = continue $ hardDrop g
handleEvent g (VtyEvent (V.EvKey V.KUp []))         = continue $ rotate g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'k') [])) = continue $ rotate g
handleEvent g (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt g
handleEvent g (VtyEvent (V.EvKey V.KEsc []))        = halt g
handleEvent g _                                     = continue g

-- | Handles time steps, does nothing if game is over
handleTick :: Game -> EventM Name (Next Game)
handleTick g = if isGameOver g
                  then continue g
                  else liftIO (timeStep g) >>= continue

-- Drawing

drawUI :: Game -> [Widget Name]
drawUI g =
  [ C.vCenter $ vLimit 22 $ hBox [ padLeft Max $ padRight (Pad 2) $ drawStats g
                                 , drawGrid g
                                 , padRight Max $ padLeft (Pad 2) $ drawInfo g
                                 ]
  ]

drawGrid :: Game -> Widget Name
drawGrid g = hLimit 22
  $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Tetris")
  $ vBox rows
  where
    rows = [foldr (<+>) emptyWidget $ M.filterWithKey (inRow r) gmap
             | r <- [boardHeight,boardHeight-1..1]
           ]
    inRow r (_,y) _ = r == y
    gmap = drawMCell InGrid <$> mconcat [brdMap, blkMap, emptyMap]
    brdMap = Just <$> g ^. board
    blkMap = M.fromList [(c, Just $ g ^. block ^. shape) | c <- g ^. block ^. to coords]

emptyMap :: Map Coord (Maybe a)
emptyMap = M.fromList [((x,y), Nothing) | x <- [1..boardWidth], y <- [1..boardHeight]]

drawMCell :: CellLocation -> Maybe Tetrimino -> Widget Name
drawMCell InGrid Nothing = withAttr emptyAttr cw
drawMCell InNextShape Nothing = withAttr emptyAttr ecw
drawMCell _ (Just t) = drawCell t

drawCell :: Tetrimino -> Widget Name
drawCell t = withAttr (tToAttr t) cw
  where tToAttr I = iAttr
        tToAttr O = oAttr
        tToAttr T = tAttr
        tToAttr S = sAttr
        tToAttr Z = zAttr
        tToAttr J = jAttr
        tToAttr L = lAttr

-- TODO • for hardDrop preview

cw :: Widget Name
cw = str " ."

ecw :: Widget Name
ecw = str "  "

drawStats :: Game -> Widget Name
drawStats g = hLimit 22
  $ withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Stats")
  $ vBox [ drawStat "Score" $ g ^. score
         , padTop (Pad 1) $ drawStat "Level" $ g ^. level
         , drawLeaderBoard g
         ]

drawStat :: String -> Int -> Widget Name
drawStat s n = padLeftRight 1
  $ str s <+> (padLeft Max $ str $ show n)

drawLeaderBoard :: Game -> Widget Name
drawLeaderBoard g = emptyWidget

drawInfo :: Game -> Widget Name
drawInfo g = hLimit 16 -- size of next piece box
  $ vBox [ drawNextShape (g ^. nextShape)
         , padTop (Pad 2) $ drawHelp
         ]

drawNextShape :: Tetrimino -> Widget Name
drawNextShape t = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Next")
  $ padTopBottom 1 $ padLeftRight 3
  $ vLimit 4
  $ vBox $ mkRow <$> [0,-1]
  where
    mkRow y = hBox $ drawMCell InNextShape . cellAt . (,y) <$> [-2..1]
    cellAt (x,y) = if (x,y) `elem` cs then Just t else Nothing
    blk = Block t (0,0) (relCells t)
    cs = blk ^. to coords

drawHelp :: Widget Name
drawHelp = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Help")
  $ padTopBottom 1
  $ vBox $ map (uncurry drawKeyInfo)
  $ [ ("Left", "h, ←")
    , ("Right", "l, →")
    , ("Down", "j, ↓")
    , ("Rotate", "k, ↑")
    , ("Quit", "q")
    ]

drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  (padRight Max $ padLeft (Pad 1) $ str (action ++ ":"))
  <+> (padLeft Max $ padRight (Pad 1) $ str keys)


theMap = attrMap V.defAttr
  [ (iAttr, on V.cyan V.cyan)
  , (oAttr, on V.yellow V.yellow)
  , (tAttr, on V.magenta V.magenta)
  , (sAttr, on V.green V.green)
  , (zAttr, on V.red V.red)
  , (jAttr, on V.blue V.blue)
  , (lAttr, on V.white V.white) -- damn no orange in ANSI
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
