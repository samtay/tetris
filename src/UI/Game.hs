{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module UI.Game
  ( playGame
  ) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Control.Monad.IO.Class (liftIO)
import Prelude hiding (Left, Right)

import Brick hiding (Down)
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens hiding (preview, op)
import Control.Monad.Trans.State
import qualified Graphics.Vty as V
import Data.Map (Map)
import qualified Data.Map as M
import Linear.V2 (V2(..))

import Tetris

data UI = UI
  { _game    :: Game         -- ^ tetris game
  , _preview :: Maybe String -- ^ hard drop preview cell
  , _locked  :: Bool         -- ^ lock after hard drop before time step
  }

makeLenses ''UI

-- | Ticks mark passing of time
data Tick = Tick

-- | Named resources
type Name = ()

data CellLocation = InGrid | InNextShape
data TVisual = Normal | HardDrop

-- App definition and execution

app :: App UI Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = return
  , appAttrMap      = const theMap
  }

playGame :: Int -> Maybe String -> IO Game
playGame lvl mp = do
  let delay = levelToDelay lvl
  chan <- newBChan 10
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  initialGame <- initGame lvl
  let initialUI = UI initialGame mp False
  ui <- customMain (V.mkVty V.defaultConfig) (Just chan) app initialUI
  return $ ui ^. game

levelToDelay :: Int -> Int
levelToDelay n = floor $ 400000 * (0.85 :: Double) ^ (2 * n)

-- Handling events

handleEvent :: UI -> BrickEvent Name Tick -> EventM Name (Next UI)
handleEvent ui (AppEvent Tick                      ) = handleTick ui
handleEvent ui (VtyEvent (V.EvKey V.KRight      [])) = exec (shift Right) ui
handleEvent ui (VtyEvent (V.EvKey V.KLeft       [])) = exec (shift Left) ui
handleEvent ui (VtyEvent (V.EvKey V.KDown       [])) = exec (shift Down) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'l') [])) = exec (shift Right) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'h') [])) = exec (shift Left) ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'j') [])) = exec (shift Down) ui
handleEvent ui (VtyEvent (V.EvKey V.KUp         [])) = exec rotate ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'k') [])) = exec rotate ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  continue $ ui & game %~ execTetris hardDrop & locked .~ True
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'r') [])) = restart ui
handleEvent ui (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt ui
handleEvent ui (VtyEvent (V.EvKey V.KEsc        [])) = halt ui
handleEvent ui _ = continue ui

-- | This common execution function is used for all game input except hard
-- drop. If locked (from hard drop) do nothing, else execute the state
-- computation and unlock.
exec :: Tetris () -> UI -> EventM Name (Next UI)
exec op ui = continue
  $ if ui ^. locked || ui ^. game . to isGameOver
    then ui
    else ui & game %~ execTetris op

-- | Handles time steps, does nothing if game is over
handleTick :: UI -> EventM Name (Next UI)
handleTick ui =
  if ui ^. game . to isGameOver
  then continue ui
  else do
    next <- execStateT timeStep $ ui ^. game
    continue $ ui & game .~ next
                  & locked .~ False

-- | Restart game at the same level
restart :: UI -> EventM Name (Next UI)
restart ui = do
  let lvl = ui ^. game ^. level
  g <- liftIO $ initGame lvl
  continue $ ui & game .~ g
                & locked .~ False

-- Drawing

drawUI :: UI -> [Widget Name]
drawUI ui =
  [ C.vCenter $ vLimit 22 $ hBox
      [ padLeft Max $ padRight (Pad 2) $ drawStats (ui ^. game)
      , drawGrid ui
      , padRight Max $ padLeft (Pad 2) $ drawInfo (ui ^. game)
      ]
  ]

drawGrid :: UI -> Widget Name
drawGrid ui =
  hLimit 22
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Tetris")
    $ vBox rows
 where
  rows =
    [ foldr (<+>) emptyWidget $ M.filterWithKey (inRow r) gmap
    | r <- [boardHeight, boardHeight - 1 .. 1]
    ]
  inRow r (V2 _ y) _ = r == y
  gmap    = mconcat [brdMap, cBlkMap, hrdMap, emptyCellMap]
  brdMap  = draw Normal . Just <$> g ^. board
  hrdMap  = blkMap (evalTetris hardDroppedBlock g) HardDrop
  cBlkMap = blkMap (g ^. block) Normal
  draw    = drawMCell (ui ^. preview) InGrid
  g       = ui ^. game
  blkMap b v = M.fromList . map (, draw v . Just $ b ^. shape) $ coords b

emptyCellMap :: Map Coord (Widget Name)
emptyCellMap = M.fromList cws
 where
  cws = [ ((V2 x y), ew) | x <- [1 .. boardWidth], y <- [1 .. boardHeight] ]
  ew  = drawMCell Nothing InGrid Normal Nothing

drawMCell
  :: Maybe String -> CellLocation -> TVisual -> Maybe Tetrimino -> Widget Name
drawMCell _  InGrid      _ Nothing  = withAttr emptyAttr cw
drawMCell _  InNextShape _ Nothing  = withAttr emptyAttr ecw
drawMCell mp _           v (Just t) = drawCell mp t v

drawCell :: Maybe String -> Tetrimino -> TVisual -> Widget Name
drawCell _        t Normal   = withAttr (tToAttr t) cw
drawCell Nothing  t HardDrop = withAttr (tToAttrH t) hcw
drawCell (Just p) t HardDrop = withAttr (tToAttrH t) (str p)

tToAttr :: Tetrimino -> AttrName
tToAttr I = iAttr
tToAttr O = oAttr
tToAttr T = tAttr
tToAttr S = sAttr
tToAttr Z = zAttr
tToAttr J = jAttr
tToAttr L = lAttr

tToAttrH :: Tetrimino -> AttrName
tToAttrH I = ihAttr
tToAttrH O = ohAttr
tToAttrH T = thAttr
tToAttrH S = shAttr
tToAttrH Z = zhAttr
tToAttrH J = jhAttr
tToAttrH L = lhAttr

cw :: Widget Name
cw = str " ."

ecw :: Widget Name
ecw = str "  "

hcw :: Widget Name
hcw = str "◤◢"

drawStats :: Game -> Widget Name
drawStats g =
  hLimit 22
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Stats")
    $ vBox
        [ drawStat "Score" $ g ^. score
        , padTop (Pad 1) $ drawStat "Level" $ g ^. level
        , drawLeaderBoard g
        ]

drawStat :: String -> Int -> Widget Name
drawStat s n = padLeftRight 1 $ str s <+> (padLeft Max $ str $ show n)

drawLeaderBoard :: Game -> Widget Name
drawLeaderBoard _ = emptyWidget

drawInfo :: Game -> Widget Name
drawInfo g = hLimit 18 -- size of next piece box
  $ vBox
    [ drawNextShape (g ^. nextShape)
    , padTop (Pad 2) $ drawHelp
    , padTop (Pad 1) $ drawGameOver g
    ]

drawNextShape :: Tetrimino -> Widget Name
drawNextShape t =
  withBorderStyle BS.unicodeBold
    $   B.borderWithLabel (str "Next")
    $   padTopBottom 1
    $   padLeftRight 4
    $   vLimit 4
    $   vBox
    $   mkRow
    <$> [0, -1]
 where
  mkRow y =
    hBox
      $   drawMCell Nothing InNextShape Normal
      .   cellAt
      .   (`V2` y)
      <$> [-2 .. 1]
  cellAt (V2 x y) = if (V2 x y) `elem` cs then Just t else Nothing
  blk = Block t (V2 0 0) (relCells t)
  cs  = blk ^. to coords

drawHelp :: Widget Name
drawHelp =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Help")
    $ padTopBottom 1
    $ vBox
    $ map (uncurry drawKeyInfo)
    $ [ ("Left"   , "h, ←")
      , ("Right"  , "l, →")
      , ("Down"   , "j, ↓")
      , ("Rotate" , "k, ↑")
      , ("Drop"   , "space")
      , ("Restart", "r")
      , ("Quit"   , "q")
      ]

drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  (padRight Max $ padLeft (Pad 1) $ str action)
    <+> (padLeft Max $ padRight (Pad 1) $ str keys)

drawGameOver :: Game -> Widget Name
drawGameOver g =
  if (isGameOver g)
  then padLeftRight 4 $ withAttr gameOverAttr $ str "GAME OVER"
  else emptyWidget

theMap :: AttrMap
theMap = attrMap
  V.defAttr
  [ (iAttr       , tToColor I `on` tToColor I)
  , (oAttr       , tToColor O `on` tToColor O)
  , (tAttr       , tToColor T `on` tToColor T)
  , (sAttr       , tToColor S `on` tToColor S)
  , (zAttr       , tToColor Z `on` tToColor Z)
  , (jAttr       , tToColor J `on` tToColor J)
  , (lAttr       , tToColor L `on` tToColor L)
  , (ihAttr      , fg $ tToColor I)
  , (ohAttr      , fg $ tToColor O)
  , (thAttr      , fg $ tToColor T)
  , (shAttr      , fg $ tToColor S)
  , (zhAttr      , fg $ tToColor Z)
  , (jhAttr      , fg $ tToColor J)
  , (lhAttr      , fg $ tToColor L)
  , (gameOverAttr, fg V.red `V.withStyle` V.bold)
  ]

tToColor :: Tetrimino -> V.Color
tToColor I = V.cyan
tToColor O = V.yellow
tToColor T = V.magenta
tToColor S = V.green
tToColor Z = V.red
tToColor J = V.blue
tToColor L = V.white

iAttr, oAttr, tAttr, sAttr, zAttr, jAttr, lAttr :: AttrName
iAttr = "I"
oAttr = "O"
tAttr = "T"
sAttr = "S"
zAttr = "Z"
jAttr = "J"
lAttr = "L"

ihAttr, ohAttr, thAttr, shAttr, zhAttr, jhAttr, lhAttr :: AttrName
ihAttr = "Ih"
ohAttr = "Oh"
thAttr = "Th"
shAttr = "Sh"
zhAttr = "Zh"
jhAttr = "Jh"
lhAttr = "Lh"

emptyAttr :: AttrName
emptyAttr = "empty"

gameOverAttr :: AttrName
gameOverAttr = "gameOver"
