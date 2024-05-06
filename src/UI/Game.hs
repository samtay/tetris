{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
module UI.Game
  ( playGame
  ) where

import Control.Concurrent (threadDelay, forkIO)
import Control.Monad (void, forever)
import Prelude hiding (Left, Right)

import Brick hiding (Down)
import Brick.BChan
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import Control.Lens hiding (preview, op, zoom)
import Control.Monad.Extra (orM, unlessM)
import Control.Monad.IO.Class (liftIO)
import qualified Graphics.Vty as V
import qualified Graphics.Vty.CrossPlatform
import qualified Graphics.Vty.Config
import Data.Map (Map)
import qualified Data.Map as M
import Linear.V2 (V2(..))

import Tetris

data UI = UI
  { _game    :: Game         -- ^ tetris game
  , _preview :: Maybe String -- ^ hard drop preview cell
  , _locked  :: Bool         -- ^ lock after hard drop before time step
  , _paused  :: Bool         -- ^ game paused
  }

makeLenses ''UI

-- | Ticks mark passing of time
data Tick = Tick

-- | Named resources
type Name = ()

data VisualBlock
  = NormalBlock
  | HardDropBlock String

-- App definition and execution

app :: App UI Tick Name
app = App
  { appDraw         = drawUI
  , appChooseCursor = neverShowCursor
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const theMap
  }

playGame
  :: Int -- ^ Starting level
  -> Maybe String -- ^ Preview cell (Nothing == no preview)
  -> IO Game
playGame lvl mp = do
  let delay = levelToDelay lvl
  chan <- newBChan 10
  void . forkIO $ forever $ do
    writeBChan chan Tick
    threadDelay delay
  initialGame <- initGame lvl
  let buildVty = Graphics.Vty.CrossPlatform.mkVty Graphics.Vty.Config.defaultConfig
  initialVty <- buildVty
  ui <- customMain initialVty buildVty (Just chan) app $ UI
    { _game    = initialGame
    , _preview = mp
    , _locked  = False
    , _paused  = False
    }
  return $ ui ^. game

levelToDelay :: Int -> Int
levelToDelay n = floor $ 400000 * (0.85 :: Double) ^ (2 * n)

-- Handling events

handleEvent :: BrickEvent Name Tick -> EventM Name UI ()
handleEvent (VtyEvent (V.EvKey (V.KChar 'r') [])) = restart
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') [])) = halt
handleEvent (VtyEvent (V.EvKey V.KEsc        [])) = halt
handleEvent (VtyEvent (V.EvKey V.KRight      [])) = exec (shift Right)
handleEvent (VtyEvent (V.EvKey V.KLeft       [])) = exec (shift Left)
handleEvent (VtyEvent (V.EvKey V.KDown       [])) = exec (shift Down)
handleEvent (VtyEvent (V.EvKey (V.KChar 'l') [])) = exec (shift Right)
handleEvent (VtyEvent (V.EvKey (V.KChar 'h') [])) = exec (shift Left)
handleEvent (VtyEvent (V.EvKey (V.KChar 'j') [])) = exec (shift Down)
handleEvent (VtyEvent (V.EvKey V.KUp         [])) = exec rotate
handleEvent (VtyEvent (V.EvKey (V.KChar 'k') [])) = exec rotate
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') [])) =
  unlessM (orM [use paused, use (game . to isGameOver)]) $ do
    zoom game hardDrop
    assign locked True
handleEvent (VtyEvent (V.EvKey (V.KChar 'p') [])) =
  unlessM (orM [use locked, use (game . to isGameOver)]) $ do
    modifying paused not
handleEvent (AppEvent Tick                      ) =
  unlessM (orM [use paused, use (game . to isGameOver)]) $ do
    zoom game timeStep
    assign locked False
handleEvent _ = pure ()

-- | This common execution function is used for all game user input except hard
-- drop and pause. If paused or locked (from hard drop) do nothing, else
-- execute the state computation.
exec :: Tetris () -> EventM Name UI ()
exec = unlessM (orM [use paused, use locked, use (game . to isGameOver)]) . zoom game

-- | Restart game at the same level
restart :: EventM Name UI ()
restart = do
  lvl <- use $ game . level
  g <- liftIO $ initGame lvl
  assign game g
  assign locked False

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
    $ case ui ^. paused of
        True  -> C.center $ str "Paused"
        False -> vBox $ [boardHeight, boardHeight - 1 .. 1] <&> \r ->
          foldr (<+>) emptyWidget
            . M.filterWithKey (\(V2 _ y) _ -> r == y)
            $ mconcat
                [ drawBlockCell NormalBlock <$> ui ^. (game . board)
                , blockMap NormalBlock (ui ^. (game . block))
                , case ui ^. preview of
                    Nothing -> M.empty
                    Just s  -> blockMap (HardDropBlock s) (evalTetris hardDroppedBlock (ui ^. game))
                , emptyCellMap
                ]
 where
  blockMap v b =
    M.fromList $ [ (c, drawBlockCell v (b ^. shape)) | c <- coords b ]

emptyCellMap :: Map Coord (Widget Name)
emptyCellMap = M.fromList
  [ (V2 x y, emptyGridCellW) | x <- [1 .. boardWidth], y <- [1 .. boardHeight] ]

emptyGridCellW :: Widget Name
emptyGridCellW = withAttr emptyAttr cw

emptyNextShapeCellW :: Widget Name
emptyNextShapeCellW = withAttr emptyAttr ecw

drawBlockCell :: VisualBlock -> Tetrimino -> Widget Name
drawBlockCell NormalBlock       t = withAttr (tToAttr t) cw
drawBlockCell (HardDropBlock s) t = withAttr (tToAttrH t) (str s)

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
drawStat s n = padLeftRight 1 $ str s <+> padLeft Max (str $ show n)

drawLeaderBoard :: Game -> Widget Name
drawLeaderBoard _ = emptyWidget

drawInfo :: Game -> Widget Name
drawInfo g = hLimit 18 -- size of next piece box
  $ vBox
    [ drawNextShape (g ^. nextShape)
    , padTop (Pad 1) drawHelp
    , padTop (Pad 1) (drawGameOver g)
    ]

drawNextShape :: Tetrimino -> Widget Name
drawNextShape t =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Next")
    $ padTopBottom 1
    $ padLeftRight 4
    $ vLimit 4
    $ vBox
    $ [0, -1] <&> \y ->
      hBox [ if V2 x y `elem` coords blk
             then drawBlockCell NormalBlock t
             else emptyNextShapeCellW
           | x <- [-2 .. 1]
           ]
  where blk = Block t (V2 0 0) (relCells t)

drawHelp :: Widget Name
drawHelp =
  withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Help")
    $ padTopBottom 1
    $ vBox
    $ map (uncurry drawKeyInfo)
      [ ("Left"   , "h, ←")
      , ("Right"  , "l, →")
      , ("Down"   , "j, ↓")
      , ("Rotate" , "k, ↑")
      , ("Drop"   , "space")
      , ("Restart", "r")
      , ("Pause"  , "p")
      , ("Quit"   , "q")
      ]

drawKeyInfo :: String -> String -> Widget Name
drawKeyInfo action keys =
  padRight Max (padLeft (Pad 1) $ str action)
    <+> padLeft Max (padRight (Pad 1) $ str keys)

drawGameOver :: Game -> Widget Name
drawGameOver g =
  if isGameOver g
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
iAttr = attrName "I"
oAttr = attrName "O"
tAttr = attrName "T"
sAttr = attrName "S"
zAttr = attrName "Z"
jAttr = attrName "J"
lAttr = attrName "L"

ihAttr, ohAttr, thAttr, shAttr, zhAttr, jhAttr, lhAttr :: AttrName
ihAttr = attrName "Ih"
ohAttr = attrName "Oh"
thAttr = attrName "Th"
shAttr = attrName "Sh"
zhAttr = attrName "Zh"
jhAttr = attrName "Jh"
lhAttr = attrName "Lh"

emptyAttr :: AttrName
emptyAttr = attrName "empty"

gameOverAttr :: AttrName
gameOverAttr = attrName "gameOver"
