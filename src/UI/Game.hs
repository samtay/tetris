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
data TVisual = Normal | HardDrop

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
    gmap = mconcat [brdMap, cBlkMap, hrdMap, emptyCellMap]
    brdMap = draw Normal . Just <$> g ^. board
    hrdMap = blkMap (hardDroppedBlock g) HardDrop
    cBlkMap = blkMap (g ^. block) Normal
    blkMap b v = M.fromList . map (, draw v . Just $ b ^. shape) $ coords b
    draw = drawMCell InGrid

emptyCellMap :: Map Coord (Widget Name)
emptyCellMap = M.fromList cws
  where
    cws = [((x,y), ew) | x <- [1..boardWidth], y <- [1..boardHeight]]
    ew = drawMCell InGrid Normal Nothing

drawMCell :: CellLocation -> TVisual -> Maybe Tetrimino -> Widget Name
drawMCell InGrid _ Nothing = withAttr emptyAttr cw
drawMCell InNextShape _ Nothing = withAttr emptyAttr ecw
drawMCell _ v (Just t) = drawCell t v

drawCell :: Tetrimino -> TVisual ->  Widget Name
drawCell t Normal = withAttr (tToAttr t) cw
drawCell t HardDrop = withAttr (tToAttrH t) hcw

tToAttr I = iAttr
tToAttr O = oAttr
tToAttr T = tAttr
tToAttr S = sAttr
tToAttr Z = zAttr
tToAttr J = jAttr
tToAttr L = lAttr

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
         , padTop (Pad 2) $ drawGameOver g
         ]

drawNextShape :: Tetrimino -> Widget Name
drawNextShape t = withBorderStyle BS.unicodeBold
  $ B.borderWithLabel (str "Next")
  $ padTopBottom 1 $ padLeftRight 3
  $ vLimit 4
  $ vBox $ mkRow <$> [0,-1]
  where
    mkRow y = hBox $ drawMCell InNextShape Normal . cellAt . (,y) <$> [-2..1]
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

drawGameOver :: Game -> Widget Name
drawGameOver g = if (isGameOver g)
                    then padLeftRight 3 $ withAttr gameOverAttr $ str "GAME OVER"
                    else emptyWidget

theMap :: AttrMap
theMap = attrMap V.defAttr
  [ (iAttr, tToColor I `on` tToColor I)
  , (oAttr, tToColor O `on` tToColor O)
  , (tAttr, tToColor T `on` tToColor T)
  , (sAttr, tToColor S `on` tToColor S)
  , (zAttr, tToColor Z `on` tToColor Z)
  , (jAttr, tToColor J `on` tToColor J)
  , (lAttr, tToColor L `on` tToColor L)
  -- attributes for hard drop preview (would be VERY clean if I could figure out how to
  -- query for default background color.. alas
  , (ihAttr, fg $ tToColor I)
  , (ohAttr, fg $ tToColor O)
  , (thAttr, fg $ tToColor T)
  , (shAttr, fg $ tToColor S)
  , (zhAttr, fg $ tToColor Z)
  , (jhAttr, fg $ tToColor J)
  , (lhAttr, fg $ tToColor L)
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
