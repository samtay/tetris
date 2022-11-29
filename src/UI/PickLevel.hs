module UI.PickLevel
  ( pickLevel
  ) where

import System.Exit (exitSuccess)
import Control.Monad (when)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

app :: App (Maybe Int) e ()
app = App
  { appDraw         = const [ui]
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr []
  , appChooseCursor = neverShowCursor
  }

ui :: Widget ()
ui =
  padLeft (Pad 19)
    $ padRight (Pad 21)
    $ C.center
    $ vLimit 22
    $ hLimit 22
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Tetris")
    $ C.center
    $ str " Choose Level (0-9)"

handleEvent :: BrickEvent () e -> EventM () (Maybe Int) ()
handleEvent (VtyEvent (V.EvKey V.KEsc        _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar d) [])) =
  when (d `elem` ['0' .. '9']) $ do
    put $ Just $ read [d]
    halt
handleEvent _ = pure ()

pickLevel :: IO Int
pickLevel = defaultMain app Nothing >>= maybe exitSuccess return
