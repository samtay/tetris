module UI.PickLevel
  ( pickLevel
  , LevelConfig(..)
  ) where

import System.Exit (exitSuccess)
import Control.Monad (when)

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

data LevelConfig = LevelConfig
  { levelNumber :: Int
  , progression :: Bool
  } deriving (Show, Eq)

data MenuOption = YesOption | NoOption deriving (Eq)

data PickState = PickState
  { currentLevel :: Maybe Int
  , showProgression :: Bool
  , pickingLevel :: Bool
  , selectedOption :: MenuOption
  }

app :: App PickState e ()
app = App
  { appDraw         = drawUI
  , appHandleEvent  = handleEvent
  , appStartEvent   = pure ()
  , appAttrMap      = const $ attrMap V.defAttr
      [ (selectedAttr, V.black `on` V.white)
      ]
  , appChooseCursor = neverShowCursor
  }

selectedAttr :: AttrName
selectedAttr = attrName "selected"

drawUI :: PickState -> [Widget ()]
drawUI ps = [ui ps]

ui :: PickState -> Widget ()
ui ps =
  padLeft (Pad 19)
    $ padRight (Pad 21)
    $ C.center
    $ vLimit 22
    $ hLimit 22
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Tetris")
    $ C.center
    $ vBox
    [ if pickingLevel ps
        then str "Choose Level (0-9)"
        else vBox
          [ str "Level Progression?"
          , str ""
          , drawOption "YES" YesOption (selectedOption ps)
          , drawOption "NO" NoOption (selectedOption ps)
          , str ""
          , str "Use ↑↓ to select"
          , str "Press Enter to continue"
          ]
    ]

drawOption :: String -> MenuOption -> MenuOption -> Widget ()
drawOption label opt current =
  withAttr (if opt == current then selectedAttr else attrName "")
    $ str $ "  " ++ label ++ "  "

handleEvent :: BrickEvent () e -> EventM () PickState ()
handleEvent (VtyEvent (V.EvKey V.KEsc        _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar d) [])) =
  whenPickingLevel $ when (d `elem` ['0' .. '9']) $ do
    modify $ \s -> s { currentLevel = Just $ read [d], pickingLevel = False }
handleEvent (VtyEvent (V.EvKey V.KEnter [])) = do
  s <- get
  when (not $ pickingLevel s) $ do
    case currentLevel s of
      Just l -> do
        put $ PickState (Just l) (selectedOption s == YesOption) True YesOption
        halt
      Nothing -> pure ()
handleEvent (VtyEvent (V.EvKey V.KUp [])) =
  whenNotPickingLevel $ modify $ \s -> s { selectedOption = YesOption }
handleEvent (VtyEvent (V.EvKey V.KDown [])) =
  whenNotPickingLevel $ modify $ \s -> s { selectedOption = NoOption }
handleEvent _ = pure ()

whenPickingLevel :: EventM () PickState () -> EventM () PickState ()
whenPickingLevel action = do
  picking <- gets pickingLevel
  when picking action

whenNotPickingLevel :: EventM () PickState () -> EventM () PickState ()
whenNotPickingLevel action = do
  picking <- gets pickingLevel
  when (not picking) action

initialState :: PickState
initialState = PickState Nothing True True YesOption

pickLevel :: IO LevelConfig
pickLevel = do
  result <- defaultMain app initialState
  case currentLevel result of
    Nothing -> exitSuccess
    Just l -> return $ LevelConfig l (showProgression result)
