{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}

module UI.PickLevel
  ( configureLeveling
  , LevelConfig'(..)
  ) where

import System.Exit
import Control.Monad
import Control.Monad.Extra
import Data.Functor.Identity
import Data.Maybe

import Brick
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.Border.Style as BS
import qualified Brick.Widgets.Center as C
import qualified Graphics.Vty as V

type LevelConfig = LevelConfig' Identity
type LevelConfigMaybe = LevelConfig' Maybe

data LevelConfig' t = LevelConfig
  { level :: t Int
  , progression :: t Bool
  }

app :: App LevelConfigMaybe e ()
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

drawUI :: LevelConfigMaybe -> [Widget ()]
drawUI ps = [ui ps]

ui :: LevelConfigMaybe -> Widget ()
ui ps =
  padLeft (Pad 19)
    $ padRight (Pad 21)
    $ C.center
    $ vLimit 22
    $ hLimit 22
    $ withBorderStyle BS.unicodeBold
    $ B.borderWithLabel (str "Tetris")
    $ C.center
    $ if (isNothing ps.level)
        then str "Choose Level (0-9)"
        else vBox
        [ C.hCenter $ str "Level Progression?"
        , C.hCenter $ str "([Y]/N)"
        ]

handleEvent :: BrickEvent () e -> EventM () LevelConfigMaybe ()
handleEvent (VtyEvent (V.EvKey V.KEsc        _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'Q') _)) = halt
handleEvent (VtyEvent (V.EvKey (V.KChar 'Y') _)) = pickProgression True
handleEvent (VtyEvent (V.EvKey (V.KChar 'y') _)) = pickProgression True
handleEvent (VtyEvent (V.EvKey (V.KChar 'N') _)) = pickProgression False
handleEvent (VtyEvent (V.EvKey (V.KChar 'n') _)) = pickProgression False
handleEvent (VtyEvent (V.EvKey (V.KChar ' ') _)) = pickProgression True
handleEvent (VtyEvent (V.EvKey V.KEnter []))     = pickProgression True
handleEvent (VtyEvent (V.EvKey (V.KChar d) [])) =
  when (d `elem` ['0' .. '9']) $ pickLevel $ read [d]
handleEvent _ = pure ()

pickLevel :: Int -> EventM () LevelConfigMaybe ()
pickLevel n = do
  whenM pickingLevel $ do
    modify $ \s -> s { level = Just n }
    whenM (gets (isJust . progression)) $ do
      halt

pickProgression :: Bool -> EventM () LevelConfigMaybe ()
pickProgression b = do
  unlessM pickingLevel $ do
    modify $ \s -> s { progression = Just b }
    halt

pickingLevel :: EventM () LevelConfigMaybe Bool
pickingLevel = gets (isNothing . level)

configureLeveling :: Maybe Int -> Maybe Bool -> IO LevelConfig
configureLeveling (Just l) (Just p) = pure $ LevelConfig (Identity l) (Identity p)
configureLeveling ml mp = do
  result <- defaultMain app $ LevelConfig ml mp
  case result of
    (LevelConfig {level = Just l, progression = Just p}) ->
      pure $ LevelConfig (Identity l) (Identity p)
    _ ->
      exitSuccess
