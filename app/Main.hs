{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Control.Monad (when)
import System.Exit (exitSuccess)
import Text.Read (readMaybe)
import Data.Char (toLower)
import Data.Functor.Identity

import Options.Applicative
import qualified System.Directory as D
import System.FilePath ((</>))

import Tetris (Game(..))
import UI.PickLevel (configureLeveling, LevelConfig'(..))
import UI.Game (playGame)

data Opts = Opts
  { hardDrop :: HardDropOpt
  , level :: Maybe Int
  , progression :: Maybe Bool
  , score :: Bool
  }

data HardDropOpt = None | AsciiOnly | CustomChars String

opts :: Parser Opts
opts = Opts
  <$> hardDropOpt
  <*> optional (option auto
    (  long "level"
    <> short 'l'
    <> metavar "LEVEL"
    <> help "Specify level (unspecified results in prompt)" ))
  <*> optional (option toggle
    (  long "progression"
    <> metavar "BOOL"
    <> help "Turn level progression ON/OFF (unspecified results in prompt)" ))
  <*> switch
    (  long "high-score"
    <> help "Print high score and exit" )

toggle :: ReadM Bool
toggle = do
  s <- str
  case toLower <$> s of
    y | y `elem` ["y", "yes", "on", "t", "true"] -> return True
    n | n `elem` ["n", "no", "off", "f", "false"] -> return False
    _   -> readerError "Must be 'Y' or 'N'"

hardDropOpt :: Parser HardDropOpt
hardDropOpt = noneOpt <|> asciiOpt <|> custOpt
  where
    noneOpt = flag' None
      (  long "no-preview"
      <> short 'n'
      <> help "Don't show preview cell" )
    asciiOpt = flag' AsciiOnly
      (  long "ascii-only"
      <> short 'a'
      <> help "Use '[]' as hard drop preview cell" )
    custOpt = CustomChars <$> option twoChar
      (  long "preview-chars"
      <> short 'p'
      <> metavar "CHARS"
      <> value "◤◢"
      <> showDefaultWith (const "◤◢")
      <> help "Customize two character preview cell" )

fullopts :: ParserInfo Opts
fullopts = info (helper <*> opts)
  (  fullDesc
  <> header "tetris - the iconic game right in your terminal" )

twoChar :: ReadM String
twoChar = do
  cs <- str
  if length cs /= 2
     then readerError "Preview must be two characters long"
     else return cs

hdOptStr :: HardDropOpt -> Maybe String
hdOptStr None            = Nothing
hdOptStr AsciiOnly       = Just "[]"
hdOptStr (CustomChars s) = Just s

main :: IO ()
main = do
  (Opts {..}) <- execParser fullopts
  -- show high score and exit
  when score (getHighScore >>= printM >> exitSuccess)
  -- pick level prompt if necessary
  levelConfig <- configureLeveling level progression
  -- play game
  g <- playGame
    (runIdentity levelConfig.level)
    (runIdentity levelConfig.progression)
    (hdOptStr hardDrop)
  -- save & print score
  handleEndGame (_score g)

handleEndGame :: Int -> IO ()
handleEndGame s = do
  mhs <- getHighScore
  case mhs of
    Nothing -> newHighScore
    Just hs -> if s <= hs then justShowScore else newHighScore
  where
    justShowScore = putStrLn $ "Your final score: " ++ show s
    newHighScore = do
      putStrLn $ "Congrats! You just got the new highest score: " ++ show s
      setHighScore s

printM :: Show a => Maybe a -> IO ()
printM Nothing  = putStrLn "None"
printM (Just s) = print s

getHighScore :: IO (Maybe Int)
getHighScore = do
  lb <- getLeaderboardFile
  exists <- D.doesFileExist lb
  if exists
     then readMaybe <$> readFile lb
     else return Nothing

setHighScore :: Int -> IO ()
setHighScore s = do
  lb <- getLeaderboardFile
  writeFile lb (show s)

getLeaderboardFile :: IO FilePath
getLeaderboardFile = do
  xdg <- D.getXdgDirectory D.XdgData "tetris"
  D.createDirectoryIfMissing True xdg
  return (xdg </> "leaderboard")
